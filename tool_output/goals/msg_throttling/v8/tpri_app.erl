-module(tpri_app).
-file("tpri_app.erl", 1).
-behaviour(application).

-export([start/2, stop/1]).

-export([ start/1,
          get_env/1
        ]).

-export([start/0]).
-export([run/0]).

-define(WORKER_NUM, 2).

-include("printout.hrl").
-include("ets_get.hrl").

get_env(Key) -> 
  case application:get_env(?MODULE, Key) of
    {ok, Val} -> {ok, Val};
    undefined -> undefined
  end.

start() -> start([]).

start(Args) -> start(normal, Args).

start(Type, Args) -> 
  printout("~p, type=~p.", [?FUNCTION_NAME, Type]),

  ets:new(tpri, [set,named_table,protected]),
  ets:insert(tpri, {app_id, self()}),
  printout("~p, created ets, added self()=~p.", [?FUNCTION_NAME, self()]),

  {ok, ServerID} = tpri_server:start_link(Args),
  printout("~p, server started: ~p.\n", [?FUNCTION_NAME,ServerID]),

  printout("~p, beginning ets setup.", [?FUNCTION_NAME]),
  ets_setup([]),
  printout("~p, finished ets setup.\n", [?FUNCTION_NAME]),

  printout("~p, ets:\n\t~p.\n", [?FUNCTION_NAME, ets_string(tpri)]),

  {ok, ServerID}.

stop(_State) -> ok.

run() ->
  %% get all roles
  Roles = ets_get(tpri, roles),
  printout("~p, roles: ~p.", [?FUNCTION_NAME, Roles]),

  %% assuming only two roles
  A = lists:nth(1, Roles),
  B = lists:nth(2, Roles),

  RoleA = ets_get(tpri, A),
  RoleB = ets_get(tpri, B),

  printout("~p, role ~p: ~p.", [?FUNCTION_NAME, A, RoleA]),
  printout("~p, role ~p: ~p.", [?FUNCTION_NAME, B, RoleB]),

  ASup = maps:get(sup, RoleA),
  AMon = maps:get(mon, RoleA),
  AImp = maps:get(imp, RoleA),

  BSup = maps:get(sup, RoleB),
  BMon = maps:get(mon, RoleB),
  BImp = maps:get(imp, RoleB),

  %% exchange monitor ids
  AMon ! {ASup, sup_init, BMon},
  BMon ! {BSup, sup_init, AMon},

  %% give imps their monitor
  AImp ! {setup_coparty, AMon},
  BImp ! {setup_coparty, BMon},

  %% do any last minute things here

  %% begin!
  AImp ! {setup_finished, start},
  BImp ! {setup_finished, start},
  % AMon ! {setup_finished, start},
  % BMon ! {setup_finished, start},

  ok.


%% @doc continually wait to receive message from children, adding them to ets
%% then check if all are accounted for before returning
ets_setup(Roles) ->
  % printout("~p, waiting.", [?FUNCTION_NAME]),
  receive
    {tpri, server_id, ServerID} = Msg -> 
      printout("~p, server_id: ~p.", [?FUNCTION_NAME,ServerID]),
      ets:insert(tpri, {tpri_server_id, ServerID}),
      Roles1 = Roles;
    {tpri, sup_id, SupID} = Msg -> 
      printout("~p, tpri sup_id: ~p.", [?FUNCTION_NAME,SupID]),
      ets:insert(tpri, {tpri_sup_id, SupID}),
      Roles1 = Roles;
    {role, Name, Kind, SupID} = Msg ->
      printout("~p, role ~p: ~p.", [?FUNCTION_NAME,Name,Kind]),
      %% check if role already exists
      case ets:member(tpri, Name) of
        true -> 
          RoleKinds = ets:lookup(tpri, Name),
          {_, RoleKinds1} = lists:nth(1, RoleKinds),
          RoleKinds2 = maps:put(Kind, SupID, RoleKinds1),
          ets:insert(tpri, {Name, RoleKinds2}),
          Roles1 = Roles;
        false -> 
          ets:insert(tpri, {Name, #{Kind => SupID}}),
          Roles1 = Roles ++ [Name]
      end,
      %% if sup, send back confirmation and register id to name
      if Kind==sup -> 
        ets:insert(tpri, {SupID, Name});
        % SupID ! {self(), name_registered} 
        true -> ok
      end;
    Msg -> 
      printout("~p, unexpected msg: ~p.", [?FUNCTION_NAME,Msg]),
      Roles1 = Roles
  end,
  % printout("~p, received: ~p.", [?FUNCTION_NAME, Msg]),
  case check_ets_finished(Roles1) of
    false -> ets_setup(Roles1);
    _ -> 
      ets:insert(tpri, {roles, Roles1}),
      ok
  end.


check_ets_finished(Roles) ->
  Keys = [ tpri_server_id,
           tpri_sup_id ],
  if (length(Roles)==?WORKER_NUM) -> 
      CheckRoles = fun(Name, Acc) -> 
        case ets:member(tpri, Name) of
          true -> 
            {_, RoleKinds} = lists:nth(1, ets:lookup(tpri, Name)),
            RoleKinds1 = maps:keys(RoleKinds),
            %% valid if members: sup mon imp
            HasSup = lists:member(sup, RoleKinds1),
            HasMon = lists:member(mon, RoleKinds1),
            HasImp = lists:member(imp, RoleKinds1),
            Result = HasSup and HasMon and HasImp,
            Acc ++ [Result];
          false -> 
            % printout("~p, check_roles, ~p  => false.", [?FUNCTION_NAME, Name]),
            Acc ++ [false]
        end
      end,
      CheckKeys = fun(Key, Acc) -> Acc ++ [ets:member(tpri, Key)] end,
      %% check roles and keys
      Results = lists:foldl(CheckRoles, [], Roles) ++ lists:foldl(CheckKeys, [], Keys),
      lists:foldl(fun(B, Acc) -> B and Acc end, true, Results);
    %% not finished if not enough roles
    true -> 
      % printout("~p, incorrect role num: ~p =/= ~p.", [?FUNCTION_NAME, length(Roles), ?WORKER_NUM]),
      false
  end.



