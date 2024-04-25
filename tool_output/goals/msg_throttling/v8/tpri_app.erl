-module(tpri_app).
-file("tpri_app.erl", 1).
-behaviour(application).

-export([start/2, stop/1]).

-export([ start/1,
          get_env/1
        ]).

-export([start/0]).

-define(WORKER_NUM, 2).

-include("printout.hrl").

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
  printout("~p, server started: ~p.", [?FUNCTION_NAME,ServerID]),

  printout("~p, beginning ets setup.", [?FUNCTION_NAME]),
  finish_ets_setup([]),
  printout("~p, finished ets setup.", [?FUNCTION_NAME]),

  {ok, ServerID}.

stop(_State) -> ok.


%% @doc continually wait to receive message from children, adding them to ets
%% then check if all are accounted for before returning
finish_ets_setup(Roles) ->
  printout("~p, waiting.", [?FUNCTION_NAME]),
  receive
    {tpri, server_id, ServerID} = Msg -> 
      ets:insert(tpri, {tpri_server_id, ServerID}),
      Roles1 = Roles;
    {tpri, sup_id, SupID} = Msg -> 
      ets:insert(tpri, {tpri_sup_id, SupID}),
      Roles1 = Roles;
    {role, Name, Kind, SupID} = Msg ->
      %% check if role already exists
      case ets:member(tpri, Name) of
        true -> 
          Role = ets:lookup(tpri, Name),
          ets:insert(tpri, {Name, Role ++ [{Kind, SupID}]});
        false -> ets:insert(tpri, {Name, [{Kind, SupID}]})
      end,
      Roles1 = Roles ++ [Name];
    Msg -> 
      printout("~p, unexpected msg: ~p.", [?FUNCTION_NAME,Msg]),
      Roles1 = Roles
  end,
  printout("~p, received: ~p.", [?FUNCTION_NAME, Msg]),
  case check_ets_finished(Roles1) of
    false -> finish_ets_setup(Roles1);
    _ -> ok
  end.


check_ets_finished(Roles) ->
  Keys = [ tpri_server_id,
           tpri_sup_id,
           role_sup_id ],
  if (length(Roles)==?WORKER_NUM) -> 
      CheckRoles = fun(Name, Acc) -> 
        case ets:member(tpri, Name) of
          true -> 
            Role = maps:keys(maps:from_list(ets:lookup(tpri, Name))),
            %% valid if members: sup mon imp
            Acc ++ [lists:member(sup, Role) and lists:member(mon, Role) and lists:member(imp, Role)];
          false -> Acc ++ [false]
        end
      end,
      CheckKeys = fun(Key, Acc) -> Acc ++ [ets:member(tpri, Key)] end,
      %% check roles and keys
      Results = lists:foldl(CheckRoles, [], Roles) ++ lists:foldl(CheckKeys, [], Keys),
      lists:foldl(fun(B, Acc) -> B and Acc end, true, Results);
    %% not finished if not enough roles
    true -> false
  end.



