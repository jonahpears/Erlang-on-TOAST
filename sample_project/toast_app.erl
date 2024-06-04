-module(toast_app).
-file("toast_app.erl", 1).
-behaviour(application).

-include("toast_data_records.hrl").

-export([start/2, stop/1]).

-export([ start/1,
          get_env/1
        ]).

-export([start/0]).
-export([run/0]).
-export([run_startup/3]).

-define(WORKER_NUM, 2).

-include_lib("stdlib/include/assert.hrl").

-include("printout.hrl").
-include("ets_get.hrl").
-include("sup_flags.hrl").
-include("toast_app_params.hrl").

get_env(Key) -> 
  case application:get_env(?MODULE, Key) of
    {ok, Val} -> {ok, Val};
    undefined -> undefined
  end.

start() -> error(no_params).
  % printout("~p, no parameters provided, using default.", [?FUNCTION_NAME]),
  % start(normal, [{use_preset, default}]).

start(Args) when is_list(Args) -> start(normal, Args).

start(Type, Args) when is_list(Args) -> 
  printout("~p, type=~p.", [?FUNCTION_NAME, Type]),
  printout("~p, args:\n\t~p.", [?FUNCTION_NAME, Args]),

  %% create ets for tpri app
  ets:new(toast, [set,named_table,protected]),
  ets:insert(toast, {session_id, self()}),
  printout("~p, created ets, added self()=~p.", [?FUNCTION_NAME, self()]),

  timer:sleep(50),

  % {ok, ServerID} = toast_server:start_link(Args),
  {ok, ServerID} = toast_sup:start_link(Args),

  timer:sleep(500),
  printout("~p, server started: ~p.\n", [?FUNCTION_NAME,ServerID]),


  %% get list of role names
  Roles = lists:foldl(fun(Elem,AccIn) -> 
      case Elem of
        {role, #{module:=_ModuleName,name:=_RoleName}} -> AccIn++[Elem];
        _ -> AccIn
      end
  end, [], Args),
  printout("~p, roles:\n\t~p.", [?FUNCTION_NAME, Roles]),

  % WorkerNum=length(Roles),
  % ets:insert(toast, {roles, Roles}),


  printout("~p, beginning ets setup.", [?FUNCTION_NAME]),
  Rs = ets_setup(length(Roles),[]),
  ets:insert(toast, {roles, Rs}),

  % ets_setup([], WorkerNum, RolePresets),
  % printout("~p, finished ets setup.\n", [?FUNCTION_NAME]),


  printout("~p, ets:\n\t~p.\n", [?FUNCTION_NAME, ets_string(toast)]),

  {ok, ServerID}.

stop(_State) -> ok.

ets_setup(0, Rs) -> printout("~p, finished, roles:\n\t~p.",[?FUNCTION_NAME,Rs]),Rs;

ets_setup(Num, Rs) when Num>0 -> 
  receive 
    {RoleID, role, #{module:=_RoleModule,name:=_RoleName}=R} ->
      printout("~p, received: ~p.",[?FUNCTION_NAME,R]),
      ets:insert(toast, {RoleID, R}),
      ets_setup(Num-1, Rs++[RoleID])
    % _Else -> printout("~p, else:\n\t~p.",[?FUNCTION_NAME,_Else]),
    %   ets_setup(Num,Rs)
  end.

run() ->
  SessionID = ets_get(toast, session_id),

  %% get all roles
  Roles = ets_get(toast, roles),
  printout("~p, roles: ~p.", [?FUNCTION_NAME, Roles]),

  %% assuming only two roles
  A = lists:nth(1, Roles),
  B = lists:nth(2, Roles),

  A ! {SessionID,coparty_id,B},
  B ! {SessionID,coparty_id,A},

  receive 
    {A, ready} -> receive {B, ready} -> ok end;
    {B, ready} -> receive {A, ready} -> ok end
  end,
  printout("~p, both ready.",[?FUNCTION_NAME]),

  A ! {SessionID, start},
  B ! {SessionID, start},


  % %% check for any special modes/preset
  % AppParams = ets_get(toast, app_params),
  % ActivePreset = maps:get(active_preset, AppParams, default),
  % printout("~p, active preset: ~p.", [?FUNCTION_NAME, ActivePreset]),
  % case ActivePreset of
  %   default -> run_startup(ActivePreset, {A, B}, #{});
  %   _ -> run_startup(ActivePreset, {A, B}, maps:get(ActivePreset, AppParams, #{}))
  % end,
  ok.



run_startup(asym_direct_imp=Preset, {A, B}, #{enabled:=true,role:=AsymRole}) ->
  %% ensure asym role is one of the 
  ?assert(lists:member(AsymRole,[A,B]), "Asymmetric role must be one of the specified roles."),

  RoleA = ets_get(tpri, A),
  RoleB = ets_get(tpri, B),

  printout("~p, role ~p: ~p.", [?FUNCTION_NAME, A, RoleA]),
  printout("~p, role ~p: ~p.", [?FUNCTION_NAME, B, RoleB]),


  HelperMap = #{ asym_role_imp => undefined, 
                 mon_role_sup => undefined,
                 mon_role_mon => undefined,
                 mon_role_imp => undefined },

  RoleMap = lists:foldl(fun(Elem, Acc) -> 
      case Elem=:=AsymRole of 
        true ->
          Role = ets_get(tpri, Elem),
          Imp = maps:get(imp, Role),
          printout("~p, ~p...\n\tasymm. role ~p: ~p.", [?FUNCTION_NAME, Preset, Elem, Role]),
          Acc1 = maps:put(asym_role_imp, Imp, Acc);
          _ -> %% start normally, 
          Role = ets_get(tpri, Elem),
          Sup = maps:get(sup, Role),
          Mon = maps:get(mon, Role),
          Imp = maps:get(imp, Role),
          printout("~p, ~p...\n\tdefault role ~p: ~p.", [?FUNCTION_NAME, Preset, Elem, Role]),
          Acc1 = maps:merge(Acc, #{mon_role_sup => Sup, mon_role_mon => Mon, mon_role_imp => Imp})
        end,
        Acc1
  end, HelperMap, [A,B]),

  AsymRoleImp = maps:get(asym_role_imp, RoleMap),
  MonRoleSup = maps:get(mon_role_sup, RoleMap),
  MonRoleMon = maps:get(mon_role_mon, RoleMap),
  MonRoleImp = maps:get(mon_role_imp, RoleMap),

  %% for monitored role
  MonRoleMon ! {MonRoleSup, sup_init, AsymRoleImp},
  MonRoleImp ! {setup_coparty, MonRoleMon},

  %% for direct imp role
  AsymRoleImp ! {setup_coparty, MonRoleMon},

  %% do any last minute things here

  %% begin!
  MonRoleImp ! {setup_finished, start},
  AsymRoleImp ! {setup_finished, start},
  ok;


run_startup(default=Preset, {A, B}, _) -> 
  %% get map of each roles (sup,mon,imp) ids
  RoleA = ets_get(tpri, A),
  ASup = maps:get(sup, RoleA),
  AMon = maps:get(mon, RoleA),
  AImp = maps:get(imp, RoleA),

  RoleB = ets_get(tpri, B),
  BSup = maps:get(sup, RoleB),
  BMon = maps:get(mon, RoleB),
  BImp = maps:get(imp, RoleB),

  printout("~p, ~p\n\trole ~p: ~p.", [?FUNCTION_NAME, Preset, A, RoleA]),
  printout("~p, ~p\n\trole ~p: ~p.", [?FUNCTION_NAME, Preset, B, RoleB]),

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

  ok.



% %% @doc continually wait to receive message from children, adding them to ets
% %% then check if all are accounted for before returning
% ets_setup(Roles, WorkerNum, RolePresets) ->
%   % printout("~p, waiting.", [?FUNCTION_NAME]),
%   receive
%     {tpri, server_id, ServerID} = _Msg -> 
%       printout("~p, server_id: ~p.", [?FUNCTION_NAME,ServerID]),
%       ets:insert(tpri, {tpri_server_id, ServerID}),
%       Roles1 = Roles;
%     {tpri, sup_id, SupID} = _Msg -> 
%       printout("~p, tpri sup_id: ~p.", [?FUNCTION_NAME,SupID]),
%       ets:insert(tpri, {tpri_sup_id, SupID}),
%       Roles1 = Roles;
%     {role, Name, Kind, SupID} = _Msg ->
%       printout("~p, role ~p: ~p.", [?FUNCTION_NAME,Name,Kind]),
%       %% check if role already exists
%       case ets:member(tpri, Name) of
%         true -> 
%           RoleKinds = ets:lookup(tpri, Name),
%           {_, RoleKinds1} = lists:nth(1, RoleKinds),
%           RoleKinds2 = maps:put(Kind, SupID, RoleKinds1),
%           ets:insert(tpri, {Name, RoleKinds2}),
%           Roles1 = Roles;
%         false -> 
%           ets:insert(tpri, {Name, #{Kind => SupID}}),
%           Roles1 = Roles ++ [Name]
%       end,
%       %% if sup, send back confirmation and register id to name
%       if Kind==sup -> 
%         ets:insert(tpri, {SupID, Name});
%         % SupID ! {self(), name_registered} 
%         true -> ok
%       end;
%     Msg -> 
%       printout("~p, unexpected msg: ~p.", [?FUNCTION_NAME,Msg]),
%       Roles1 = Roles
%   end,
%   % printout("~p, received: ~p.", [?FUNCTION_NAME, Msg]),
%   case check_ets_finished(Roles1, WorkerNum, RolePresets) of
%     false -> ets_setup(Roles1, WorkerNum, RolePresets);
%     _ -> 
%       ets:insert(tpri, {roles, Roles1}),
%       ok
%   end.


% check_ets_finished(Roles, WorkerNum, RolePresets) ->
%   Keys = [ tpri_server_id,
%            tpri_sup_id ],
%   if (length(Roles)==WorkerNum) -> 
%       CheckRoles = fun(Name, Acc) -> 
%         case ets:member(tpri, Name) of
%           true -> 
%             {_, RoleKinds} = lists:nth(1, ets:lookup(tpri, Name)),
%             RoleKindsKeys = maps:keys(RoleKinds),
%             RolePreset = maps:get(Name, RolePresets),
%             %% valid if members: sup mon imp
%             Result = lists:foldl(fun(Elem, Acc1) -> Acc1 and lists:member(Elem, RoleKindsKeys) end, true, RolePreset),
%             Acc and Result;
%           false -> 
%             % printout("~p, check_roles, ~p  => false.", [?FUNCTION_NAME, Name]),
%             Acc and false
%         end
%       end,
%       CheckKeys = fun(Key, Acc) -> Acc and ets:member(tpri, Key) end,
%       %% check roles and keys
%       lists:foldl(CheckRoles, true, Roles) and lists:foldl(CheckKeys, true, Keys);
%       % lists:foldl(fun(B, Acc) -> B and Acc end, true, Results);
%     %% not finished if not enough roles
%     true -> 
%       % printout("~p, incorrect role num: ~p =/= ~p.", [?FUNCTION_NAME, length(Roles), ?WORKER_NUM]),
%       false
%   end.



