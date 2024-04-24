-module(role_sup).
-file("role_sup.erl", 1).
-behaviour(supervisor).

-include_lib("stdlib/include/assert.hrl").
-include("tpri_data_records.hrl").

%% supervisor exports
-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

%% supervisor -- start_link
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
  {ok, PID}.

%% init
init(Params) ->
  Default = [ {child_options, #child_options{ restart = transient, 
                                              shutdown = 2000,
                                              type = worker }},
              {sup_flags, #sup_flags{ strategy = one_for_all,
                                      intensity = 1,
                                      period = 5 }},
              {role, #role_spec{ name = tes, modules = #role_modules{}, params = [] }}],
  ?MODULE:init(Default ++ Params, #{}).

init([], Params) -> ?MODULE:init(finished, Params);

init([H|T], Params) ->
  case H of 
    {Key, Val} ->
      %% other param
      Params1 = maps:put(Key, Val, Params);
    _ -> 
      %% no change
      Params1 = Params
  end,
  ?MODULE:init(T, Params1);

init(finished, Params) ->
  printout("~p.", [?FUNCTION_NAME]),

  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {tpri, sup_id, self()},

  ChildOptions = maps:get(child_options, TpriParams),
  SupFlags = maps:get(sup_flags, TpriParams),

  Role = maps:get(role, Params, err_no_role),
  ?assert(not Role==err_no_role),

  Name = Role#role_spec.name,
  Fsm = Role#role_spec.modules#role_modules.fsm,
  Imp = Role#role_spec.modules#role_modules.imp,
  RoleParams = Role#role_spec.params,
  printout("~p, role: ~p.", [?FUNCTION_NAME, Name]),

  %% create spec for fsm and imp
  FsmRegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Fsm)),
  FsmSpec = child_spec(Fsm,FsmRegID,ChildOptions,RoleParams),

  ImpRegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Imp)),
  ImpSpec = child_spec(Imp,ImpRegID,ChildOptions,RoleParams),

  ChildSpec = [FsmSpec,ImpSpec],

  {ok, {SupFlags, ChildSpecs}}.




