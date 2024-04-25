-module(role_sup).
-file("role_sup.erl", 1).
-behaviour(supervisor).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

%% supervisor exports
-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-include("printout.hrl").
-include("child_spec.hrl").

%% supervisor -- start_link
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),
  {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
  {ok, PID}.

%% init
init(Params) ->
  Default = [ {child_options, #child_options{ restart = transient, 
                                              shutdown = 2000,
                                              type = worker }},
              {sup_flags, #sup_flags{ strategy = one_for_all,
                                      intensity = 1,
                                      period = 5 }} ],
  init(Default ++ Params, #{}).

init([], Params) -> init(finished, Params);

init([H|T], Params) ->
  case H of 
    {Key, Val} ->
      printout("~p, HKey: ~p.", [?FUNCTION_NAME, H]),
      %% other param
      Params1 = maps:put(Key, Val, Params);
    _ -> 
      %% no change
      Params1 = Params
  end,
  init(T, Params1);

init(finished, Params) ->
  printout("~p, finished.", [?FUNCTION_NAME]),

  %% get roles 
  Role = maps:get(role, Params, err_no_role),
  printout("~p, role: ~p.", [?FUNCTION_NAME, Role]),
  ?assert(not Role==err_no_role, "Error in params: no role provided!"),

  Name = Role#role_spec.name,
  Mon = Role#role_spec.modules#role_modules.mon,
  Imp = Role#role_spec.modules#role_modules.imp,
  RoleParams = Role#role_spec.params,
  printout("~p, role: ~p.", [?FUNCTION_NAME, Name]),

  RoleParams1 = RoleParams ++ [{name, Name}],

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {tpri, Name, sup, self()},

  ChildOptions = maps:get(child_options, Params),
  SupFlags = maps:get(sup_flags, Params),

  %% if using "master-template" version (event handler), 
  %% then ensure necessary params
  if (Mon==role_tmpl) -> 
    Keys = maps:keys(maps:from_list(Params)),
    ?assert(lists:member(init_state, Keys), "Error in params: no init_state!"),
    ?assert(lists:member(timeouts, Keys), "Error in params: no timeouts!"),
    ?assert(lists:member(state_map, Keys), "Error in params: no state_map!")
  end,

  %% create spec for fsm and imp
  MonRegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Mon)),
  case Mon of
    %% module/file of fsm child is same as regID
    role_fsm -> MonSpec = child_spec(MonRegID,MonRegID,ChildOptions,RoleParams1);
    role_templ -> MonSpec = child_spec(Mon,MonRegID,ChildOptions,RoleParams1);
    _ -> MonSpec = undefined, error(unexpected_role_spec_fsm, [Mon])
  end,
  ?assert(not MonSpec==undefined, io_lib:format("Error in role_spec, mon (~p) unexpected.",[Mon])),

  ImpRegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Imp)),
  %% module/file of imp child is same as regID
  ImpSpec = child_spec(ImpRegID,ImpRegID,ChildOptions,[{name, Name}]),

  ChildSpecs = [MonSpec,ImpSpec],

  {ok, {SupFlags, ChildSpecs}}.




