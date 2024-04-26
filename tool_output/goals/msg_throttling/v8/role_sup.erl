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
-include("sup_flags.hrl").

%% supervisor -- start_link
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
    printout("~p.", [?FUNCTION_NAME]),
    Params1 = maps:from_list(Params),
    RoleID = maps:get(id,Params1,err_no_role_id),
    printout(RoleID, "~p, role/id: ~p.", [?FUNCTION_NAME, RoleID]),
    case RoleID of
      err_no_role_id -> 
        printout(RoleID, "~p, ~p, registering locally.", [?FUNCTION_NAME, RoleID]),
        {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params);
      _ ->
        printout(RoleID, "~p, registering globally as: ~p.", [?FUNCTION_NAME, RoleID]),
        {ok, PID} = supervisor:start_link({global, RoleID}, ?MODULE, Params)
    end,
    {ok, PID}.

%% init
init(Params) ->
  Default = [ {child_options, #child_options{ restart = temporary, % transient 
                                              shutdown = 2000,
                                              type = worker }},
              {sup_flags, sup_flags(one_for_all, 1, 5 )} ],
  init(Default ++ Params, #{}).

init([], Params) -> init(finished, Params);

init([H|T], Params) ->
  case H of 
    {Key, Val} ->
      % printout("~p, HKey: ~p.", [?FUNCTION_NAME, H]),
      %% other param
      Params1 = maps:put(Key, Val, Params);
    _ -> 
      %% no change
      Params1 = Params
  end,
  init(T, Params1);

init(finished, Params) ->
  printout("~p, finished.", [?FUNCTION_NAME]),
  % printout("~p, finished, params: ~p.", [?FUNCTION_NAME, Params]),

  %% get roles 
  RoleError = maps:take(id, Params),
  ?assert(RoleError=/=error, "Error in params: no role/id provided!"),
  {RoleID,Params1} = RoleError,
  ?assert(is_atom(RoleID), io_lib:format("Error in params: role/id is not atom: ~p.", [RoleID])),
  printout(RoleID, "~p, role/id: ~p.", [?FUNCTION_NAME, RoleID]),

  Name = maps:get(name, Params1),
  Mon = maps:get(mon, Params1),
  Imp = maps:get(imp, Params1),
  printout(RoleID, "~p, role name: ~p.", [?FUNCTION_NAME, Name]),

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {role, Name, sup, self()},
  %% get confirmation 
  % receive {AppID, name_registered} -> ok end,
  % printout(name(), "~p, registered with app successfully.", [?FUNCTION_NAME]),

  ChildOptions = maps:get(child_options, Params1),
  SupFlags = maps:get(sup_flags, Params1),

  %% if using "master-template" version (event handler), 
  %% then ensure necessary params
  if (Mon==role_tmp) -> 
    Keys = maps:keys(Params1),
    ?assert(lists:member(init_state, Keys), "Error in params: no init_state!"),
    ?assert(lists:member(timeouts, Keys), "Error in params: no timeouts!"),
    ?assert(lists:member(state_map, Keys), "Error in params: no state_map!")
  end,

  %% update name to be in non-file format (readability)
  MonName = list_to_atom(atom_to_list(Name) ++ "_role_mon"),
  % MonName = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Mon)),
  MonParams = maps:put(role, Name, Params1),
  MonParams1 = maps:put(name, MonName, MonParams),
  MonParams2 = maps:to_list(MonParams1),

  %% create spec for fsm and imp
  MonRegID = list_to_atom(atom_to_list(Mon) ++ "_" ++ atom_to_list(Name)),
  case Mon of
    %% module/file of fsm child is same as regID
    role_fsm -> MonSpec = child_spec(MonRegID,MonRegID,ChildOptions,MonParams2);
    role_tmp -> MonSpec = child_spec(Mon,MonRegID,ChildOptions,MonParams2);
    _ -> MonSpec = undefined, error(unexpected_role_spec_fsm, [Mon])
  end,
  ?assert(MonSpec=/=undefined, io_lib:format("Error in role_spec, mon (~p) unexpected.",[Mon])),

  ImpName = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Imp)),
  ImpRegID = list_to_atom(atom_to_list(Imp) ++ "_" ++ atom_to_list(Name)),
  %% module/file of imp child is same as regID
  ImpSpec = child_spec(ImpRegID,ImpRegID,ChildOptions,[{name, ImpName},{role,Name}]),

  % printout(RoleID, "~p, monspec: ~p.", [?FUNCTION_NAME,MonSpec]),
  % printout(RoleID, "~p, impspec: ~p.", [?FUNCTION_NAME,ImpSpec]),

  ChildSpecs = [MonSpec,ImpSpec],

  printout(RoleID, "leaving ~p.", [?FUNCTION_NAME]),
  {ok, {SupFlags, ChildSpecs}}.

name() -> lists:first(ets:lookup(tpri,self())).


