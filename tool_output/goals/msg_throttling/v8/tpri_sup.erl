-module(tpri_sup).
-file("tpri_sup.erl", 1).
-behaviour(supervisor).

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
              {role, #role_spec{ name = ali, modules = #role_modules{}, params = [] }},
              {role, #role_spec{ name = bob, modules = #role_modules{}, params = [] }}],
  ?MODULE:init(Default ++ Params, #{ roles => #{} }).

init([], Params) -> ?MODULE:init(finished, Params);

init([H|T], Params) -> 
  case H of 
    {role, RoleSpec} ->
      %% add new role 
      Params1 = maps:update_with(roles, fun(V) -> V ++ [RoleSpec] end, Params, [RoleSpec]);
    {Key, Val} ->
      %% other param
      Params1 = maps:put(Key, Val, Params);
    _ -> 
      %% no change
      Params1 = Params
  end,
  ?MODULE:init(T, Params1);

%% finished parsing both params
init(finished, Params) ->
  printout("~p.", [?FUNCTION_NAME]),

  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {tpri, sup_id, self()},

  ChildOptions = maps:get(child_options, Params),
  SupFlags = maps:get(sup_flags, Params),

  RoleChildren = maps:get(roles, Params),
  printout("~p, num_roles: ~p.", [?FUNCTION_NAME, length(RoleChildren)]),

  %% create supervisor for each role
  SpecFun = fun(#role_spec{name=Name, modules=#role_modules{sup=Sup,fsm=_FSM,imp=_Imp}, params=RoleParams}, Acc) -> 
    RegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Sup)),
    Acc ++ [child_spec(Sup,RegID,ChildOptions,RoleParams)] 
  end,

  ChildSpecs = lists:foldl(SpecFun, [], RoleChildren),

  {ok, {SupFlags, ChildSpecs}}.


child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type }, ChildParams) ->
  #{ id => ID,
     start => {Module, start_link, [[{name,ID}] ++ ChildParams]},
     restart => Restart,
     shutdown => Shutdown,
     type => Type,
     modules => [Module] }.


% run_setup() ->
%   receive {trm, TrmID} -> ok end,
%   receive {imp, ImpID} -> ok end,

%   printout("~p, complete,\n\ttrm: ~p\n\timp: ~p.", [?FUNCTION_NAME, TrmID, ImpID]).


