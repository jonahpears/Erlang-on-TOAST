-module(toast_sup).
-file("toast_sup.erl", 1).
-behaviour(supervisor).

-include("toast_data_records.hrl").

%% supervisor exports
-export([ start_link/0,
          init/1 ]).
        
-export([ start_link/1 ]).

-include("toast_app_params.hrl").
-include("sup_flags.hrl").
-include("child_spec.hrl").
-include("ets_get.hrl").

-include("printout.hrl").

%% supervisor -- start_link
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),

  {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
  {ok, PID}.


%% init
init(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),
  init(Params, #{ roles => [], child_options => default_child_options(), sup_flags => default_sup_flags() }).

init([], Params) -> init(finished, Params);

init([H|T], Params) -> 
  case H of 
    {role, RoleModuleName} ->
      %% add new role 
      Params1 = maps:update_with(roles, fun(V) -> V ++ [RoleModuleName] end, [RoleModuleName], Params);
    {Key, Val} ->
      %% other param
      Params1 = maps:put(Key, Val, Params);
    _ -> 
      %% no change
      Params1 = Params
  end,
  init(T, Params1);

%% finished parsing both params
init(finished, Params) ->
  % printout("~p, finished.", [?FUNCTION_NAME]),
  printout("~p, finished,\n\tparams: ~p.", [?FUNCTION_NAME, Params]),

  ChildOptions = maps:get(child_options, Params),
  SupFlags = maps:get(sup_flags, Params),
  SupFlags1 = maps:put(auto_shutdown, all_significant, SupFlags),

  Roles = maps:get(roles, Params),
  printout("~p, num_roles: ~p.", [?FUNCTION_NAME, length(Roles)]),

  SessionID = ets_get(toast,session_id),
  printout("~p, session_id: ~p.",[?FUNCTION_NAME, SessionID]),

  %% create supervisor for each role
  SpecFun = fun(#{module:=ModuleName,name:=RoleName}=Role, AccIn) -> 
      RegID = RoleName,
      AccIn ++ [child_spec(ModuleName,RegID,ChildOptions,[{session_id,SessionID},{role,Role}])]
      % %% check if this child should actually be a direct imp 
      % % Map = maps:from_list(RoleParams),
      % % Keys = maps:keys(Map),
      % case lists:member(direct_imp, Keys) and maps:get(direct_imp, Map, false) of
      %   true -> %% this role is a direct_imp 
      %     %% child should just be the imp file/process
      %     ImpName = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(imp)),
      %     ImpRegID = list_to_atom(atom_to_list(Imp) ++ "_" ++ atom_to_list(Name)),
      %     ChildOptions1 = #child_options{ restart = ChildOptions#child_options.restart,
      %                                     shutdown = ChildOptions#child_options.shutdown,
      %                                     type = ChildOptions#child_options.type,
      %                                     significant = true },
      %     ImpSpec = child_spec(ImpRegID,ImpRegID,ChildOptions1,[{name, ImpName},{role,Name}]),
      %     Acc ++ [ImpSpec];
      %   _ -> %% proceed as usual -- both sub-sup trees (with mon/imp)
      %     RoleParams1 = RoleParams ++ [{name,Name},{mon,Mon},{imp,Imp}],
      %     printout("~p, sup ~p.", [?FUNCTION_NAME, Name]),
      %     Acc ++ [child_spec(Sup,RegID,ChildOptions,RoleParams1)] 
      % end
  end,

  ChildSpecs = lists:foldl(SpecFun, [], Roles),

  {ok, {SupFlags1, ChildSpecs}}.
