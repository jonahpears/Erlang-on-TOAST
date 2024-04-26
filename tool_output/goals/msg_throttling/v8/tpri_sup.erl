-module(tpri_sup).
-file("tpri_sup.erl", 1).
-behaviour(supervisor).

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
  {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
  {ok, PID}.


%% init
init(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),
  AliTempl = [{ init_state, state1_send_msg1},
              { timeouts, #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
                             state3a_recv_ack2 => {5000, issue_timeout} }},
              { state_map, #{ state1_send_msg1  => #{send => #{msg => state2a_recv_ack1} },
                              state2a_recv_ack1 => #{recv => #{ack => state1_send_msg1}  },
                              state2b_send_msg2 => #{send => #{msg => state3a_recv_ack2} },
                              state3a_recv_ack2 => #{recv => #{ack => state2a_recv_ack1} }} }],
  BobTempl = [{ init_state, state1_recv_msg1},
              { timeouts, #{ state2a_send_ack1 => {5000, state2b_recv_msg2} }},
              { state_map, #{ state1_recv_msg1  => #{recv => #{msg => state2a_send_ack1} },
                              state2a_send_ack1 => #{send => #{ack => state1_recv_msg1}  },
                              state2b_recv_msg2 => #{recv => #{msg => state3a_send_ack2} },
                              state3a_send_ack2 => #{send => #{ack => state2a_send_ack1} }} }],
  Default = [ {child_options, #child_options{ restart = temporary, % transient
                                              shutdown = 2000,
                                              type = worker }},
              {sup_flags, sup_flags(one_for_all, 1, 5 )},
              {role, #role_spec{ name = ali, modules = #role_modules{mon=role_tmp}, params = [] ++ AliTempl }},
              {role, #role_spec{ name = bob, modules = #role_modules{mon=role_tmp}, params = [] ++ BobTempl }} ],
  init(Default ++ Params, #{ roles => [] }).

init([], Params) -> init(finished, Params);

init([H|T], Params) -> 
  case H of 
    {role, RoleSpec} ->
      %% add new role 
      Params1 = maps:update_with(roles, fun(V) -> V ++ [RoleSpec] end, [RoleSpec], Params);
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
  printout("~p, finished.", [?FUNCTION_NAME]),

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {tpri, sup_id, self()},

  ChildOptions = maps:get(child_options, Params),
  SupFlags = maps:get(sup_flags, Params),

  RoleChildren = maps:get(roles, Params),
  printout("~p, num_roles: ~p.", [?FUNCTION_NAME, length(RoleChildren)]),

  %% create supervisor for each role
  SpecFun = fun(#role_spec{name=Name, modules=#role_modules{sup=Sup,mon=Mon,imp=Imp}, params=RoleParams}, Acc) -> 
      RegID = list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Sup)),
      RoleParams1 = RoleParams ++ [{name,Name},{mon,Mon},{imp,Imp}],
      printout("~p, sup ~p.", [?FUNCTION_NAME, Name]),
      % printout("~p, sup ~p params: ~p.", [?FUNCTION_NAME, Name, RoleParams1]),
      Acc ++ [child_spec(Sup,RegID,ChildOptions,RoleParams1)] 
  end,

  ChildSpecs = lists:foldl(SpecFun, [], RoleChildren),

  {ok, {SupFlags, ChildSpecs}}.


% run_setup() ->
%   receive {trm, TrmID} -> ok end,
%   receive {imp, ImpID} -> ok end,

%   printout("~p, complete,\n\ttrm: ~p\n\timp: ~p.", [?FUNCTION_NAME, TrmID, ImpID]).


