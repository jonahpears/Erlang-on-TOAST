-compile({nowarn_unused_function, [ {get_app_params,1},
                                    {default_app_params,0},
                                    {get_server_params,1},
                                    {default_server_params,1},
                                    {default_child_options,0},
                                    {default_sup_flags,0},
                                    {new_role_spec,3} ]}).

get_app_params(asym_direct_imp=Type) -> 
  DefaultParams = default_app_params(),
  Params = #{ active_preset => asym_direct_imp, 
              asym_direct_imp => #{ enabled => true, role => cal } },
  AppParams = maps:merge(DefaultParams, Params),
  {AppParams, get_server_params(Type)};

get_app_params(Type) when is_atom(Type) -> {default_app_params(), get_server_params(Type)}.

default_app_params() -> 
  #{ num_roles => 2,
     active_preset => default,
     asym_direct_imp => #{ enabled => false,
                           role => undefined } }.

get_server_params(default_fsm) -> default_server_params(fsm);
get_server_params(default) -> default_server_params(tmp);
get_server_params(asym_direct_imp) -> default_server_params(asym_direct_imp);
get_server_params(Type) -> default_server_params(Type).

%% same as default with tmp, except ali communicates directly with other monitor
%% since ali has no monitor, bob monitor now needs to enforce timeout/protocol
default_server_params(asym_direct_imp) -> 
  BobTmp = #{ init => state1_recv_msg1,
                timeouts => #{ state2a_send_ack1 => {5000, state2b_recv_msg2},
                               state3a_send_ack2 => {5000, issue_timeout} },
                map => #{ state1_recv_msg1  => #{recv => #{msg => state2a_send_ack1} },
                                state2a_send_ack1 => #{send => #{ack => state1_recv_msg1}  },
                                state2b_recv_msg2 => #{recv => #{msg => state3a_send_ack2} },
                                state3a_send_ack2 => #{send => #{ack => state2a_send_ack1} }} },
  Default = [ {child_options, default_child_options()},
              {sup_flags, default_sup_flags()},
              {role, new_role_spec(cal, #role_modules{mon=undefined}, [{direct_imp, true}])}, 
              {role, new_role_spec(bob, #role_modules{mon=role_tmp}, [{fsm, BobTmp}])} ],
  Default;

default_server_params(fsm) -> %% {AppParamMap, ServerParamList}
  Default = [ {child_options, default_child_options()},
              {sup_flags, default_sup_flags()},
              {role, new_role_spec(ali, #role_modules{mon=role_fsm}, [])}, 
              {role, new_role_spec(bob, #role_modules{mon=role_fsm}, [])} ],
  Default;

default_server_params(tmp) ->
  AliTmp = #{ init => state1_send_msg1,
                timeouts => #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
                               state3a_recv_ack2 => {5000, issue_timeout} },
                map => #{ state1_send_msg1  => #{send => #{msg => state2a_recv_ack1} },
                                state2a_recv_ack1 => #{recv => #{ack => state1_send_msg1}  },
                                state2b_send_msg2 => #{send => #{msg => state3a_recv_ack2} },
                                state3a_recv_ack2 => #{recv => #{ack => state2a_recv_ack1} }} },
  BobTmp = #{ init => state1_recv_msg1,
                timeouts => #{ state2a_send_ack1 => {5000, state2b_recv_msg2} },
                map => #{ state1_recv_msg1  => #{recv => #{msg => state2a_send_ack1} },
                                state2a_send_ack1 => #{send => #{ack => state1_recv_msg1}  },
                                state2b_recv_msg2 => #{recv => #{msg => state3a_send_ack2} },
                                state3a_send_ack2 => #{send => #{ack => state2a_send_ack1} }} },
  Default = [ {child_options, default_child_options()},
              {sup_flags, default_sup_flags()},
              {role, new_role_spec(ali, #role_modules{mon=role_tmp}, [{fsm, AliTmp}])}, 
              {role, new_role_spec(bob, #role_modules{mon=role_tmp}, [{fsm, BobTmp}])} ],
  Default.

default_child_options() ->
  #child_options{ restart = temporary, %% transient,
                  shutdown = 2000,
                  type = worker,
                  significant = false }.


new_role_spec(Name, Modules, Params) -> #role_spec{name=Name,modules=Modules,params=Params}.


