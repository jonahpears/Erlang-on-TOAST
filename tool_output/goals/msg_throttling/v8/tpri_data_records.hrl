
printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type }, ChildParams) ->
  #{ id => ID,
     start => {Module, start_link, [[{name,ID}] ++ ChildParams]},
     restart => Restart,
     shutdown => Shutdown,
     type => Type,
     modules => [Module] }.


-record(sup_flags, { strategy = one_for_all,
                     intensity = 1,
                     period = 5 }).

-record(role_modules, { sup = role_sup,
                        fsm = role_fsm,
                        imp = role_imp }).

-record(role_spec, { name,
                     modules = #role_modules{},
                     params = [] }).

-record(statem_options, { allow_delayable_sends = false, 
                          printout_enabled = true }).

-record(child_options, { restart = transient,
                         shutdown = 2000,
                         type = worker }).

-record(supervisor_options, { strategy = one_for_all,
                              intensity = 1,
                              period = 5,
                              child_options = #child_options{},
                              statem_options = #statem_options{},
                              child_spec = #{} }).

-record(server_options, { supervisor_options = #supervisor_options{} }).

-record(statem_data, { name,
                       coparty_id, 
                       init_state,
                       states = [], 
                       msgs = #{},
                       timeouts = #{}, 
                       state_map = #{},
                       queued_actions = [],
                       options = #statem_options{} }).

-record(stop_data, {reason, statem_data = #statem_data{}}).

