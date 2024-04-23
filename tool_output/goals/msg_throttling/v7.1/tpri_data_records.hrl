


% -record(imp_options, {}).


% -record(trm_sup_options, { strategy = one_for_all,
%                            intensity = 1,
%                            period = 5,
%                            child_options = #child_options{},
%                            statem_options = #statem_options{},
%                            child_spec = #{} }).


% -record(imp_sup_options, { strategy = one_for_all,
%                            intensity = 1,
%                            period = 5,
%                            child_options = #child_options{},
%                            imp_options = #imp_options{},
%                            child_spec = #{} }).

% -record(tpri_sup_options, { trm_options = #trm_sup_options{},
%                             imp_options = #imp_sup_options{},
%                             tpri_child_specs = #{} }).



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

