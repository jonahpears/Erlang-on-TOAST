
-record(statem_options, { allow_delayable_sends = false, 
                          printout_enabled = true }).

-record(child_options, { restart = transient,
                         shutdown = 2000,
                         type = worker }).

-record(supervisor_options, { strategy = rest_for_one,
                              intensity = 2,
                              period = 3600,
                              child_options = #child_options{},
                              statem_options = #statem_options{} }).

-record(server_options, { supervisor_options = #supervisor_options{} }).

-record(statem_data, { coparty_id, 
                       state_stack = [], 
                       msg_stack = [],
                       queued_actions = [],
                       options = #statem_options{} }).

-record(stop_data, {reason, statem_data = #statem_data{}}).

