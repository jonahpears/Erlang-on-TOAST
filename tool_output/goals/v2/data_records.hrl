
-record(statem_options, {allow_delayable_sends = false}).

-record(statem_data, {coparty_id, state_stack = [], msg_stack = [], options = #statem_options{}}).

-record(stop_data, {reason, statem_data = #statem_data{}}).
