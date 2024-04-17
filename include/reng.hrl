-record(graph, {graph_ref, name, type}).

-record(edge_data, {event_type, event, trans_type, timeout, pattern, args, guard, code, attributes, comments = []}).
-record(edge, {from, to, edge_data, is_silent = false, is_delayable_send = false, is_custom_end = false, is_internal_timeout_to_supervisor = false }).

-record(trans, {from, to, data}).
-record(data, {action, var, event, cons = []}).


-record(clock_value, {is_abs, lower_bound, upper_bound}).
-record(clock, {label, value, is_global}).
