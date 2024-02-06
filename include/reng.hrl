-record(graph, {graph_ref, name, type}).

-record(edge_data, {event_type, event, pattern, args, guard, resets, code, attributes, comments = []}).
-record(edge, {from, to, edge_data}).

-record(trans, {from, to, data}).
-record(data, {action, var, event, cons = []}).


-record(clock_value, {is_abs, lower_bound, upper_bound}).
-record(clock, {label, value, is_global}).
