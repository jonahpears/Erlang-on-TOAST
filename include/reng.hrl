-compile({nowarn_unused_function, [{reng_show, 2}, {reng_show, 3}]}).

-record(graph, {graph_ref, name, type}).

-record(edge_data, {
    event_type,
    event,
    trans_type,
    timeout = #{ref=>undefined},
    if = #{is_timer=>false,ref=>undefined,is_not=>false},
    pattern,
    args,
    guard,
    code,
    attributes,
    comments = [],
    timer = #{duration => -1, name => undefined},
    delay = #{ref => undefined},
    error_reason = undefined
}).
-record(edge, {
    from,
    to,
    edge_data,
    is_error = false,
    is_custom_end = false,
    is_internal_timeout_to_supervisor = false,
    is_if = false,
    is_else = false,
    is_choice = false,
    is_silent = false,
    is_timer = false,
    is_delay = false
}).

-record(trans, {from, to, data}).
-record(data, {action, var, event, cons = []}).

% -record(clock_value, {is_abs, lower_bound, upper_bound}).
% -record(clock, {label, value, is_global}).

reng_show(edges, Edges, HeadString) ->
    io:format(HeadString),
    reng_show(edges, Edges);
reng_show(edge, Edge, HeadString) ->
    io:format(HeadString),
    reng_show(edge, Edge).

reng_show(edges, Edges) ->
    lists:foreach(fun(Edge) -> reng_show(edge, Edge) end, Edges);
reng_show(edge, Edge) ->
    io:format("edge.from: ~p.\n", [Edge#edge.from]),
    io:format("edge.to: ~p.\n", [Edge#edge.to]),
    io:format("edge.is_silent: ~p.\n", [Edge#edge.is_silent]),
    io:format("edge.is_delay: ~p.\n", [Edge#edge.is_delay]),
    io:format("edge.is_custom_end: ~p.\n", [Edge#edge.is_custom_end]),
    io:format("edge.is_internal_timeout_to_supervisor: ~p.\n", [
        Edge#edge.is_internal_timeout_to_supervisor
    ]),
    reng_show(edge_data, Edge#edge.edge_data);
reng_show(edge_data, EdgeData) ->
    io:format("edge_data.args: ~p.\n", [EdgeData#edge_data.args]),
    io:format("edge_data.attributes: ~p.\n", [EdgeData#edge_data.attributes]),
    io:format("edge_data.code: ~p.\n", [EdgeData#edge_data.code]),
    io:format("edge_data.comments: ~p.\n", [EdgeData#edge_data.comments]),
    io:format("edge_data.event: ~p.\n", [EdgeData#edge_data.event]),
    io:format("edge_data.event_type: ~p.\n", [EdgeData#edge_data.event_type]),
    io:format("edge_data.guard: ~p.\n", [EdgeData#edge_data.guard]),
    io:format("edge_data.pattern: ~p.\n", [EdgeData#edge_data.pattern]),
    io:format("edge_data.timeout: ~p.\n", [EdgeData#edge_data.timeout]),
    io:format("edge_data.trans_type: ~p.\n", [EdgeData#edge_data.trans_type]);
reng_show(Kind, Record) ->
    io:format("unexpected kind ~p: ~p.\n", [Kind, Record]).
