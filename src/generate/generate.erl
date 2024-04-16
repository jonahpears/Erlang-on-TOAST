%% @doc Process a protocol structure into fsm edges and nodes
-module(generate).

-export([gen/2, gen_module/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

%% @doc represent endP as the special terminate function
end_state() ->
    Func = get_merl_func_name(),
    Clause = ?Q([
                    "(_Reason, _State, _Data) ->",
                    "printout(\"(->) ~p, Reason: ~p,\nStatemData: \n\t~p.\", [_@Func, Reason, StatemData]),",
                    "ok"
                ]),
    {true, terminate, [Clause]}.

custom_end_state(_Id, Edges, _Nodes) ->
    Name = 'custom_end_state',
    % NextState = 'go_to_terminate',

    EndEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_custom_end end, Edges)),
    Timeout = EndEdge#edge.edge_data#edge_data.timeout,

    io:format("custom_end_state: timeout: ~p.\n", [Timeout]),

    % EnterClause = enter_clause(timeout, Name, Timeout, NextState),
    % EnterClause = enter_clause(timeout, Timeout, NextState),
    EnterClause = enter_clause(stop),

    % Q_Cons = EndEdge#edge.edge_data#edge_data.comments,
    % Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    % Q_Clause = timeout_clause('go_to_terminate', 0, Q_Comms),

    % Q_Clause = timeout_clause(NextState, 0, [], false),
    Q_Clause = custom_stop_clause(),

    Clauses = [EnterClause] ++ [Q_Clause],
    {true, Name, Clauses}.

get_merl_func_name() -> merl:var(list_to_atom("?FUNCTION_NAME")).
get_merl_name_name() -> merl:var(list_to_atom("?NAME")).

init_state(_Id, [Edge], Nodes) ->
    % NextState = list_to_atom("state" ++ integer_to_list(Edge#edge.to)),
    case maps:get(Edge#edge.to, Nodes) of
        end_state ->
            NextState = normal;
        custom_end_state ->
            NextState = custom_end_state;
        standard_state ->
            NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to));
        choice_state ->
            NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to));
        % mixed_choice_state -> NextState = list_to_atom("mixed_choice_state" ++ integer_to_list(Edge#edge.to));
        recv_after_state ->
            NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[init_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[init_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[init_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[init_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        % after_recv_state ->   NextState = list_to_atom("after_recv_state" ++ integer_to_list(Edge#edge.to)),
        %                       io:format("\n[init_state -> after_recv_state] Nodes: ~p.\n", [Nodes]);
        % after_send_state ->   NextState = list_to_atom("after_send_state" ++ integer_to_list(Edge#edge.to)),
        %                       io:format("\n[init_state -> after_send_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[init_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
            io:format("\n[unhandled NextState] init_state, Nodes: ~p.\n", [Nodes])
    end,
    Comms = lists:map(
        fun({A, B}) ->
            atom_to_list(A) ++ " " ++
                atom_to_list(B)
        end,
        Edge#edge.edge_data#edge_data.comments
    ),
    Cl = ?Q(["([]) -> {ok, '@NextState@', {} }"]),
    Clause =
        if
            length(Comms) > 0 ->
                erl_syntax:add_precomments(
                    lists:map(
                        fun(Com) ->
                            erl_syntax:comment([Com])
                        end,
                        Comms
                    ),
                    Cl
                );
            true ->
                Cl
        end,
    {true, init, [Clause]}.

%% @doc construct the clauses for the standard and choice states
clause(Event, Act, Var, Trans, NextState, Cons) ->
    Clause = ?Q([
        "('@Event@', {'@Act@', _@Var}, Data) ->",
        " {'@Trans@', '@NextState@', Data }"
    ]),
    if
        length(Cons) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
            );
        true ->
            Clause
    end.

queue_clause() ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(state_timeout, process_queue, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = [H|T], options = Options } = _Data) ->",
        "printout(\"(->) ~p, queued action: ~p.\", [_@Func, H]),",
        "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = T, options = Options },",
        "{repeat_state, Data1, [{next_event, cast, H}]}"
    ]),
    
    % erl_syntax:add_precomments(
    %     erl_syntax:comment(list_to_atom("% process queued actions")), Clause
    % ).
    Clause.


timeout_clause(TimeoutState, TimeoutDuration, Cons) ->
    timeout_clause(TimeoutState, TimeoutDuration, Cons, true).
timeout_clause(TimeoutState, TimeoutDuration, Cons, ShowIO) ->
    % timeout_clause(Cons) ->
    % Clause = ?Q(["(state_timeout, TimeoutState, Data) ->", " {next_state, TimeoutState, Data}"]),
    % PrintStr = io_lib:format("io:format(\"(timeout[~p] -> ~p.)\n\")", [TimeoutDuration,TimeoutState]),
    if
        ShowIO == true ->
            Clause = ?Q([
                "(state_timeout, '@TimeoutState@', Data) ->",
                "io:format(\"(timeout[~p] -> ~p.)\n\",['@TimeoutDuration@','@TimeoutState@']),",
                "{next_state, '@TimeoutState@', Data}"
            ]),
            Cons1 = Cons ++ ["% This is a timeout branch:"];
        true ->
            Clause = ?Q([
                "(state_timeout, '@TimeoutState@', Data) ->",
                "{next_state, '@TimeoutState@', Data}"
            ]),
            %++ ["% This is a timeout branch:"]
            Cons1 = Cons
    end,

    if
        length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            );
        true ->
            Clause
    end.

%% @doc an extra clause for enter state
enter_clause() -> enter_clause(basic).
enter_clause(basic) ->
    ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options } = _Data) -> keep_state_and_data"
    ]);
enter_clause(queues) ->
    [enter_clause(queue_process), enter_clause(queue_empty)];
enter_clause(queue_process) ->
    Func = get_merl_func_name(),
    ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [_H], options = _Options } = _Data) ->",
        "printout(\"(->) ~p, queued actions.\", [_@Func]),",
        " {keep_state_and_data, [{state_timeout, 0, process_queue}]}"
    ]);
enter_clause(queue_empty) ->
    Func = get_merl_func_name(),
    ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [], options = _Options } = _Data) ->",
        "printout(\"(->) ~p.\", [_@Func]),",
        " keep_state_and_data"
    ]);
enter_clause(state) ->
    Func = get_merl_func_name(),
    ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options } = _Data) ->",
        "printout(\"(->) ~p.\", [_@Func]),",
        %  "io:format(\"(~p ->.)\n\",['@State@']),",
        " keep_state_and_data"
    ]);
enter_clause(stop) ->
    ?Q([
        "(enter, _OldState, #stop_data{ reason = _Reason, statem_data = _StatemData } = _Data) ->",
        " {keep_state_and_data, [{state_timeut, 0, go_to_terminate}]}"
    ]).
enter_clause(timeout, Timeout, ToState) ->
    Func = get_merl_func_name(),
    [
        enter_clause(queue_process),
        ?Q([
            "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [], options = _Options } = Data) ->",
            "io:format(\"(~p ->.)\n\",[_@Func]),",
            "{keep_state, Data, [{state_timeout, '@Timeout@', '@ToState@'}]}"
        ])
    ].
% enter_clause(Timeout,State) -> ?Q(["(enter, _OldState, Data) ->", "{keep_state, Data, [{state_timeout, ", Timeout, ", ", State, "}]}"]).
% enter_clause(Timeout,State) -> ?Q(["(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout,1000,init}]}"]).
custom_stop_clause() ->
    Func = get_merl_func_name(),
    ?Q([
        "(state_timeout, go_to_terminate, #stop_data{ reason = Reason, statem_data = StatemData } = _Data) ->",
        "printout(\"(->) ~p, Reason: ~p,\nStatemData: \n\t~p.\", [_@Func, Reason, StatemData]),",
        " {stop, Reason, Data}"
    ]).
stop_clause() ->
    Func = get_merl_func_name(),
    Name = get_merl_name_name(),
    ?Q([
        "() -> printout(\"~p.\", [_@Func]), gen_statem:stop(_@Name)"
    ]).

%% @doc generates standard states, i.e. act x
std_state(Id, [Edge], Nodes) ->
    case maps:get(Edge#edge.to, Nodes) of
        end_state ->
            NextState = normal,
            Trans = stop;
        custom_end_state ->
            NextState = custom_end_state,
            Trans = next_state;
        standard_state ->
            NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state;
        choice_state ->
            NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state;
        % mixed_choice_state -> NextState = list_to_atom("mixed_choice_state" ++ integer_to_list(Edge#edge.to)),
        %            Trans = next_state;
        recv_after_state ->
            NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state,
            io:format("\n[std_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state,
            io:format("\n[std_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state,
            io:format("\n[std_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state,
            io:format("\n[std_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        % after_recv_state ->   NextState = list_to_atom("after_recv_state" ++ integer_to_list(Edge#edge.to)),
        %                       Trans = next_state,
        %                       io:format("\n[std_state -> after_recv_state] Nodes: ~p.\n", [Nodes]);
        % after_send_state ->   NextState = list_to_atom("after_send_state" ++ integer_to_list(Edge#edge.to)),
        %                       Trans = next_state,
        %                       io:format("\n[std_state -> after_send_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state,
            io:format("\n[std_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
            Trans = next_state
    end,
    {Act, Var} = Edge#edge.edge_data#edge_data.event,
    Event = Edge#edge.edge_data#edge_data.event_type,
    Cons = Edge#edge.edge_data#edge_data.comments,

    Comms = lists:map(
        fun({A, B}) ->
            atom_to_list(A) ++ " " ++
                atom_to_list(B)
        end,
        Cons
    ),
    Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms),
    % Name = list_to_atom("std_state" ++ integer_to_list(Id)),
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_std"),
    % {true, Name, [enter_clause(), Clause]}.
    {true, Name, enter_clause(queues) ++ [queue_clause(), Clause]}.

%% @doc generates choice states, i.e. branch
choice_state(Id, Edges, Nodes) ->
    Fun = fun(Edge) ->
        case maps:get(Edge#edge.to, Nodes) of
            end_state ->
                NextState = normal,
                Trans = stop;
            custom_end_state ->
                NextState = custom_end_state,
                Trans = next_state;
            standard_state ->
                NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            choice_state ->
                NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            % mixed_choice_state -> NextState = list_to_atom("mixed_choice_state" ++ integer_to_list(Edge#edge.to)),
            %          Trans = next_state;
            recv_after_state ->
                NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[choice_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
            branch_after_state ->
                NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[choice_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
            send_after_state ->
                NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[choice_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
            select_after_state ->
                NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[choice_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
            % after_recv_state ->   NextState = list_to_atom("after_recv_state" ++ integer_to_list(Edge#edge.to)),
            %                       Trans = next_state,
            %                       io:format("\n[choice_state -> after_recv_state] Nodes: ~p.\n", [Nodes]);
            % after_send_state ->   NextState = list_to_atom("after_send_state" ++ integer_to_list(Edge#edge.to)),
            %                       Trans = next_state,
            %                       io:format("\n[choice_state -> after_send_state] Nodes: ~p.\n", [Nodes]);
            after_state ->
                NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[choice_state -> after_state] Nodes: ~p.\n", [Nodes]);
            _Else ->
                NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state
        end,
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    end,

    % Name = list_to_atom("choice_state" ++ integer_to_list(Id)),
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_choice"),
    % Clauses = [enter_clause(state, Name)] ++ lists:map(Fun, Edges),
    Clauses = enter_clause(queues) ++ [queue_clause()] ++lists:map(Fun, Edges),
    {true, Name, Clauses}.

recv_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_recv_after"),
    % Name = list_to_atom("recv_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    case maps:get(TimeoutEdge#edge.to, Nodes) of
        end_state ->
            Q_NextState = normal;
        % Q_Trans = stop;
        custom_end_state ->
            Q_NextState = 'custom_end_state';
        % Trans = next_state;

        standard_state ->
            Q_NextState = list_to_atom("std_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        choice_state ->
            Q_NextState = list_to_atom("choice_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        recv_after_state ->
            Q_NextState = list_to_atom("recv_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: recv_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            Q_NextState = list_to_atom(
                "branch_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: recv_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            Q_NextState = list_to_atom("send_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: recv_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            Q_NextState = list_to_atom(
                "select_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: recv_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            Q_NextState = list_to_atom("after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[ERROR] [Q: recv_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            Q_NextState = list_to_atom("_state" ++ integer_to_list(TimeoutEdge#edge.to))
        % Q_Trans = next_state
    end,

    % EnterClause = enter_clause(timeout, Name, Timeout, Q_NextState),
    EnterClause = enter_clause(timeout, Timeout, Q_NextState),
    % ProcessQueueClauses = enter_clause(queues),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    %% same as branch, but should only really be one other edge
    Fun = fun(Edge) ->
        case maps:get(Edge#edge.to, Nodes) of
            end_state ->
                NextState = normal,
                Trans = stop;
            custom_end_state ->
                NextState = custom_end_state,
                Trans = next_state;
            standard_state ->
                NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            choice_state ->
                NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            recv_after_state ->
                NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[recv_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
            branch_after_state ->
                NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[recv_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
            send_after_state ->
                NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[recv_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
            select_after_state ->
                NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[recv_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
            after_state ->
                NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[ERROR] [recv_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
            _Else1 ->
                NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state
        end,
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    end,

    %% remove silent edge from edges
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    Edges2 = lists:map(Fun, Edges1),

    % %% create default delay
    % DelayClause = ?Q([""]),

    % EnterClause1 = erl_syntax:add_postcomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, ["default delay: ('@Timeout@')"]), EnterClause),

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    % Clauses = [EnterClause] ++ ProcessQueueClauses ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause],
    {true, Name, Clauses}.

branch_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_branch_after"),
    % Name = list_to_atom("branch_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    case maps:get(TimeoutEdge#edge.to, Nodes) of
        end_state ->
            Q_NextState = normal;
        % Q_Trans = stop;
        custom_end_state ->
            Q_NextState = 'custom_end_state';
        standard_state ->
            Q_NextState = list_to_atom("std_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        choice_state ->
            Q_NextState = list_to_atom("choice_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        recv_after_state ->
            Q_NextState = list_to_atom("recv_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: branch_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            Q_NextState = list_to_atom(
                "branch_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: branch_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            Q_NextState = list_to_atom("send_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: branch_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            Q_NextState = list_to_atom(
                "select_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: branch_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            Q_NextState = list_to_atom("after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[ERROR] [Q: branch_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            Q_NextState = list_to_atom("_state" ++ integer_to_list(TimeoutEdge#edge.to))
        % Q_Trans = next_state
    end,

    % EnterClause = enter_clause(timeout, Name, Timeout,Q_NextState),
    EnterClause = enter_clause(timeout, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    Fun = fun(Edge) ->
        % if
        %   Edge#edge.is_silent ->
        %       io:format("\n[branch_after_state] is_silent, Nodes: ~p.\n", [Nodes]),

        %       % TimeoutState = Name,
        %       Cons = Edge#edge.edge_data#edge_data.comments,

        %       Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        %       Clause = timeout_clause(AfterName, Comms),
        %       % Clause = timeout_clause(TimeoutState, Comms),
        %       % {true, ParentName, Clause}.
        %       % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        %       io:format("\n[branch_after_state] Clause: ~p.\n", [Clause]),
        %       Clause;

        %   true ->
        case maps:get(Edge#edge.to, Nodes) of
            end_state ->
                NextState = normal,
                Trans = stop;
            custom_end_state ->
                NextState = custom_end_state,
                Trans = next_state;
            standard_state ->
                NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            choice_state ->
                NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            recv_after_state ->
                NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[branch_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
            branch_after_state ->
                NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[branch_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
            send_after_state ->
                NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[branch_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
            select_after_state ->
                NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[branch_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
            after_state ->
                NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[ERROR] [branch_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
            _Else1 ->
                NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state
        end,
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    % end
    end,

    %% remove silent edge from edges
    % io:format("\n[branch_after_state] Edges: ~p.\n",[Edges]),
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    % io:format("\n[branch_after_state] Edges1: ~p.\n",[Edges1]),
    Edges2 = lists:map(Fun, Edges1),

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause],
    % Clauses = [EnterClause] ++ Edges,
    {true, Name, Clauses}.

send_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_send_after"),
    % Name = list_to_atom("send_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    case maps:get(TimeoutEdge#edge.to, Nodes) of
        end_state ->
            Q_NextState = normal;
        % Q_Trans = stop;
        custom_end_state ->
            Q_NextState = 'custom_end_state';
        standard_state ->
            Q_NextState = list_to_atom("std_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        choice_state ->
            Q_NextState = list_to_atom("choice_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        recv_after_state ->
            Q_NextState = list_to_atom("recv_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: send_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            Q_NextState = list_to_atom(
                "branch_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: send_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            Q_NextState = list_to_atom("send_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: send_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            Q_NextState = list_to_atom(
                "select_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: send_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            Q_NextState = list_to_atom("after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[ERROR] [Q: send_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            Q_NextState = list_to_atom("_state" ++ integer_to_list(TimeoutEdge#edge.to))
        % Q_Trans = next_state
    end,

    % EnterClause = enter_clause(timeout, Name, Timeout, Q_NextState),
    EnterClause = enter_clause(timeout, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    %% same as branch, but should only really be one other edge
    Fun = fun(Edge) ->
        case maps:get(Edge#edge.to, Nodes) of
            end_state ->
                NextState = normal,
                Trans = stop;
            custom_end_state ->
                NextState = custom_end_state,
                Trans = next_state;
            standard_state ->
                NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            choice_state ->
                NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            recv_after_state ->
                NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[send_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
            branch_after_state ->
                NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[send_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
            send_after_state ->
                NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[send_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
            select_after_state ->
                NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[send_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
            after_state ->
                NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[ERROR] [send_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
            _Else1 ->
                NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state
        end,
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    end,

    %% remove silent edge from edges
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    Edges2 = lists:map(Fun, Edges1),

    % %% create default delay
    % DelayClause = ?Q([""]),

    % EnterClause1 = erl_syntax:add_postcomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, ["default delay: ('@Timeout@')"]), EnterClause),

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause],
    {true, Name, Clauses}.

select_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_select_after"),
    % Name = list_to_atom("select_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    case maps:get(TimeoutEdge#edge.to, Nodes) of
        end_state ->
            Q_NextState = normal;
        % Q_Trans = stop;
        custom_end_state ->
            Q_NextState = 'custom_end_state';
        standard_state ->
            Q_NextState = list_to_atom("std_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        choice_state ->
            Q_NextState = list_to_atom("choice_state" ++ integer_to_list(TimeoutEdge#edge.to));
        % Q_Trans = next_state;

        recv_after_state ->
            Q_NextState = list_to_atom("recv_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: select_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
        branch_after_state ->
            Q_NextState = list_to_atom(
                "branch_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: select_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
        send_after_state ->
            Q_NextState = list_to_atom("send_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[Q: select_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
        select_after_state ->
            Q_NextState = list_to_atom(
                "select_after_state" ++ integer_to_list(TimeoutEdge#edge.to)
            ),
            % Q_Trans = next_state,
            io:format("\n[Q: select_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            Q_NextState = list_to_atom("after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
            % Q_Trans = next_state,
            io:format("\n[ERROR] [Q: select_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
        _Else ->
            Q_NextState = list_to_atom("_state" ++ integer_to_list(TimeoutEdge#edge.to))
        % Q_Trans = next_state
    end,

    % EnterClause = enter_clause(timeout, Name, Timeout,Q_NextState),
    EnterClause = enter_clause(timeout, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    Fun = fun(Edge) ->
        % if
        %   Edge#edge.is_silent ->
        %       io:format("\n[select_after_state] is_silent, Nodes: ~p.\n", [Nodes]),

        %       % TimeoutState = Name,
        %       Cons = Edge#edge.edge_data#edge_data.comments,

        %       Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        %       Clause = timeout_clause(AfterName, Comms),
        %       % Clause = timeout_clause(TimeoutState, Comms),
        %       % {true, ParentName, Clause}.
        %       % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        %       io:format("\n[select_after_state] Clause: ~p.\n", [Clause]),
        %       Clause;

        %   true ->
        case maps:get(Edge#edge.to, Nodes) of
            end_state ->
                NextState = normal,
                Trans = stop;
            custom_end_state ->
                NextState = custom_end_state,
                Trans = next_state;
            standard_state ->
                NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            choice_state ->
                NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state;
            recv_after_state ->
                NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[select_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);
            branch_after_state ->
                NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[select_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);
            send_after_state ->
                NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[select_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);
            select_after_state ->
                NextState = list_to_atom("select_after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[select_after_state -> select_after_state] Nodes: ~p.\n", [Nodes]);
            after_state ->
                NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state,
                io:format("\n[ERROR] [select_after_state -> after_state] Nodes: ~p.\n", [Nodes]);
            _Else1 ->
                NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
                Trans = next_state
        end,
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    % end
    end,

    %% remove silent edge from edges
    % io:format("\n[select_after_state] Edges: ~p.\n",[Edges]),
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    % io:format("\n[select_after_state] Edges1: ~p.\n",[Edges1]),
    Edges2 = lists:map(Fun, Edges1),

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause],
    % Clauses = [EnterClause] ++ Edges,
    {true, Name, Clauses}.

% send_after_state(Id, Edges, Nodes) ->
%   Name = list_to_atom("send_after_state" ++ integer_to_list(Id)),

%   TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
%   Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

%   case maps:get(TimeoutEdge#edge.to, Nodes) of
%       end_state ->          Q_NextState = normal;
%                             % Q_Trans = stop;

%       standard_state ->     Q_NextState = list_to_atom("std_state" ++ integer_to_list(TimeoutEdge#edge.to));
%                             % Q_Trans = next_state;

%       choice_state ->       Q_NextState = list_to_atom("choice_state" ++ integer_to_list(TimeoutEdge#edge.to));
%                             % Q_Trans = next_state;

%       recv_after_state ->   Q_NextState = list_to_atom("recv_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
%                             % Q_Trans = next_state,
%                             io:format("\n[Q: send_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);

%       branch_after_state -> Q_NextState = list_to_atom("branch_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
%                             % Q_Trans = next_state,
%                             io:format("\n[Q: send_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);

%       send_after_state ->   Q_NextState = list_to_atom("send_after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
%                             % Q_Trans = next_state,
%                             io:format("\n[Q: send_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);

%       after_state ->        Q_NextState = list_to_atom("after_state" ++ integer_to_list(TimeoutEdge#edge.to)),
%                             % Q_Trans = next_state,
%                             io:format("\n[ERROR] [Q: send_after_state -> after_state] Nodes: ~p.\n", [Nodes]);

%       _Else ->              Q_NextState = list_to_atom("_state" ++ integer_to_list(TimeoutEdge#edge.to))
%                             % Q_Trans = next_state
%   end,

%   EnterClause = enter_clause(Timeout, Q_NextState),
%   Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

%   Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
%   Q_Clause = timeout_clause(Q_NextState, Q_Comms),

%   %% same as branch, but should only really be one other edge
%   Fun = fun(Edge) ->
%       case maps:get(Edge#edge.to, Nodes) of
%           end_state ->          NextState = normal,
%                                 Trans = stop;

%           standard_state ->     NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state;

%           choice_state ->       NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state;

%           recv_after_state ->   NextState = list_to_atom("recv_after_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state,
%                                 io:format("\n[send_after_state -> recv_after_state] Nodes: ~p.\n", [Nodes]);

%           branch_after_state -> NextState = list_to_atom("branch_after_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state,
%                                 io:format("\n[send_after_state -> branch_after_state] Nodes: ~p.\n", [Nodes]);

%           send_after_state ->   NextState = list_to_atom("send_after_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state,
%                                 io:format("\n[send_after_state -> send_after_state] Nodes: ~p.\n", [Nodes]);

%           after_state ->        NextState = list_to_atom("after_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state,
%                                 io:format("\n[ERROR] [send_after_state -> after_state] Nodes: ~p.\n", [Nodes]);

%           _Else1 ->              NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
%                                 Trans = next_state
%       end,
%       {Act, Var} = Edge#edge.edge_data#edge_data.event,
%       Event = Edge#edge.edge_data#edge_data.event_type,
%       Cons = Edge#edge.edge_data#edge_data.comments,
%       Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
%       clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
%   end,

%   %% remove silent edge from edges
%   Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
%   Edges2 = lists:map(Fun, Edges1),

%   % %% create default delay
%   % DelayClause = ?Q([""]),

%   % EnterClause1 = erl_syntax:add_postcomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, ["default delay: ('@Timeout@')"]), EnterClause),

%   Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
%   {true, Name, Clauses}.

% % after_recv_state(Id, Edges, Nodes) -> 0.
% % after_send_state(Id, Edges, Nodes) -> 0.

% % after_state(Id, [Edge], Nodes) -> 0.%after_state(Id, [Edge], Nodes, {"test", "tset"}).

% % after_state(Id, [Edge], Nodes, {Name, ParentName}) ->
% %   io:format("\n[after_state], Id: ~p,\n\tEdge: ~p,\n\tNodes: ~p,\n\t{~p, ~p}.\n", [Id, Edge, Nodes, Name, ParentName]),

% %   TimeoutState = Name,
% %   Cons = Edge#edge.edge_data#edge_data.comments,

% %   Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
% %   Clause = timeout_clause(TimeoutState, Comms),
% %   {true, ParentName, Clause}.

%% @doc generates mixed-choice states, i.e. branch-after, act-after, if-then-else
%% TODO add mixed-choice state (timeouts)
%% TODO add if-then-else ?
% mixed_choice_state(Id, Edges, Nodes) ->
%   Fun = fun(Edge) ->
%     case maps:get(Edge#edge.to, Nodes) of
%       end_state -> NextState = normal,
%                    Trans = stop;
%       standard_state -> NextState = list_to_atom("std_state" ++ integer_to_list(Edge#edge.to)),
%               Trans = next_state;
%       choice_state -> NextState = list_to_atom("choice_state" ++ integer_to_list(Edge#edge.to)),
%                Trans = next_state;
%       % mixed_choice_state -> NextState = list_to_atom("mixed_choice_state" ++ integer_to_list(Edge#edge.to)),
%       %          Trans = next_state;
%       _Else -> NextState = list_to_atom("_state" ++ integer_to_list(Edge#edge.to)),
%                Trans = next_state
%     end,
%     {Act, Var} = Edge#edge.edge_data#edge_data.event,
%     Event = Edge#edge.edge_data#edge_data.event_type,
%     Cons = Edge#edge.edge_data#edge_data.comments,
%     Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
%     clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
%   end,

% %% TODO find way to extract the timeout and destination from Edges?
% %% ! maybe structure choices so that thye have to be only one pair of mixed choices?
% %% ! or maybe represent them as a list of substates
%   Timeout = 0,
%   Q = 0,

%   Clauses = [enter_clause(Timeout,Q)] ++ lists:map(Fun, Edges),
%   Name = list_to_atom("mixed_state" ++ integer_to_list(Id)),
%   {true, Name, Clauses}.

%% @doc calls the appropriate function for choice and standard states
state_funs(K, V, Edges, Nodes) ->
    % io:format("\n[state_funs] K: ~p,\n\tV: ~p\n\tEdges: ~p\n\tNodes: ~p.\n", [K,V,Edges,Nodes]),
    Pred = fun(Edge) -> Edge#edge.from =:= K end,
    Es = lists:filter(Pred, Edges),
    case V of
        init_state ->
            init_state(K, Es, Nodes);
        end_state ->
            end_state();
        custom_end_state ->
            custom_end_state(K, Es, Nodes);
        choice_state ->
            choice_state(K, Es, Nodes);
        % mixed_choice_state -> mixed_choice_state(K, Es, Nodes);
        standard_state ->
            std_state(K, Es, Nodes);
        %
        recv_after_state ->
            recv_after_state(K, Es, Nodes);
        branch_after_state ->
            branch_after_state(K, Es, Nodes);
        send_after_state ->
            send_after_state(K, Es, Nodes);
        select_after_state ->
            select_after_state(K, Es, Nodes);
        % after_recv_state -> after_recv_state(K, Es, Nodes);
        % after_send_state -> after_send_state(K, Es, Nodes);
        % after_state -> after_state(K, Es, Nodes);
        unknown_state ->
            io:format("\n[unknown_state] state_funs: ~p.\n", [V]),
            std_state(K, Es, Nodes)
    end.

%% @doc generate the callback functions
cb_fun(Edge, NameMacro) ->
    Data = Edge#edge.edge_data,
    {Act, Var} = Data#edge_data.event,
    Event = Data#edge_data.event_type,
    Var1 = merl:var(Var),
    % FuncClause = ?Q(["(_@Var1) -> "]),
    % ActClause = ?Q(["gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"]),
    if
        Edge#edge.is_delayable_send ->
            Timeout = Data#edge_data.timeout,
            % TimeDelayClause = erl_syntax:add_precomments(
            %       [erl_syntax:comment(["default delay ('@Timeout@')"])],
            %       ?Q([ "TimeDelay = (rand:uniform('@Timeout@' * 2))*1000" ])
            %   ),

            % TimerClause = erl_syntax:add_precomments(
            %       [erl_syntax:comment(["some time consuming task:"])],
            %       ?Q([ "timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []})" ])
            %   ),

            % WaitClause = erl_syntax:add_precomments(
            %       [erl_syntax:comment(["wait for time consuming task to end. if early, may still be able to send before timeout triggers"])],
            %       ?Q([ "receive {delay_stop, TimeDelay, _MoreData} -> io:format(\"[send_accept]: delay(~p) stopped.\n\", [TimeDelay]) end" ])
            %   ),

            % Clauses = [
            %         FuncClause,
            %         TimeDelayClause,
            %         TimerClause,
            %         WaitClause,
            %         FuncClause,
            %         ActClause
            %   ];
            Clause = ?Q([
                "(_@Var1) -> TimeDelay = rand:uniform('@Timeout@' * 2), timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}), receive {delay_stop, TimeDelay, _MoreData} -> io:format(\"[send_accept]: delay(~p) stopped.\n\", [TimeDelay]) end, gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
            % Clause = ?Q(["(_@Var1) -> TimeDelay = (rand:uniform('@Timeout@' * 2))*1000, timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}), receive {delay_stop, TimeDelay, _MoreData} -> io:format(\"[send_accept]: delay(~p) stopped.\n\", [TimeDelay]) end, gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"]),
            Comms = [
                io_lib:format("% Timeout (~p)", [Timeout]),
                "% Duration `TimeDelay` may be long enough to trigger a timeout.",
                "% Timer represents some time consuming task that must be completed before performing send.",
                io_lib:format("% If TimeDelay>~p then timeout will trigger.", [Timeout]),
                "% Otherwise, send action is performed."
            ],
            Clauses = [
                erl_syntax:add_precomments(
                    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Comms), Clause
                )
            ];
        true ->
            % Clauses = [
            %         FuncClause,
            %         ActClause
            %   ]
            Clause = ?Q(["(_@Var1) -> gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"]),
            Clauses = [Clause]
    end,
    % {true, Act, [Clause]}.
    {true, Act, Clauses}.

gen_module(FileName, P) ->
    Server = merl:var(list_to_atom("?SERVER")),
    Module = merl:var(list_to_atom("?MODULE")),

    Start = ?Q(["() -> ", "gen_statem:start_link({local, _@Server}, _@Module, [], []) "]),

    % Cb = merl:quote(["() -> ", "[state_functions, state_enter]" ]),
    Cb = merl:quote(["() -> ", "[state_functions, state_enter]"]),

    Stop = merl:quote(["() -> ", "gen_statem:stop(_@Server)"]),

    % Init = merl:quote(["([]) ->
    %              {ok, state1, {}}
    %           "]),

    {Edges, Nodes} = build_fsm:to_fsm(P),

    io:format("\n------ FSM:\nNodes: ~p\nEdges: ~p\n------\n", [Nodes, Edges]),

    NonEndEdges = lists:filter(fun(Elem) -> not Elem#edge.is_custom_end end, Edges),

    io:format("\n------ FSM (filtered NonEndEdges): ~p\n------\n", [NonEndEdges]),

    StateFuns = maps:fold(
        fun(K, V, AccIn) ->
            % io:format("\n[StateFuns] (~p, ~p):...\n\tAccIn: ~p.\n",[K,V,AccIn]),
            StateFun = state_funs(K, V, Edges, Nodes),
            % io:format("\n\tStateFun: ~p.\n",[StateFun]),
            AccIn ++ [StateFun]
        end,
        [],
        Nodes
    ),

    NonSilentEdges = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, NonEndEdges),
    io:format("\n------ FSM (filtered NonSilentEdges): ~p\n------\n", [NonSilentEdges]),

    CBFuns = lists:foldl(
        fun(Edge, AccIn) ->
            % AccIn ++ [cb_fun(Edge#edge.edge_data, Server)]
            AccIn ++ [cb_fun(Edge, Server)]
        end,
        [],
        lists:delete(hd(NonSilentEdges), NonSilentEdges)
    ),

    io:format("--- success: CBFuns: ~p\n-----\n", [CBFuns]),

    Fs =
        [
            {true, start_link, [Start]},
            {true, callback_mode, [Cb]}
            % {true, init, [Init]}
            | StateFuns
        ] ++ lists:usort(CBFuns) ++ [{true, stop, [Stop]}],

    io:format("--- success: Fs: ~p\n------\n", [Fs]),

    Forms = merl_build:add_attribute(
        behaviour, [merl:term('gen_statem')], merl_build:init_module(FileName)
    ),

    Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),

    Forms2 = merl_build:add_record(
        statem_options,
        [
            {'allow_delayable_sends', merl:term('false')},
            {'printout_enabled', merl:term('true')}
        ],
        Forms1
    ),

    Forms3 = merl_build:add_record(
        statem_data,
        [
            {'coparty_id', merl:term('undefined')},
            {'state_stack', merl:var('[]')},
            {'msg_stack', merl:var('[]')},
            {'queued_actions', merl:var('[]')},
            {'options', merl:var('#statem_options{}')}
            % {'trace', merl:var('[]')},
            % {'msgs', merl:var('[]')},
            % {'state_map', merl:var('{}')},
            % {'delayable_sends', merl:term('false')}
        ],
        Forms2
    ),

    Forms4 = merl_build:add_function(false, printout, [
        ?Q(["(Str, Params) -> io:format(\"[~p|~p]: \" ++ Str ++ \"\n\", [_@Module, self()] ++ Params)"])
    ], Forms3),

    io:format("--- success: Forms.\n"),

    merl_build:module_forms(
        lists:foldl(
            fun({X, Name, Cs}, S) ->
                merl_build:add_function(X, Name, Cs, S)
            end,
            Forms4,
            Fs
        )
    ).

-spec gen(interleave:protocol(), string()) -> none().
gen(P, FileName) ->
    ModuleName = list_to_atom(lists:last(lists:droplast(string:tokens(FileName, "/.")))),
    Forms = gen_module(ModuleName, P),
    io:format("--- success: gen_module.\n"),
    % Program = erl_prettypr:format(erl_syntax:form_list(Forms),[{paper,160},{ribbon,80}]),
    Program = erl_prettypr:format(erl_syntax:form_list(Forms), [{paper, 160}, {ribbon, 160}]),
    io:format("--- success: Program.\n"),
    io:format("\n------ Program:\n~s\n------\n", [Program]),
    file:write_file(string:concat("tool_output/", FileName), Program).
