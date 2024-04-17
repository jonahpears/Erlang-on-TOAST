%% @doc Process a protocol structure into fsm edges and nodes
-module(generate).

-export([gen/2, gen_module/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

get_next_state_and_trans(To, Num) ->
    case To of
        end_state ->
            Trans = stop,
            NextState = normal;
        custom_end_state ->
            Trans = next_state,
            NextState = custom_end_state;
        standard_state ->
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_std");
        choice_state ->
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_choice");
        % mixed_choice_state -> NextState = list_to_atom("mixed_choice_state" ++ Num);
        recv_after_state ->
            % io:format("\n[init_state -> recv_after_state] Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_recv_after");
        branch_after_state ->
            % io:format("\n[init_state -> branch_after_state] Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_branch_after");
        send_after_state ->
            % io:format("\n[init_state -> send_after_state] Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_send_after");
        select_after_state ->
            % io:format("\n[init_state -> select_after_state] Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_select_after");
        % after_recv_state ->   NextState = list_to_atom("after_recv_state" ++ Num),
        %                       io:format("\n[init_state -> after_recv_state] Nodes: ~p.\n", [Nodes]);
        % after_send_state ->   NextState = list_to_atom("after_send_state" ++ Num),
        %                       io:format("\n[init_state -> after_send_state] Nodes: ~p.\n", [Nodes]);
        after_state ->
            % io:format("\n[init_state -> after_state] Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_after");
        fatal_timeout_state ->
          Trans = next_state,
          NextState = list_to_atom("state" ++ Num ++ "_fatal_timeout");
        _Else ->
            % io:format("\n[unhandled NextState] init_state, Nodes: ~p.\n", [Nodes]),
            Trans = next_state,
            NextState = list_to_atom("state" ++ Num ++ "_unexpected_" ++ atom_to_list(To))
    end,
    {NextState, Trans}.

get_next_state(To, Num) ->
    {NextState, _Trans} = get_next_state_and_trans(To, Num),
    NextState.

%% @doc represent endP as the special terminate function
end_state() ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(Reason, State, Data) ->",
        "printout(\"(->) ~p, ~p, Reason: ~p,\nData: \n\t~p.\", [_@Func, State, Reason, Data]),",
        "ok"
    ]),
    {true, terminate, [Clause]}.

custom_end_state(_Id, Edges, _Nodes) ->
    Name = 'custom_end_state',
    % NextState = 'go_to_terminate',

    EndEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_custom_end end, Edges)),
    Timeout = EndEdge#edge.edge_data#edge_data.timeout,

    io:format("custom_end_state: timeout: ~p.\n", [Timeout]),

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

init_setup_state(_Id, [Edge], Nodes) ->
    % NextState = list_to_atom("state" ++ integer_to_list(Edge#edge.to)),
    NextState = get_next_state(maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)),
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options } = _Data) ->",
        "{keep_state_and_data, [{state_timeout, 0, wait_to_finish_setup}]}"
    ]),

    Func = get_merl_func_name(),

    Clause2 = ?Q([
        "(state_timeout, wait_to_finish_setup, #statem_data{ coparty_id = _CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options } = _Data) -> ",
        "printout(\"~p, waiting to finish setup.\", [_@Func]), ",
        "receive {_SupervisorID, sup_init, CoPartyID} -> ",
        "printout(\"~p, received coparty ID (~p).\", [_@Func, CoPartyID]), ",
        "{next_state, '@NextState@', #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options }} ",
        "end"
    ]),


    Cons = [
            "% @doc Waits to receive the process-ID of the `coparty' before continuing to the initial state of the FSM.",
            "% Note: In the binary setting, sessions are only composed of two participants."
          ],
    Clause1 = erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ),
    Clauses = [Clause1, Clause2],
    {true, init_setup_state, Clauses}.

init_state(no_data) ->
    Clause = ?Q([
        "([{HKey,HVal}|T])",
        "when is_atom(HKey) and is_map_key(HKey, #statem_options{}) ->",
        "init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})})"
      ]),
    Cons = [ "% @doc Parse init params." ],
    Clause1 = erl_syntax:add_precomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause),

    Clauses = [
        Clause1,
        ?Q(["([_H|T]) -> init(T, #statem_data{})"]),
        ?Q(["([]) -> {ok, init_setup_state, #statem_data{}}"])
    ],

    % {true, init, Clauses};
    Clauses;
init_state(with_data) ->
  Clause = ?Q([
            "([{HKey,HVal}|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = Options} = Data) ",
            "when is_atom(HKey) and is_map_key(HKey, Options) -> ",
            "init(T, maps:put(options, maps:put(HKey, HVal, Options), Data))"
        ]),
    
        Cons = [ "% @doc Parse init params (with #statem_data already created)." ],
        Clause1 = erl_syntax:add_precomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause),
    
    Clauses = [
        Clause1,
        ?Q([
            "([_H|T], ",
            "#statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> ",
            "init(T, Data)"
        ]),
        ?Q([
            "([_H|T], ",
            "#statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> ",
            "init(T, Data)"
        ]),
        ?Q([
            "([], ",
            "#statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> ",
            "{ok, init_setup_state, Data}"
        ])
    ],

    % {true, init, Clauses}.
    Clauses.

clause(send, Act, Var, Trans, NextState, Cons) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(cast, {send, {'@Act@', _@Var}}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled, persistent_queue = _PersistentQueue } = Options } = _Data) ->",
        "CoPartyID ! {self(), {'@Act@', _@Var}},",
        "printout(\"~p, send (~p) to ~p.\", [_@Func, {'@Act@', _@Var}, CoPartyID]),",
        "NextState = '@NextState@',",
        "case maps:find('@Act@', Msgs) of ",
        " {ok, StateMsgs} -> Msgs1 = maps:put('@Act@', [_@Var] ++ StateMsgs, Msgs);",
        " error -> Msgs1 = maps:put('@Act@', [_@Var], Msgs) end,",
        "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue, options = Options },",
        " {'@Trans@', NextState, Data1 }"
    ]),
    Cons1 = Cons ++ [
                      "% @doc Sends a message to the #statem_data.coparty_id given that the message label that is valid for this state."
                    ],
    % if
    %     length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            );
    %     true ->
    %         Clause
    % end;
clause(recv, Act, Var, Trans, NextState, Cons) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(info, {CoPartyID, {'@Act@', _@Var}}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled, persistent_queue = PersistentQueue } = Options } = _Data) ->",
        "printout(\"~p, recv (~p) from ~p.\", [_@Func, {'@Act@', _@Var}, CoPartyID]),",
        "NextState = '@NextState@',",
        "case maps:find('@Act@', Msgs) of ",
        " {ok, StateMsgs} -> Msgs1 = maps:put('@Act@', [_@Var] ++ StateMsgs, Msgs);",
        " error -> Msgs1 = maps:put('@Act@', [_@Var], Msgs) end,",
        "case PersistentQueue of false -> Queue1 = []; true -> Queue1 = Queue end,",
        "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue1, options = Options },",
        " {'@Trans@', NextState, Data1 }"
    ]),
    Cons1 = Cons ++ [
                      "% @doc `Urgently' receives messages from the processes mailbox.",
                      "% Only the expected messages may be received from the current state -- the rest will be `postponed' and received at a later state.",
                      "% Note: unexpected messages can be perpetually postponed this way."
                    ],
    % if
        % length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            );
    %     true ->
    %         Clause
    % end;
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
clause(recv_msg, Cons) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "({call, From}, {recv, Label}, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options } = _Data) ->",
        "printout(\"~p, looking for msg with label (~p).\", [_@Func, Label]),",
        "case maps:find(Label, Msgs) of",
        "{ok, [H]} -> ",
        "printout(\"~p, found msg (~p: ~p) out of 1.\", [_@Func, Label, H]),",
        "ReplyMsg = {ok, #{ label => Label, msg => H, total => 1, tail => [] }};",
        "{ok, [H|T]} -> ",
        "NumMsgs = length(T)+1,",
        "printout(\"~p, found msg (~p: ~p) out of ~p.\", [_@Func, Label, H, NumMsgs]),",
        "ReplyMsg = {ok, #{ label => Label, msg => H, total => NumMsgs, tail => T }};",
        "error -> ",
        "printout(\"~p, no msgs with label (~p) found.\", [_@Func, Label]), ",
        "ReplyMsg = {error, no_msg_found_under_label}",
        "end,",
        " { keep_state_and_data, [{reply, From, ReplyMsg}] }"
    ]),
    Cons1 = Cons ++ [
                      "% @doc Allows messages to be fetched once they have been `urgently received'.",
                      "% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label."
                    ],
    % if
    %     length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            );
    %     true ->
    %         Clause
    % end;
clause(postpone_send, Cons) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(cast, {send, {Label, Msg}}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled, persistent_queue = _PersistentQueue } = Options } = _Data) ->",
        "printout(\"~p, early send -- add to queue: ~p.\", [_@Func, {Label, Msg}]),",
        "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options },",
        " {keep_state, Data1 }"
    ]),
    Cons1 = Cons ++ [
                      "% @doc Handle trying to perform send action from wrong state.", 
                      "% Adds not-enabled sending actions to queue, to be processed at the next sending-state.",
                      "% Allows sending actions to be sequentially `queued'.",
                      "% Invalid send actions will cause error when later processed.",
                      "% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise."
                    ],
    % if
    %     length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            );
    %     true ->
    %         Clause
    % end;
clause(postpone_recv, Cons) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(info, {CoPartyID, {Label, Msg}}, #statem_data{ coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled, persistent_queue = _PersistentQueue } = _Options } = _Data) ->",
        "printout(\"~p, early recv (~p) -- postponing.\", [_@Func, {Label, Msg}]),",
        % "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options },",
        " {keep_state_and_data, [postpone] }"
    ]),
    Cons1 = Cons ++ [
                      "% @doc Handle messages arriving in mailbox before state-change.", 
                      "% Postpone receiving the message until next state.",
                      "% Note: unexpected messages can be perpetually postponed this way."
                  ],
    % if
    %     length(Cons1) > 0 ->
            erl_syntax:add_precomments(
                lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons1), Clause
            ).
    %     true ->
    %         Clause
    % end.

fatal_timeout_clause() ->
  Clause = ?Q([
      "(state_timeout, go_to_fatal_timeout, Data) ->
        {next_state, custom_end_state, Data}"
  ]),
  Clause.
  % Cons = [
  %           "% @doc Fatal timeout state."
  %       ],
  % erl_syntax:add_precomments(
  %     lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
  % ).
queue_clause() ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(state_timeout, process_queue, #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H|T], options = Options } = _Data) ->",
        "printout(\"(->) ~p, queued action: ~p.\", [_@Func, H]),",
        "Data1 = #statem_data{ coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options },",
        "{repeat_state, Data1, [{next_event, cast, {send, H}}]}"
    ]),

    Cons = [
            "% @doc Process queued (sending) actions.",
            "% Note: unspecified sending actions may be perpetually queued this way -- without stalling progress, as they are only processed once per-state (on entry)."
          ],

    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ).
    % erl_syntax:add_precomments(
    %     erl_syntax:comment(list_to_atom("% process queued actions")), Clause
    % ).
    % Clause.

timeout_clause(TimeoutState, TimeoutDuration, Cons) ->
    timeout_clause(TimeoutState, TimeoutDuration, Cons, true).
timeout_clause(TimeoutState, TimeoutDuration, Cons, ShowIO) ->
    % timeout_clause(Cons) ->
    % Clause = ?Q(["(state_timeout, TimeoutState, Data) ->", " {next_state, TimeoutState, Data}"]),
    % PrintStr = io_lib:format("io:format(\"(timeout[~p] -> ~p.)\n\")", [TimeoutDuration,TimeoutState]),
    if
        ShowIO == true ->
            Func = get_merl_func_name(),
            Clause = ?Q([
                "(state_timeout, '@TimeoutState@', Data) ->",
                "printout(\"~p, (timeout[~p] -> ~p.)\n\",[_@Func, '@TimeoutDuration@','@TimeoutState@']),",
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
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options } = _Data) -> keep_state_and_data"
    ]),

    Cons = [
              "% @doc Enter new state. (basic)"
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    );
enter_clause(queues) ->
    [enter_clause(queue_process), enter_clause(queue_empty)];
enter_clause(queue_process) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options } = _Data) ->",
        "printout(\"(->) ~p, queued actions.\", [_@Func]),",
        " {keep_state_and_data, [{state_timeout, 0, process_queue}]}"
    ]),

    Cons = [
              "% @doc Enter new state with queued actions, and immediately begin processesing them."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    );
enter_clause(queue_empty) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options } = _Data) ->",
        "printout(\"(->) ~p.\", [_@Func]),",
        " keep_state_and_data"
    ]),
    Cons = [
              "% @doc Enter new state with no queued actions."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    );
enter_clause(state) ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options } = _Data) ->",
        "printout(\"(->) ~p.\", [_@Func]),",
        %  "io:format(\"(~p ->.)\n\",['@State@']),",
        " keep_state_and_data"
    ]),
    Cons = [
              "% @doc Enter new state."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    );
enter_clause(stop) ->
    Clause = ?Q([
        "(enter, _OldState, #stop_data{ reason = _Reason, statem_data = _StatemData } = _Data) ->",
        " {keep_state_and_data, [{state_timeout, 0, go_to_terminate}]}"
    ]),
    Cons = [
              "% @doc Stop state."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    );
enter_clause(fatal_timeout) ->
    Clause = ?Q([
        "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options } = Data) ->",
        " {keep_state, #stop_data{reason = error_fatal_timeout, statem_data = Data}, [{state_timeout, 0, go_to_fatal_timeout}]}"
    ]),
    Cons = [
              "% @doc Fatal timeout state -- trigger supervisor to restart protocol."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ).
enter_clause(timeout_recv, Timeout, ToState) ->
    Func = get_merl_func_name(),
    % [
        % enter_clause(queue_process),
        Clause = ?Q([
            "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options } = _Data) ->",
            "printout(\"(~p ->.)\",[_@Func]),",
            "{keep_state_and_data, [{state_timeout, '@Timeout@', '@ToState@'}]}"
        ]),
        Cons = [
                  "% @doc Enter new receiving state that has a timeout."
              ],
        Clause1 = erl_syntax:add_precomments(
            lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
        ),
        [Clause1];
    % ];
enter_clause(timeout_send, Timeout, ToState) ->
    Func = get_merl_func_name(),
    % [
        Clause2 = enter_clause(queue_process),
        Clause = ?Q([
            "(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options } = _Data) ->",
            "printout(\"(~p ->.)\",[_@Func]),",
            "{keep_state_and_data, [{state_timeout, '@Timeout@', '@ToState@'}]}"
        ]),

    Cons = [
              "% @doc Enter new sending state that has a timeout."
          ],
    Clause1 = erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ),
    [Clause2, Clause1].
    % ].
% enter_clause(Timeout,State) -> ?Q(["(enter, _OldState, Data) ->", "{keep_state, Data, [{state_timeout, ", Timeout, ", ", State, "}]}"]).
% enter_clause(Timeout,State) -> ?Q(["(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout,1000,init}]}"]).
custom_stop_clause() ->
    Func = get_merl_func_name(),
    Clause = ?Q([
        "(state_timeout, go_to_terminate, #stop_data{ reason = Reason, statem_data = StatemData } = Data) ->",
        "printout(\"(->) ~p,\nReason: ~p,\nStatemData: \n\t~p.\", [_@Func, Reason, StatemData]),",
        " {stop, Reason, Data}"
    ]),
    
    Cons = [
              "% @doc Custom stopping-state to allow for better debugging."
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ).
stop_clause() ->
    Func = get_merl_func_name(),
    Name = get_merl_name_name(),
    Clause = ?Q([
        "() -> printout(\"~p.\", [_@Func]), gen_statem:stop(_@Name)"
    ]),
    Cons = [
              "% @doc Stop state. (original)"
          ],
    erl_syntax:add_precomments(
        lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Cons), Clause
    ).

%% @doc generates standard states, i.e. act x
std_state(Id, [Edge], Nodes) ->
    {NextState, Trans} = get_next_state_and_trans(
        maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
    ),
    {Act, Var} = Edge#edge.edge_data#edge_data.event,
    Event = Edge#edge.edge_data#edge_data.event_type,
    Cons = Edge#edge.edge_data#edge_data.comments,

    Dir = Edge#edge.edge_data#edge_data.trans_type,

    Comms = lists:map(
        fun({A, B}) ->
            atom_to_list(A) ++ " " ++
                atom_to_list(B)
        end,
        Cons
    ),

    io:format("\n\ndir: ~p.\n", [Dir]),
    io:format("act: ~p.\n", [Act]),

    if
        (Dir == recv) or (Dir == send) ->
            case Dir of
                recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                send -> Label = string:prefix(atom_to_list(Act), "send_");
                _ -> Label = atom_to_list(Act)
            end,

            io:format("label: ~p.\n", [Label]),


            Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
        true ->
            Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    end,

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Name = list_to_atom("std_state" ++ integer_to_list(Id)),
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_std"),
    % {true, Name, [enter_clause(), Clause]}.
    {true, Name, enter_clause(queues) ++ [queue_clause(), Clause] ++ AuxClauses}.

%% @doc generates choice states, i.e. branch
choice_state(Id, Edges, Nodes) ->
    Fun = fun(Edge) ->
        {NextState, Trans} = get_next_state_and_trans(
            maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
        ),
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)

        Dir = Edge#edge.edge_data#edge_data.trans_type,

        Comms = lists:map(
            fun({A, B}) ->
                atom_to_list(A) ++ " " ++
                    atom_to_list(B)
            end,
            Cons
        ),

        if
            (Dir == recv) or (Dir == send) ->
                case Dir of
                    recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                    send -> Label = string:prefix(atom_to_list(Act), "send_");
                    _ -> Label = atom_to_list(Act)
                end,
    
                io:format("label: ~p.\n", [Label]),
    
    
                Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
            true ->
                Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        end,
    
        Clause
    end,

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Name = list_to_atom("choice_state" ++ integer_to_list(Id)),
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_choice"),
    % Clauses = [enter_clause(state, Name)] ++ lists:map(Fun, Edges),
    Clauses = enter_clause(queues) ++ [queue_clause()] ++ lists:map(Fun, Edges) ++ AuxClauses,
    {true, Name, Clauses}.

recv_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_recv_after"),
    % Name = list_to_atom("recv_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    Q_NextState = get_next_state(
        maps:get(TimeoutEdge#edge.to, Nodes), integer_to_list(TimeoutEdge#edge.to)
    ),

    EnterClause = enter_clause(timeout_recv, Timeout, Q_NextState),
    % ProcessQueueClauses = enter_clause(queues),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    %% same as branch, but should only really be one other edge
    Fun = fun(Edge) ->
        {NextState, Trans} = get_next_state_and_trans(
            maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
        ),
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        Dir = Edge#edge.edge_data#edge_data.trans_type,

        Comms = lists:map(
            fun({A, B}) ->
                atom_to_list(A) ++ " " ++
                    atom_to_list(B)
            end,
            Cons
        ),

        if
            (Dir == recv) or (Dir == send) ->
                case Dir of
                    recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                    send -> Label = string:prefix(atom_to_list(Act), "send_");
                    _ -> Label = atom_to_list(Act)
                end,
    
                io:format("label: ~p.\n", [Label]),
    
    
                Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
            true ->
                Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        end,
    
        Clause
    end,

    %% remove silent edge from edges
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    Edges2 = lists:map(Fun, Edges1),

    % %% create default delay
    % DelayClause = ?Q([""]),

    % EnterClause1 = erl_syntax:add_postcomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, ["default delay: ('@Timeout@')"]), EnterClause),

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    % Clauses = [EnterClause] ++ ProcessQueueClauses ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ Edges2 ++ [Q_Clause] ++ AuxClauses,
    {true, Name, Clauses}.

branch_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_branch_after"),
    % Name = list_to_atom("branch_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    Q_NextState = get_next_state(
        maps:get(TimeoutEdge#edge.to, Nodes), integer_to_list(TimeoutEdge#edge.to)
    ),

    EnterClause = enter_clause(timeout_recv, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    Fun = fun(Edge) ->
        {NextState, Trans} = get_next_state_and_trans(
            maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
        ),
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        Dir = Edge#edge.edge_data#edge_data.trans_type,

        Comms = lists:map(
            fun({A, B}) ->
                atom_to_list(A) ++ " " ++
                    atom_to_list(B)
            end,
            Cons
        ),

        if
            (Dir == recv) or (Dir == send) ->
                case Dir of
                    recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                    send -> Label = string:prefix(atom_to_list(Act), "send_");
                    _ -> Label = atom_to_list(Act)
                end,
    
                io:format("label: ~p.\n", [Label]),
    
    
                Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
            true ->
                Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        end,
    
        Clause
    % end
    end,

    %% remove silent edge from edges
    % io:format("\n[branch_after_state] Edges: ~p.\n",[Edges]),
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    % io:format("\n[branch_after_state] Edges1: ~p.\n",[Edges1]),
    Edges2 = lists:map(Fun, Edges1),

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ Edges2 ++ [Q_Clause] ++ AuxClauses,
    % Clauses = [EnterClause] ++ Edges,
    {true, Name, Clauses}.

send_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_send_after"),
    % Name = list_to_atom("send_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    Q_NextState = get_next_state(
        maps:get(TimeoutEdge#edge.to, Nodes), integer_to_list(TimeoutEdge#edge.to)
    ),


    EnterClause = enter_clause(timeout_send, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    %% same as branch, but should only really be one other edge
    Fun = fun(Edge) ->
        {NextState, Trans} = get_next_state_and_trans(
            maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
        ),
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        Dir = Edge#edge.edge_data#edge_data.trans_type,

        Comms = lists:map(
            fun({A, B}) ->
                atom_to_list(A) ++ " " ++
                    atom_to_list(B)
            end,
            Cons
        ),

        if
            (Dir == recv) or (Dir == send) ->
                case Dir of
                    recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                    send -> Label = string:prefix(atom_to_list(Act), "send_");
                    _ -> Label = atom_to_list(Act)
                end,
    
                io:format("label: ~p.\n", [Label]),
    
    
                Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
            true ->
                Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        end,
    
        Clause
    end,

    %% remove silent edge from edges
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    Edges2 = lists:map(Fun, Edges1),

    % %% create default delay
    % DelayClause = ?Q([""]),

    % EnterClause1 = erl_syntax:add_postcomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, ["default delay: ('@Timeout@')"]), EnterClause),

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause] ++ AuxClauses,
    {true, Name, Clauses}.

select_after_state(Id, Edges, Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_select_after"),
    % Name = list_to_atom("select_after_state" ++ integer_to_list(Id)),

    TimeoutEdge = lists:last(lists:filter(fun(Elem) -> Elem#edge.is_silent end, Edges)),
    Timeout = TimeoutEdge#edge.edge_data#edge_data.timeout,

    Q_NextState = get_next_state_and_trans(
        maps:get(TimeoutEdge#edge.to, Nodes), integer_to_list(TimeoutEdge#edge.to)
    ),

    EnterClause = enter_clause(timeout_send, Timeout, Q_NextState),
    Q_Cons = TimeoutEdge#edge.edge_data#edge_data.comments,

    Q_Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Q_Cons),
    Q_Clause = timeout_clause(Q_NextState, Timeout, Q_Comms),

    Fun = fun(Edge) ->
        {NextState, Trans} = get_next_state_and_trans(
            maps:get(Edge#edge.to, Nodes), integer_to_list(Edge#edge.to)
        ),
        {Act, Var} = Edge#edge.edge_data#edge_data.event,
        Event = Edge#edge.edge_data#edge_data.event_type,
        Cons = Edge#edge.edge_data#edge_data.comments,
        Comms = lists:map(fun({A, B}) -> atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
        % clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        Dir = Edge#edge.edge_data#edge_data.trans_type,

        Comms = lists:map(
            fun({A, B}) ->
                atom_to_list(A) ++ " " ++
                    atom_to_list(B)
            end,
            Cons
        ),

        if
            (Dir == recv) or (Dir == send) ->
                case Dir of
                    recv -> Label = string:prefix(atom_to_list(Act), "receive_");
                    send -> Label = string:prefix(atom_to_list(Act), "send_");
                    _ -> Label = atom_to_list(Act)
                end,
    
                io:format("label: ~p.\n", [Label]),
    
    
                Clause = clause(Dir, list_to_atom(Label), merl:var(Var), Trans, NextState, Comms);
            true ->
                Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
        end,
    
        Clause
    % end
    end,

    %% remove silent edge from edges
    % io:format("\n[select_after_state] Edges: ~p.\n",[Edges]),
    Edges1 = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, Edges),
    % io:format("\n[select_after_state] Edges1: ~p.\n",[Edges1]),
    Edges2 = lists:map(Fun, Edges1),

    AuxClauses = [clause(recv_msg, []), clause(postpone_send, []), clause(postpone_recv, [])],

    % Clauses = [EnterClause] ++ Edges2 ++ [Q_Clause],
    Clauses = EnterClause ++ [queue_clause()] ++ Edges2 ++ [Q_Clause] ++ AuxClauses,
    % Clauses = [EnterClause] ++ Edges,
    {true, Name, Clauses}.
    
  
fatal_timeout_state(Id, _Edges, _Nodes) ->
    Name = list_to_atom("state" ++ integer_to_list(Id) ++ "_fatal_timeout"),
    Clauses = [enter_clause(fatal_timeout), fatal_timeout_clause()],
    {true, Name, Clauses}.

%% @doc calls the appropriate function for choice and standard states
state_funs(K, V, Edges, Nodes) ->
    % io:format("\n[state_funs] K: ~p,\n\tV: ~p\n\tEdges: ~p\n\tNodes: ~p.\n", [K,V,Edges,Nodes]),
    Pred = fun(Edge) -> Edge#edge.from =:= K end,
    Es = lists:filter(Pred, Edges),
    case V of
      fatal_timeout_state -> fatal_timeout_state(K, Es, Nodes);
        init_state ->
            % init_state(K, Es, Nodes);
            init_setup_state(K, Es, Nodes);
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
    if
        Edge#edge.is_delayable_send ->
            Timeout = Data#edge_data.timeout,
            Clause = ?Q([
                "(_@Var1) -> TimeDelay = rand:uniform('@Timeout@' * 2), timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}), receive {delay_stop, TimeDelay, _MoreData} -> io:format(\"[send_accept]: delay(~p) stopped.\n\", [TimeDelay]) end, gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
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
            Clause = ?Q(["(_@Var1) -> gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"]),
            Clauses = [Clause]
    end,
    
    % {true, Act, [Clause]}.
    {true, Act, Clauses}.

gen_module(FileName, P) ->
    Server = merl:var(list_to_atom("?SERVER")),
    Module = merl:var(list_to_atom("?MODULE")),
    Func = merl:var(list_to_atom("?FUNCTION_NAME")),

    % Start = ?Q(["() -> ", "gen_statem:start_link({local, _@Server}, _@Module, [], []) "]),
    StartP = ?Q(["(Params) -> ", "gen_statem:start_link({local, _@Server}, _@Module, Params, []) "]),
    Start = ?Q(["() -> ", "_@Module:start_link([]) "]),

    Send = ?Q([
        "(Label, Msg) -> ",
        "printout(\"~p: {~p, ~p}.\", [_@Func, Label, Msg]),",
        "gen_statem:cast(_@Module, {send, {Label, Msg}})"
    ]),

    Recv = ?Q([
        "(Label) -> ",
        "printout(\"~p: ~p.\", [_@Func, Label]),",
        "gen_statem:call(_@Module, {recv, Label})"
    ]),

    Tout = ?Q([
        "(Label, Msg) -> ",
        "printout(\"~p: {~p, ~p}.\", [_@Func, Label, Msg]),",
        "gen_statem:cast(_@Module, {issue_timeout, {Msg, Label}})"
    ]),

    % SetupFun =

    % Cb = merl:quote(["() -> ", "[state_functions, state_enter]" ]),
    Cb = merl:quote(["() -> ", "[state_functions, state_enter]"]),

    Stop = merl:quote(["() -> ", "gen_statem:stop(_@Server)"]),

    % Init = merl:quote(["([]) ->
    %              {ok, state1, {}}
    %           "]),

    {Edges, Nodes} = build_fsm:to_fsm(P),

    %     ok.

    % blah(FileName, P, Nodes, Edges, Start, Stop, Cb, Server, Module) ->
    io:format("\n------ FSM:\nNodes: ~p\nEdges: ~p\n------\n", [Nodes, Edges]),

    NonEndEdges = lists:filter(fun(Elem) -> not Elem#edge.is_custom_end end, Edges),

    io:format("\n------ FSM (filtered NonEndEdges): ~p\n------\n", [NonEndEdges]),

    % [InitFun|StateFuns]
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

  %   % [InitFun|StateFuns]
  % NameID = 0,
  % StateFuns = maps:fold(
  %     fun(K, V, AccIn) ->
  %         NameID=NameID+1,
  %         % io:format("\n[StateFuns] (~p, ~p):...\n\tAccIn: ~p.\n",[K,V,AccIn]),
  %         {true, Name, Clauses} = state_funs(K, V, Edges, Nodes),
  %         Name1 = list_to_atom("state" ++ integer_to_list(NameID) ++ "_" ++ Name),
  %         % io:format("\n\tStateFun: ~p.\n",[StateFun]),
  %         AccIn ++ [{true, Name1, Clauses}]
  %         % AccIn ++ [StateFun]
  %     end,
  %     [],
  %     Nodes
  % ),
    % [InitFun|StateFuns1] = StateFuns,
    SetupFun = hd(StateFuns),
    StateFuns1 = lists:nthtail(1, StateFuns),

    NonSilentEdges = lists:filter(fun(Elem) -> not Elem#edge.is_silent end, NonEndEdges),
    % io:format("\n------ FSM (filtered NonSilentEdges): ~p\n------\n", [NonSilentEdges]),

    ActEdges = lists:filter(fun(Elem) -> Elem#edge.edge_data#edge_data.trans_type == action end, lists:delete(hd(NonSilentEdges), NonSilentEdges)),

    CBFuns = lists:foldl(
        fun(Edge, AccIn) ->
            % AccIn ++ [cb_fun(Edge#edge.edge_data, Server)]
            AccIn ++ [cb_fun(Edge, Server)]
        end,
        [],
        ActEdges
    ),

    % io:format("--- success: CBFuns: ~p\n-----\n", [CBFuns]),

    % InitFunNoDataCons = [ "% @doc Parse init params." ],

    % InitFunNoData = erl_syntax:add_precomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, InitFunNoDataCons), init_state(no_data)),

    % InitFunWithDataCons = [ "% @doc Parse init params." ],

    % InitFunWithData = erl_syntax:add_precomments(lists:map(fun(Com) -> erl_syntax:comment([Com]) end, InitFunWithDataCons), init_state(with_data)),



    Fs =
        [
            {true, start_link, [StartP]},
            {true, start_link, [Start]},
            {true, callback_mode, [Cb]},
            {true, send, [Send]},
            {true, recv, [Recv]},
            {true, issue_timeout, [Tout]},
            {true, init, init_state(no_data)},
            {true, init, init_state(with_data)},
            SetupFun
            % {true, init, [Init]}
            | StateFuns1
        ] ++ lists:usort(CBFuns) ++ [{true, stop, [Stop]}],

    % io:format("--- success: Fs: ~p\n------\n", [Fs]),

    Forms = merl_build:add_attribute(
        behaviour, [merl:term('gen_statem')], merl_build:init_module(FileName)
    ),

    Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),

    Forms2 = merl_build:add_record(
        statem_options,
        [
            {'allow_delayable_sends', merl:term('false')},
            {'printout_enabled', merl:term('true')},
            {'persistent_queue', merl:term('false')}
        ],
        Forms1
    ),

    Forms3 = merl_build:add_record(
        statem_data,
        [
            {'coparty_id', merl:term('undefined')},
            {'state_stack', merl:var('[]')},
            {'msgs', merl:var('#{}')},
            {'queued_actions', merl:var('[]')},
            {'options', merl:var('#statem_options{}')}
        ],
        Forms2
    ),

    Forms4 = merl_build:add_record(
        stop_data,
        [
            {'reason', merl:term('undefined')},
            {'statem_data', merl:var('#statem_data{}')}
        ],
        Forms3
    ),

    Forms5 = merl_build:add_function(
        false,
        printout,
        [
            ?Q([
                "(Str, Params) -> io:format(\"[~p|~p]: \" ++ Str ++ \"\n\", [_@Module, self()] ++ Params)"
            ])
        ],
        Forms4
    ),

    io:format("--- success: Forms.\n"),

    merl_build:module_forms(
        lists:foldl(
            fun({X, Name, Cs}, S) ->
                merl_build:add_function(X, Name, Cs, S)
            end,
            Forms5,
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
    % io:format("\n------ Program:\n~s\n------\n", [Program]),
    file:write_file(string:concat("tool_output/", FileName), Program).
