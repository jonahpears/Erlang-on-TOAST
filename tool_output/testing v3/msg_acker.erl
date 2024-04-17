-module('msg_acker').

-file("msg_acker", 1).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         custom_end_state/3,
         init/1,
         init/2,
         init_setup_state/3,
         issue_timeout/2,
         recv/1,
         send/2,
         start_link/0,
         start_link/1,
         state1_std/3,
         state2_send_after/3,
         state4_std/3,
         state5_send_after/3,
         state7_std/3,
         stop/0,
         terminate/3]).

-record(statem_options, {allow_delayable_sends = false, printout_enabled = true, persistent_queue = false}).

-record(statem_data, {coparty_id = undefined, state_stack = [], msgs = #{}, queued_actions = [], options = #statem_options{}}).

-record(stop_data, {reason = undefined, statem_data = #statem_data{}}).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

start_link(Params) -> gen_statem:start_link({local, ?SERVER}, ?MODULE, Params, []).

start_link() -> ?MODULE:start_link([]).

callback_mode() -> [state_functions, state_enter].

send(Label, Msg) ->
    printout("~p: {~p, ~p}.", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?MODULE, {send, {Label, Msg}}).

recv(Label) ->
    printout("~p: ~p.", [?FUNCTION_NAME, Label]),
    gen_statem:call(?MODULE, {recv, Label}).

issue_timeout(Label, Msg) ->
    printout("~p: {~p, ~p}.", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?MODULE, {issue_timeout, {Msg, Label}}).

%% @doc Parse init params.
init([{HKey, HVal} | T]) when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
init([_H | T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

%% @doc Parse init params (with #statem_data already created).
init([{HKey, HVal} | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = Options} = Data)
    when is_atom(HKey) and is_map_key(HKey, Options) ->
    init(T, maps:put(options, maps:put(HKey, HVal, Options), Data));
init([_H | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> init(T, Data);
init([_H | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> init(T, Data);
init([], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) ->
    {ok, init_setup_state, Data}.

%% @doc Waits to receive the process-ID of the `coparty' before continuing to the initial state of the FSM.
%% Note: In the binary setting, sessions are only composed of two participants.
init_setup_state(enter, _OldState,
                 #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    {keep_state_and_data, [{state_timeout, 0, wait_to_finish_setup}]};
init_setup_state(state_timeout, wait_to_finish_setup,
                 #statem_data{coparty_id = _CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options} = _Data) ->
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupervisorID, sup_init, CoPartyID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME, CoPartyID]),
            {next_state, state1_std, #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options}}
    end.

state1_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state1_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state1_std(state_timeout, process_queue,
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%% @doc `Urgently' receives messages from the processes mailbox.
%% Only the expected messages may be received from the current state -- the rest will be `postponed' and received at a later state.
%% Note: unexpected messages can be perpetually postponed this way.
state1_std(info, {CoPartyID, {msg1, Msg1}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, recv (~p) from ~p.", [?FUNCTION_NAME, {msg1, Msg1}, CoPartyID]),
    NextState = state2_send_after,
    case maps:find(msg1, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(msg1, [Msg1] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(msg1, [Msg1], Msgs)
    end,
    case PersistentQueue of
        false -> Queue1 = [];
        true -> Queue1 = Queue
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue1, options = Options},
    {next_state, NextState, Data1};
%% @doc Allows messages to be fetched once they have been `urgently received'.
%% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label.
state1_std({call, From}, {recv, Label},
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
%% @doc Handle trying to perform send action from wrong state.
%% Adds not-enabled sending actions to queue, to be processed at the next sending-state.
%% Allows sending actions to be sequentially `queued'.
%% Invalid send actions will cause error when later processed.
%% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise.
state1_std(cast, {send, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
%% @doc Handle messages arriving in mailbox before state-change.
%% Postpone receiving the message until next state.
%% Note: unexpected messages can be perpetually postponed this way.
state1_std(info, {CoPartyID, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                _Options} =
               _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

state2_send_after(enter, _OldState,
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state2_send_after(enter, _OldState,
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(~p ->.)", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 3000, state4_std}]};
state2_send_after(state_timeout, process_queue,
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%% @doc Sends a message to the #statem_data.coparty_id given that the message label that is valid for this state.
state2_send_after(cast, {send, {ack1, Ack1}},
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       Options} =
                      _Data) ->
    CoPartyID ! {self(), {ack1, Ack1}},
    printout("~p, send (~p) to ~p.", [?FUNCTION_NAME, {ack1, Ack1}, CoPartyID]),
    NextState = state1_std,
    case maps:find(ack1, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(ack1, [Ack1] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(ack1, [Ack1], Msgs)
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue, options = Options},
    {next_state, NextState, Data1};
%% This is a timeout branch:
state2_send_after(state_timeout, state4_std, Data) ->
    printout("~p, (timeout[~p] -> ~p.)\n", [?FUNCTION_NAME, 3000, state4_std]),
    {next_state, state4_std, Data};
%% @doc Allows messages to be fetched once they have been `urgently received'.
%% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label.
state2_send_after({call, From}, {recv, Label},
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
%% @doc Handle trying to perform send action from wrong state.
%% Adds not-enabled sending actions to queue, to be processed at the next sending-state.
%% Allows sending actions to be sequentially `queued'.
%% Invalid send actions will cause error when later processed.
%% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise.
state2_send_after(cast, {send, {Label, Msg}},
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       Options} =
                      _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
%% @doc Handle messages arriving in mailbox before state-change.
%% Postpone receiving the message until next state.
%% Note: unexpected messages can be perpetually postponed this way.
state2_send_after(info, {CoPartyID, {Label, Msg}},
                  #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       _Options} =
                      _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

state4_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state4_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state4_std(state_timeout, process_queue,
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%% @doc `Urgently' receives messages from the processes mailbox.
%% Only the expected messages may be received from the current state -- the rest will be `postponed' and received at a later state.
%% Note: unexpected messages can be perpetually postponed this way.
state4_std(info, {CoPartyID, {msg2, Msg2}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, recv (~p) from ~p.", [?FUNCTION_NAME, {msg2, Msg2}, CoPartyID]),
    NextState = state5_send_after,
    case maps:find(msg2, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(msg2, [Msg2] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(msg2, [Msg2], Msgs)
    end,
    case PersistentQueue of
        false -> Queue1 = [];
        true -> Queue1 = Queue
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue1, options = Options},
    {next_state, NextState, Data1};
%% @doc Allows messages to be fetched once they have been `urgently received'.
%% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label.
state4_std({call, From}, {recv, Label},
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
%% @doc Handle trying to perform send action from wrong state.
%% Adds not-enabled sending actions to queue, to be processed at the next sending-state.
%% Allows sending actions to be sequentially `queued'.
%% Invalid send actions will cause error when later processed.
%% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise.
state4_std(cast, {send, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
%% @doc Handle messages arriving in mailbox before state-change.
%% Postpone receiving the message until next state.
%% Note: unexpected messages can be perpetually postponed this way.
state4_std(info, {CoPartyID, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                _Options} =
               _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

state5_send_after(enter, _OldState,
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state5_send_after(enter, _OldState,
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(~p ->.)", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 3000, state7_std}]};
state5_send_after(state_timeout, process_queue,
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%% @doc Sends a message to the #statem_data.coparty_id given that the message label that is valid for this state.
state5_send_after(cast, {send, {ack2, Ack2}},
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       Options} =
                      _Data) ->
    CoPartyID ! {self(), {ack2, Ack2}},
    printout("~p, send (~p) to ~p.", [?FUNCTION_NAME, {ack2, Ack2}, CoPartyID]),
    NextState = state2_send_after,
    case maps:find(ack2, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(ack2, [Ack2] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(ack2, [Ack2], Msgs)
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue, options = Options},
    {next_state, NextState, Data1};
%% This is a timeout branch:
state5_send_after(state_timeout, state7_std, Data) ->
    printout("~p, (timeout[~p] -> ~p.)\n", [?FUNCTION_NAME, 3000, state7_std]),
    {next_state, state7_std, Data};
%% @doc Allows messages to be fetched once they have been `urgently received'.
%% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label.
state5_send_after({call, From}, {recv, Label},
                  #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
%% @doc Handle trying to perform send action from wrong state.
%% Adds not-enabled sending actions to queue, to be processed at the next sending-state.
%% Allows sending actions to be sequentially `queued'.
%% Invalid send actions will cause error when later processed.
%% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise.
state5_send_after(cast, {send, {Label, Msg}},
                  #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       Options} =
                      _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
%% @doc Handle messages arriving in mailbox before state-change.
%% Postpone receiving the message until next state.
%% Note: unexpected messages can be perpetually postponed this way.
state5_send_after(info, {CoPartyID, {Label, Msg}},
                  #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                               options =
                                   #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                                   persistent_queue = _PersistentQueue} =
                                       _Options} =
                      _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

state7_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state7_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state7_std(state_timeout, process_queue,
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%% @doc `Urgently' receives messages from the processes mailbox.
%% Only the expected messages may be received from the current state -- the rest will be `postponed' and received at a later state.
%% Note: unexpected messages can be perpetually postponed this way.
state7_std(info, {CoPartyID, {tout, Tout}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, recv (~p) from ~p.", [?FUNCTION_NAME, {tout, Tout}, CoPartyID]),
    NextState = custom_end_state,
    case maps:find(tout, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(tout, [Tout] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(tout, [Tout], Msgs)
    end,
    case PersistentQueue of
        false -> Queue1 = [];
        true -> Queue1 = Queue
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue1, options = Options},
    {next_state, NextState, Data1};
%% @doc Allows messages to be fetched once they have been `urgently received'.
%% Messages received from the mailbox are added to #statem_data.msgs with each message is stored in a list under their respective label.
state7_std({call, From}, {recv, Label},
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
%% @doc Handle trying to perform send action from wrong state.
%% Adds not-enabled sending actions to queue, to be processed at the next sending-state.
%% Allows sending actions to be sequentially `queued'.
%% Invalid send actions will cause error when later processed.
%% If #statem_option.persistent_queue == false then queue is emptied whenever a message is received -- useful for queueing up `default' timeout actions if no message is received, while allowing you to respond if otherwise.
state7_std(cast, {send, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
%% @doc Handle messages arriving in mailbox before state-change.
%% Postpone receiving the message until next state.
%% Note: unexpected messages can be perpetually postponed this way.
state7_std(info, {CoPartyID, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                _Options} =
               _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

custom_end_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = _Data) ->
    {keep_state_and_data, [{state_timeut, 0, go_to_terminate}]};
custom_end_state(state_timeout, go_to_terminate, #stop_data{reason = Reason, statem_data = StatemData} = Data) ->
    printout("(->) ~p,\nReason: ~p,\nStatemData: \n\t~p.", [?FUNCTION_NAME, Reason, StatemData]),
    {stop, Reason, Data}.

terminate(Reason, State, Data) ->
    printout("(->) ~p, ~p, Reason: ~p,\nData: \n\t~p.", [?FUNCTION_NAME, State, Reason, Data]),
    ok.

stop() -> gen_statem:stop(?SERVER).