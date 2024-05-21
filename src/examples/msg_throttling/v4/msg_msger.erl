-module(msg_msger).

-behaviour(gen_statem).

-include("data_records.hrl").

%% gen_statem
-export([ start_link/0,
          callback_mode/0,
          init/1,
          stop/0,
          terminate/3 ]).

%% custom wrappers for gen_statem
-export([ start_link/1, 
          init/2,
          init_setup_state/3,
          stop_state/3 ]).

%% callbacks
-export([ send_msg1/1,
          send_msg2/1,
          recv_ack1/1,
          recv_ack2/1,
          issue_timeout/1,
          handle_event/4 ]).

% %% states
% -export([ state1_send_msg1/3, 
%           state2a_recv_ack1/3,
%           state2b_send_msg2/3,
%           state3a_recv_ack2/3,
%           state3b_issue_timeout/3 ]).

% %% mixed-choice + timeout timing constraints
% -define(ACK1_TIME, 3000).
% -define(ACK2_TIME, 3000).


-define(NAME, ?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

%% gen_statem
start_link(Params) -> 
    % add timeout parameters 
    Params1 = [{timeouts, #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
                             state3a_recv_ack2 => {5000, state3b_issue_timeout} }}] ++ Params,
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params]),
    {ok, Pid} = gen_statem:start_link({local, ?NAME}, ?MODULE, Params1, []),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.

start_link() -> msg_msger:start_link([]).

callback_mode() -> [handle_event_function, state_enter].

-spec init([{atom(),any()}|[]]) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T]) 
    when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
init([_H|T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = Options} = StatemData) 
    when is_atom(HKey) and is_map_key(HKey, Options) -> init(T, maps:put(options, maps:put(HKey, HVal, Options), StatemData));
init([_H|T], #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData) -> init(T, StatemData);
init([], #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData) -> 
    {ok, init_setup_state, StatemData}.


stop() -> 
    printout("~p.", [?FUNCTION_NAME]),
    gen_statem:stop(?NAME).


terminate(Reason, state3b_issue_timeout, StatemData) ->
    printout("~p, timeout: {Reason: ~p},\n\t{State: ~p}", [?FUNCTION_NAME, Reason, StatemData]);
terminate(Reason, State, StatemData) -> 
    printout("~p, {Reason: ~p},\n\t{State: ~p},\n\t{StatemData: ~p}.", [?FUNCTION_NAME,Reason,State,StatemData]),
    ok.


%% custom init/stop wrappers
init_setup_state(enter, _OldState, #statem_data{coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData) -> {keep_state, StatemData, [{state_timeout, 0, wait_to_finish}]};
init_setup_state(state_timeout, wait_to_finish, #statem_data{coparty_id = _CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    % io:format("[~p|~p]: init_setup_state, waiting to finish setup.", [?NAME,self()]),
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupID, sup_init, CoID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME,CoID]),
            {next_state, state1_send_msg1, #statem_data{ coparty_id = CoID, 
                                                         states = States,
                                                         msgs = Msgs,
                                                         timeouts = Timeouts,
                                                         queued_actions = Queue,
                                                         options = Options }}
    end.

stop_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = Data) -> {keep_state, Data, [{state_timeout, 0, exit_deferral}]};
stop_state(state_timeout, exit_deferral, #stop_data{reason = Reason, statem_data = #statem_data{coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData} = Data) -> 
    printout("~p, ~p, StatemData: \n\t~p.", [?FUNCTION_NAME, Reason, StatemData]),
    {stop, Reason, Data}.





%% callback wrappers
-spec cb_send({atom(), any()}) -> ok.
cb_send({Event, Label, Msg}) ->
    printout("~p, (~p: ~p).", [Event, Label, Msg]),
    gen_statem:cast(?NAME, {Event, Label, Msg}).

-spec cb_recv({atom()}) -> ok.
cb_recv({Label}) ->
    printout("~p, (~p).", [?FUNCTION_NAME, Label]),
    gen_statem:call(?NAME, {recv, Label}).

-spec cb_timeout({atom(), any()}) -> ok.
cb_timeout({Event, Label, Msg}) ->
    printout("~p, (~p: ~p).", [Event, Label, Msg]),
    gen_statem:cast(?NAME, {Event, Label, Msg}).




%% event callbacks
send_msg1({Label, Msg}) -> cb_send({?FUNCTION_NAME, Label, Msg}).
send_msg2({Label, Msg}) -> cb_send({?FUNCTION_NAME, Label, Msg}).

issue_timeout({Label, Msg}) -> cb_timeout({?FUNCTION_NAME, Label, Msg}).

recv_ack1({Label}) -> cb_recv({Label}).
recv_ack2({Label}) -> cb_recv({Label}).




%% % % % % % %
%% states enter
%% % % % % % %


%% state enter, issue timeout (stop and cause supervisor to notice)
handle_event(enter, _OldState, state3b_issue_timeout=State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData) ->
    printout("(->) ~p.", [State]),
    {keep_state, #stop_data{reason = error_exceeded_throttling_capacity, statem_data = StatemData}, [{state_timeout, 0, goto_stop}]};
handle_event(state_timeout, goto_stop, state3b_issue_timeout=_State, Data) ->
    {next_state, state_stop, Data};

%% state enter, handle queued actions
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = [H], options = _Options} = _StatemData) ->
    printout("(->) ~p, with (~p) queued actions.", [State, lists:length(H)]),
    {keep_state_and_data, [{state_timeout,0,process_queue}]};
handle_event(state_timeout, process_queue, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = [H|T], options = Options} = _StatemData) ->
    printout("(->) ~p, queued action: ~p.", [State, H]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                queued_actions = T,
                                options = Options },
    {repeat_state, StatemData1, [{next_event, cast, H}]};

%% state enter, mixed choice (and no queued actions)
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = Timeouts, queued_actions = [], options = _Options} = StatemData)
    when is_map_key(State, Timeouts) ->
        {TimeoutDuration, TimeoutState} = maps:get(State, Timeouts),
        {keep_state, StatemData, [{state_timeout, TimeoutDuration, TimeoutState}]};


%% state enter, no queued actions and no mixed-choice
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = [], options = _Options} = _StatemData) ->
    printout("(->) ~p.", [State]),
    keep_state_and_data;



%% % % % % % %
%% sending actions, from correct states
%% % % % % % %
handle_event(cast, {send, msg1=Label, Msg}, state1_send_msg1=State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    CoPartyID ! {self(), Label, Msg},
    NextState = state2a_recv_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, send (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};


handle_event(cast, {send, msg2=Label, Msg}, state2b_send_msg2=State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    CoPartyID ! {self(), Label, Msg},
    NextState = state3a_recv_ack2,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, send (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};



%% % % % % % %
%% receiving actions, from correct states
%% % % % % % %
handle_event(info, {CoPartyID, ack1=Label, Msg}, state2a_recv_ack1=State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    % add message to the front of the queue of messages received by this label
    case maps:find(Label, Msgs) of
        {ok, StateMsgs} -> 
            Msgs1 = maps:put([Msg] ++ StateMsgs, Label, Msgs);
        error -> 
            Msgs1 = maps:put([Msg], Label, Msgs)
    end,
    NextState = state1_send_msg1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs1,
                                timeouts = Timeouts,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, recv (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};


handle_event(info, {CoPartyID, ack2=Label, Msg}, state3a_recv_ack2=State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    % add message to the front of the queue of messages received by this label
    case maps:find(Label, Msgs) of
        {ok, StateMsgs} -> 
            Msgs1 = maps:put([Msg] ++ StateMsgs, Label, Msgs);
        error -> 
            Msgs1 = maps:put([Msg], Label, Msgs)
    end,
    NextState = state2a_recv_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs1,
                                timeouts = Timeouts,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, send (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};



%% % % % % % %
%% sending actions, from wrong states
%% % % % % % %
handle_event(cast, {send, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, wrong state to send (~p: ~p), adding to queue.", [State, Label, Msg]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                queued_actions = Queue ++ [{send, Label, Msg}],
                                options = Options },
    {keep_state, StatemData1};



%% % % % % % %
%% receiving actions, from wrong states
%% % % % % % %
handle_event(info, {CoPartyID, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = _StatemData) ->
    printout("~p, wrong state to recv (~p: ~p), postponing.", [State, Label, Msg]),
    {keep_state_and_data, [postpone]};



%% % % % % % %
%% mixed-choice (timeouts)
%% % % % % % %
handle_event(state_timeout, NextState, state3b_issue_timeout=State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = StatemData) ->
    printout("~p, internal timeout (~p).", [State, NextState]),
    {next_state, NextState, StatemData};



%% % % % % % %
%% retreive latest message of given label
%% % % % % % %
handle_event({call, From}, {recv, Label}, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = Msgs, timeouts = _Timeouts, queued_actions = _Queue, options = _Options} = _StatemData) -> 
    printout("~p, ~p, looking for msg with label (~p).", [?FUNCTION_NAME, State, Label]),
    % add message to the front of the queue of messages received by this label
    case maps:find(Label, Msgs) of
        {ok, StateMsgs} -> 
            LastMsg = lists:last(StateMsgs),
            NumMsgs = lists:length(StateMsgs),
            printout("~p, ~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, State, Label, LastMsg, NumMsgs]),
            ReplyMsg = {ok, #{ msg => LastMsg, total => NumMsgs }};
         error -> 
            printout("~p, ~p, no msgs with label (~p) found.", [?FUNCTION_NAME, State, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};



%% % % % % % %
%% anything else
%% % % % % % %
handle_event(EventType, EventContent, State, Data) ->
    printout("error, reached unknown event:\n\tEventType: ~p,\n\tEventContent: ~p,\n\tState: ~p,\n\tData: ~p.", [EventType, EventContent, State, Data]),
    {next_state, state_stop, #stop_data{ reason = unknown_event_to_handle, statem_data = Data}}.

