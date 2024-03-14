-module(msg_acker).

-behaviour(gen_statem).

-include("data_records.hrl").

%% gen_statem
-export([ start_link/0,
          callback_mode/0,
          init/1,
          stop/0,
          terminate/3 ]).

%% custom wrappers for gen_statem
% -export([ start_link/1, 
%           init/2,
%           init_setup_state/3,
%           stop_state/3 ]).
-export([ start_link/1, 
          init/2 ]).

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
    Timeouts = #{ state2a_send_ack1 => {5000, state2b_recv_msg2} },
    StateMap = #{ state1_recv_msg1  => #{recv => #{msg1 => state2a_send_ack1} },
                  state2a_send_ack1 => #{send => #{ack1 => state1_recv_msg1}  },
                  state2b_recv_msg2 => #{recv => #{msg2 => state3a_send_ack2} },
                  state3a_send_ack2 => #{send => #{ack2 => state2a_send_ack1} } },
    % add timeout + statemap parameters 
    Params1 = [{timeouts, Timeouts}, {state_map, StateMap}] ++ Params,
    printout("~p, Params: ~p.", [?FUNCTION_NAME, Params1]),
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
init([{HKey,HVal}|T], #statem_data{ coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = Options} = StatemData) 
    when is_atom(HKey) and is_map_key(HKey, Options) -> init(T, maps:put(options, maps:put(HKey, HVal, Options), StatemData));
init([{timeouts,HVal}|T], #statem_data{ coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) -> 
    init(T, maps:put(timeouts, HVal, StatemData));
init([{state_map,HVal}|T], #statem_data{ coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) -> 
    init(T, maps:put(state_map, HVal, StatemData));
init([_H|T], #statem_data{ coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) -> init(T, StatemData);
init([], #statem_data{ coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) -> 
    {ok, init_setup_state, StatemData}.


stop() -> 
    printout("~p.", [?FUNCTION_NAME]),
    gen_statem:stop(?NAME).


terminate(Reason, state3b_issue_timeout, StatemData) ->
    printout("~p, timeout: {Reason: ~p},\n\t{State: ~p}", [?FUNCTION_NAME, Reason, StatemData]);
terminate(Reason, State, StatemData) -> 
    printout("~p, {Reason: ~p},\n\t{State: ~p},\n\t{StatemData: ~p}.", [?FUNCTION_NAME,Reason,State,StatemData]),
    ok.




%% callback wrappers
-spec cb_send({atom(), any()}) -> ok.
cb_send({Label, Msg}) ->
    printout("~p, (~p: ~p).", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?NAME, {send, Label, Msg}).

-spec cb_recv({atom()}) -> ok.
cb_recv({Label}) ->
    printout("~p, (~p).", [?FUNCTION_NAME, Label]),
    gen_statem:call(?NAME, {recv, Label}).

-spec cb_timeout({atom(), any()}) -> ok.
cb_timeout({Label, Msg}) ->
    printout("~p, (~p: ~p).", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?NAME, {Label, Msg}).




%% event callbacks
send_msg1({Label, Msg}) -> cb_send({Label, Msg}).
send_msg2({Label, Msg}) -> cb_send({Label, Msg}).

issue_timeout({Label, Msg}) -> cb_timeout({Label, Msg}).

recv_ack1({Label}) -> cb_recv({Label}).
recv_ack2({Label}) -> cb_recv({Label}).




%% custom init/stop wrappers

%% catch before start (to receive the CoPartyID)
handle_event(enter, _OldState, init_setup_state=_State, #statem_data{coparty_id = undefined, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) -> 
    {keep_state, StatemData, [{state_timeout, 0, wait_to_finish}]};
handle_event(state_timeout, wait_to_finish, init_setup_state=State, #statem_data{coparty_id = undefined, states = States, msgs = Msgs, timeouts = Timeouts, state_map = StateMap, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, waiting to finish setup.", [State]),
    receive
        {_SupID, sup_init, CoID} ->
            printout("~p, received coparty ID (~p).", [State,CoID]),
            {next_state, state1_send_msg1, #statem_data{ coparty_id = CoID, 
                                                         states = States,
                                                         msgs = Msgs,
                                                         timeouts = Timeouts,
                                                         state_map = StateMap,
                                                         queued_actions = Queue,
                                                         options = Options }}
    end;

%% catch before stop
handle_event(enter, _OldState, stop_state=_State, #stop_data{reason = _Reason, statem_data = _StatemData} = Data) -> 
    {keep_state, Data, [{state_timeout, 0, exit_deferral}]};
handle_event(state_timeout, exit_deferral, stop_state=State, #stop_data{reason = Reason, statem_data = #statem_data{coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData} = Data) -> 
    printout("~p, ~p, StatemData: \n\t~p.", [State, Reason, StatemData]),
    {stop, Reason, Data};





%% % % % % % %
%% states enter
%% % % % % % %


%% state enter, issue timeout (stop and cause supervisor to notice)
handle_event(enter, _OldState, state3b_issue_timeout=State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) ->
    printout("(->) ~p.", [State]),
    {keep_state, #stop_data{reason = error_exceeded_throttling_capacity, statem_data = StatemData}, [{state_timeout, 0, goto_stop}]};
handle_event(state_timeout, goto_stop, state3b_issue_timeout=_State, Data) ->
    {next_state, state_stop, Data};

%% state enter, handle queued actions
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = [H], options = _Options} = _StatemData) ->
    printout("(->) ~p, with (~p) queued actions.", [State, lists:length(H)]),
    {keep_state_and_data, [{state_timeout,0,process_queue}]};
handle_event(state_timeout, process_queue, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, state_map = StateMap, queued_actions = [H|T], options = Options} = _StatemData) ->
    printout("(->) ~p, queued action: ~p.", [State, H]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                state_map = StateMap,
                                queued_actions = T,
                                options = Options },
    {repeat_state, StatemData1, [{next_event, cast, H}]};

%% state enter, mixed choice (and no queued actions)
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = Timeouts, state_map = _StateMap, queued_actions = [], options = _Options} = StatemData)
    when is_map_key(State, Timeouts) ->
        {TimeoutDuration, TimeoutState} = maps:get(State, Timeouts),
        {keep_state, StatemData, [{state_timeout, TimeoutDuration, TimeoutState}]};


%% state enter, no queued actions and no mixed-choice
handle_event(enter, _OldState, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = [], options = _Options} = _StatemData) ->
    printout("(->) ~p.", [State]),
    keep_state_and_data;



%% % % % % % %
%% sending actions, from correct states
%% % % % % % %
handle_event(cast, {send, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, state_map = StateMap, queued_actions = Queue, options = Options} = _StatemData) 
    when is_map_key(State, StateMap)
    and map_get(Label, map_get(send, map_get(State, StateMap))) ->
    CoPartyID ! {self(), Label, Msg},
    NextState = maps:get(Label, maps:get(send, maps:get(State, StateMap))),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                state_map = StateMap,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, send (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};



%% % % % % % %
%% receiving actions, from correct states
%% % % % % % %
handle_event(info, {CoPartyID, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, state_map = StateMap, queued_actions = Queue, options = Options} = _StatemData) 
    when is_map_key(State, StateMap)
    and map_get(Label, map_get(recv, map_get(State, StateMap))) ->    
    % add message to the front of the queue of messages received by this label
    case maps:find(Label, Msgs) of
        {ok, StateMsgs} -> 
            Msgs1 = maps:put([Msg] ++ StateMsgs, Label, Msgs);
        error -> 
            Msgs1 = maps:put([Msg], Label, Msgs)
    end,
    NextState = maps:get(Label, maps:get(recv, maps:get(State, StateMap))),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = [NextState] ++ States,
                                msgs = Msgs1,
                                timeouts = Timeouts,
                                queued_actions = Queue,
                                options = Options },
    printout("~p, recv (~p: ~p).", [State, Label, Msg]),
    {next_state, NextState, StatemData1};



%% % % % % % %
%% sending actions, from wrong states
%% % % % % % %
handle_event(cast, {send, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = States, msgs = Msgs, timeouts = Timeouts, state_map = StateMap, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, wrong state to send (~p: ~p), adding to queue.", [State, Label, Msg]),
    printout("~p, StateMap: ~p.", [State, StateMap]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                states = States,
                                msgs = Msgs,
                                timeouts = Timeouts,
                                state_map = StateMap,
                                queued_actions = Queue ++ [{send, Label, Msg}],
                                options = Options },
    {keep_state, StatemData1};



%% % % % % % %
%% receiving actions, from wrong states
%% % % % % % %
handle_event(info, {CoPartyID, Label, Msg}, State, #statem_data{ coparty_id = CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = _StatemData) ->
    printout("~p, wrong state to recv (~p: ~p), postponing.", [State, Label, Msg]),
    {keep_state_and_data, [postpone]};



%% % % % % % %
%% mixed-choice (timeouts)
%% % % % % % %
handle_event(state_timeout, NextState, state3b_issue_timeout=State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = _Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = StatemData) ->
    printout("~p, internal timeout (~p).", [State, NextState]),
    {next_state, NextState, StatemData};



%% % % % % % %
%% retreive latest message of given label
%% % % % % % %
handle_event({call, From}, {recv, Label}, State, #statem_data{ coparty_id = _CoPartyID, states = _States, msgs = Msgs, timeouts = _Timeouts, state_map = _StateMap, queued_actions = _Queue, options = _Options} = _StatemData) -> 
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

