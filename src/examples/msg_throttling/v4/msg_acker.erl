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
-export([ start_link/1, 
          init/2,
          init_setup_state/3,
          stop_state/3 ]).

%% callbacks
-export([ recv_msg1/0,
          recv_msg2/0,
          send_ack1/1,
          send_ack2/1 ]).

%% states
-export([ state1_recv_msg1/3, 
          state2a_send_ack1/3,
          state2b_recv_msg2/3,
          state3a_send_ack2/3 ]).


%% mixed-choice + timeout timing constraints
-define(ACK1_TIME, 3000).
-define(ACK2_TIME, 3000).


-define(NAME, ?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

%% gen_statem
start_link(Params) -> 
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params]),
    {ok, Pid} = gen_statem:start_link({local, ?NAME}, ?MODULE, Params, []),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.
start_link() -> msg_acker:start_link([]).


callback_mode() -> [state_functions, state_enter].

-spec init([{atom(),any()}|[]]) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T]) 
    when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
init([_H|T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = Options} = StatemData) 
    when is_atom(HKey) and is_map_key(HKey, Options) -> init(T, maps:put(options, maps:put(HKey, HVal, Options), StatemData));
init([_H|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = StatemData) -> init(T, StatemData);
init([], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = StatemData) -> {ok, init_setup_state, StatemData}.


stop() -> 
    printout("~p.", [?FUNCTION_NAME]),
    gen_statem:stop(?NAME).


terminate(Reason, State, StatemData) -> 
    printout("~p, {Reason: ~p},\n\t{State: ~p},\n\t{StatemData: ~p}.", [?FUNCTION_NAME,Reason,State,StatemData]),
    ok.




%% custom init/stop wrappers
init_setup_state(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = StatemData) -> {keep_state, StatemData, [{state_timeout, 0, wait_to_finish}]};
init_setup_state(state_timeout, wait_to_finish, #statem_data{coparty_id = _CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupID, sup_init, CoID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME,CoID]),
            {next_state, state1_recv_msg1, #statem_data{ coparty_id = CoID, 
                                                         state_stack = States,
                                                         msg_stack = Msgs,
                                                         queued_actions = Queue,
                                                         options = Options }}
    end.

stop_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = Data) -> {keep_state, Data, [{state_timeout, 0, exit_deferral}]};
stop_state(state_timeout, exit_deferral, #stop_data{reason = Reason, statem_data = #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = StatemData} = Data) -> 
    printout("~p, ~p, StatemData: \n\t~p.", [?FUNCTION_NAME, Reason, StatemData]),
    {stop, Reason, Data}.





%% callback wrappers
-spec cb_send({atom(), any()}) -> ok.
cb_send({Event, Msg}) ->
    printout("~p: ~p.", [Event, Msg]),
    gen_statem:cast(?NAME, {Event, Msg}).

-spec cb_recv({atom()}) -> ok.
cb_recv({Event}) ->
    printout("~p.", [Event]),
    gen_statem:cast(?NAME, {Event}).




%% event callbacks
recv_msg1() -> cb_recv({?FUNCTION_NAME}).
recv_msg2() -> cb_recv({?FUNCTION_NAME}).

send_ack1(Ack) -> cb_send({?FUNCTION_NAME, Ack}).
send_ack2(Ack) -> cb_send({?FUNCTION_NAME, Ack}).




%% optional
non_deterministic_delay(MaxDelay) ->
    Delay = rand:uniform(MaxDelay),
    printout("(~p, ~pms).",[?FUNCTION_NAME, Delay]),
    timer:send_after(Delay, self(), {self(), ?FUNCTION_NAME}),
    receive 
        {_SelfID, non_deterministic_delay} -> 
            printout("(~p, end).", [?FUNCTION_NAME]) 
    end,
    {ok, Delay}.




%% states 
state1_recv_msg1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state1_recv_msg1(info, {CoPartyID, Msg1}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Msg1]),
    % set up next configuration
    NextState = state2a_send_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"recv", Msg1}] ++ Msgs,
                                queued_actions = Queue,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1};
%% state1, queue sending action from next state
state1_recv_msg1(cast, {send_ack1, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, too early to act,\n\tadding to queue: ~p.", [?FUNCTION_NAME, {send_ack1, Msg}]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = States,
                                msg_stack = Msgs,
                                queued_actions = Queue ++ [{send_ack1, Msg}],
                                options = Options },
    {keep_state, StatemData1};
state1_recv_msg1(info, {CoPartyID, Ack}, #statem_data{ coparty_id = CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = _Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p) too early! postponing.", [?FUNCTION_NAME, Ack]),
    {keep_state_and_data, [postpone]}.



state2a_send_ack1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [_H], options = _Options} = _StatemData) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout,0,process_queue}]};
%% state2a, no queued actions
state2a_send_ack1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [], options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
%% state2a, process queue contents
state2a_send_ack1(state_timeout, process_queue, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = [H|T], options = Options} = _StatemData) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = States,
                                msg_stack = Msgs,
                                queued_actions = T,
                                options = Options },
    {repeat_state, StatemData1, [{next_event, cast, H}]};
state2a_send_ack1(cast, {send_ack1, Ack}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % add delay (iff, option set)
    case AllowDelayableSends of
        true -> {ok, _} = non_deterministic_delay(?ACK1_TIME * 2);
        _ -> ok
    end,
    % perform action
    CoPartyID ! {self(), Ack},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Ack]),
    % set up next configuration
    NextState = state1_recv_msg1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"send", Ack}] ++ Msgs,
                                queued_actions = Queue,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1};
%% state2a, queue sending action from next state
state2a_send_ack1(cast, {send_ack2, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, too early to act,\n\tadding to queue: ~p.", [?FUNCTION_NAME, {send_ack2, Msg}]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = States,
                                msg_stack = Msgs,
                                queued_actions = Queue ++ [{send_ack2, Msg}],
                                options = Options },
    {keep_state, StatemData1};
state2a_send_ack1(info, {CoPartyID, Ack}, #statem_data{ coparty_id = CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = _Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p) too early! postponing.", [?FUNCTION_NAME, Ack]),
    {keep_state_and_data, [postpone]}.
    


state2b_recv_msg2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state2b_recv_msg2(info, {CoPartyID, Msg2}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Msg2]),
    % set up next configuration
    NextState = state3a_send_ack2,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"recv", Msg2}] ++ Msgs,
                                queued_actions = Queue,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1};
%% state2b, queue sending action from next state
state2b_recv_msg2(cast, {send_ack2, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = Options} = _StatemData) ->
    printout("~p, too early to act,\n\tadding to queue: ~p.", [?FUNCTION_NAME, {send_ack2, Msg}]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = States,
                                msg_stack = Msgs,
                                queued_actions = Queue ++ [{send_ack2, Msg}],
                                options = Options },
    {keep_state, StatemData1}.


state3a_send_ack2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [_H], options = _Options} = _StatemData) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout,0,process_queue}]};
%% state3a, no queued actions
state3a_send_ack2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = [], options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
%% state3a, process queue contents
state3a_send_ack2(state_timeout, process_queue, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = [H|T], options = Options} = _StatemData) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = States,
                                msg_stack = Msgs,
                                queued_actions = T,
                                options = Options },
    {repeat_state, StatemData1, [{next_event, cast, H}]};
state3a_send_ack2(cast, {send_ack2, Ack}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, queued_actions = Queue, options = #statem_options{ allow_delayable_sends = AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % add delay (iff, option set)
    case AllowDelayableSends of
        true -> {ok, _} = non_deterministic_delay(?ACK2_TIME * 2);
        _ -> ok
    end,
    % perform action
    CoPartyID ! {self(), Ack},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Ack]),
    % set up next configuration
    NextState = state2a_send_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"send", Ack}] ++ Msgs,
                                queued_actions = Queue,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1}.

