-module(msger).

-behaviour(gen_statem).

-include("data_records.hrl").

%% gen_statem
-export([ start_link/0,
          start_link/1, 
          callback_mode/0,
          init/1,
          terminate/3 ]).

%% custom wrappers for gen_statem
-export([ init/2,
          init_setup_state/3,
          stop_state/3 ]).

%% callbacks
-export([ send_msg1/1,
          send_msg2/1,
          recv_ack1/0,
          recv_ack2/0,
          issue_timeout/1 ]).

%% states
-export([ state1_send_msg1/3, 
          state2a_recv_ack1/3,
          state2b_send_msg2/3,
          state3a_recv_ack2/3,
          state3b_issue_timeout/3 ]).


-define(NAME, ?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: ~p\n", [?NAME,self(), Str] ++ Params).

%% gen_statem
start_link() -> gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

start_link(Params) -> gen_statem:start_link({local, ?NAME}, ?MODULE, Params, []).

callback_mode() -> [state_functions, state_enter].

-spec init([{atom(),any()}|[]]) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T]) 
    when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
init([_H|T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = Options} = StatemData) 
    when is_atom(HKey) and is_map_key(HKey, Options) -> init(T, maps:put(options, maps:put(HKey, HVal, Options), StatemData));
init([_H|T], StatemData) -> init(T, StatemData);
init([], StatemData) -> {ok, init_setup_state, StatemData}.


terminate(Reason, State, StatemData) -> 
    printout("~p, {Reason: ~p},\n\t{State: ~p},\n\t{StatemData: ~p}.", [?FUNCTION_NAME,Reason,State,StatemData]),
    ok.


%% custom init/stop wrappers
init_setup_state(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) -> {keep_state, StatemData, [{state_timeout, 0, wait_to_finish}]};
init_setup_state(state_timeout, wait_to_finish, StatemData) ->
    % io:format("[~p|~p]: init_setup_state, waiting to finish setup.", [?NAME,self()]),
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupID, sup_init, CoID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME,CoID]),
            % io:format("[~p|~p]: init_setup_state, received coparty ID (~p).", [?NAME,self(),CoID]),
            {next_state, state_send_msg1, maps:put(coparty_id, CoID, StatemData)}
    end.

stop_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = Data) -> {keep_state, Data, [{state_timeout, 0, exit_deferral}]};
stop_state(state_timeout, exit_deferral, #stop_data{reason = Reason, statem_data = StatemData} = Data) -> 
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

-spec cb_timeout({atom(), any()}) -> ok.
cb_timeout({Event, Msg}) ->
    printout("~p.", [Event]),
    gen_statem:cast(?NAME, {Event, Msg}).


%% event callbacks
send_msg1(Msg) -> cb_send({?FUNCTION_NAME, Msg}).
send_msg2(Msg) -> cb_send({?FUNCTION_NAME, Msg}).

issue_timeout(Msg) -> cb_timeout({?FUNCTION_NAME, Msg}).

recv_ack1() -> cb_recv({?FUNCTION_NAME}).
recv_ack2() -> cb_recv({?FUNCTION_NAME}).

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
state1_send_msg1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state1_send_msg1(cast, {send_msg1, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = Options} = StatemData) ->
    % add delay (iff, option set)
    case maps:get(allow_delayable_sends, Options, false) of
        true -> {ok, _} = non_deterministic_delay(3000 * 2);
        _ -> ok
    end,
    % perform action
    CoPartyID ! {self(), Msg},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Msg]),
    % set up next configuration
    NextState = state_recv_ack1,
    StatemData1 = StatemData#statem_data{ coparty_id = CoPartyID,
                                          state_stack = [NextState] ++ States,
                                          msg_stack = [{"send", Msg}] ++ Msgs,
                                          options = Options },
    % move to next state
    {next_state, NextState, StatemData1}.


state2a_recv_ack1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    {keep_state, StatemData, [{state_timeout, 3000, state2b_send_msg2}]};
state2a_recv_ack1(info, {CoPartyID, Ack1}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = Options} = StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Ack1]),
    % set up next configuration
    NextState = state1_send_msg1,
    StatemData1 = StatemData#statem_data{ coparty_id = CoPartyID,
                                          state_stack = [NextState] ++ States,
                                          msg_stack = [{"recv", Ack1}] ++ Msgs,
                                          options = Options },
    % move to next state
    {next_state, NextState, StatemData1};
state2a_recv_ack1(state_timeout, state2b_send_msg2, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("~p, internal timeout (~p).", [?FUNCTION_NAME, 3000]),
    {next_state, state2b_send_msg2, StatemData}.
    


state2b_send_msg2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state2b_send_msg2(cast, {send_msg2, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = Options} = StatemData) ->
    % add delay (iff, option set)
    case maps:get(allow_delayable_sends, Options, false) of
        true -> {ok, _} = non_deterministic_delay(3000 * 2);
        _ -> ok
    end,
    % perform action
    CoPartyID ! {self(), Msg},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Msg]),
    % set up next configuration
    NextState = state_recv_ack2,
    StatemData1 = StatemData#statem_data{ coparty_id = CoPartyID,
                                          state_stack = [NextState] ++ States,
                                          msg_stack = [{"send", Msg}] ++ Msgs,
                                          options = Options },
    % move to next state
    {next_state, NextState, StatemData1}.


state3a_recv_ack2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    {keep_state, StatemData, [{state_timeout, 3000, state3b_issue_timeout}]};
state3a_recv_ack2(info, {CoPartyID, Ack2}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = Options} = StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Ack2]),
    % set up next configuration
    NextState = state2a_recv_ack1,
    StatemData1 = StatemData#statem_data{ coparty_id = CoPartyID,
                                          state_stack = [NextState] ++ States,
                                          msg_stack = [{"recv", Ack2}] ++ Msgs,
                                          options = Options },
    % move to next state
    {next_state, NextState, StatemData1};
state3a_recv_ack2(state_timeout, state3b_issue_timeout, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("~p, internal timeout (~p).", [?FUNCTION_NAME, 3000]),
    {next_state, state3b_issue_timeout, StatemData}.



state3b_issue_timeout(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    {keep_state, #stop_data{reason = error_exceeded_throttling_capacity, statem_data = StatemData}, [{state_timeout, 0, goto_stop}]};
state3b_issue_timeout(state_timeout, goto_stop, Data) ->
    {next_state, state_stop, Data}.














    % printout("~p, .", [?FUNCTION_NAME]),
    











    % io:format("[~p|~p]: state, msg.", [?NAME,self()]),
