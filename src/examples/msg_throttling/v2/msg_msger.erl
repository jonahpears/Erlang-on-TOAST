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

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

%% gen_statem
start_link(Params) -> 
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params]),
    {ok, Pid} = gen_statem:start_link({local, ?NAME}, ?MODULE, Params, []),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.

start_link() -> msg_msger:start_link([]).

callback_mode() -> [state_functions, state_enter].

-spec init([{atom(),any()}|[]]) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T]) 
    when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> %init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
    StatemData = #statem_data{},
    StatemOptions = #statem_options{},
    StatemOptions1 = maps:put(HKey, HVal, StatemOptions),
    StatemData1 = maps:put(options, StatemOptions1, StatemData),
    % init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
    init(T, StatemData1);
init([_H|T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = Options} = StatemData) 
    when is_atom(HKey) and is_map_key(HKey, Options) -> init(T, maps:put(options, maps:put(HKey, HVal, Options), StatemData));
init([_H|T], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) -> init(T, StatemData);
init([], #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) -> {ok, init_setup_state, StatemData}.


stop() -> 
    printout("~p.", [?FUNCTION_NAME]),
    gen_statem:stop(?NAME).


terminate(Reason, state3b_issue_timeout, StatemData) ->
    printout("~p, timeout: {Reason: ~p},\n\t{State: ~p}", [?FUNCTION_NAME, Reason, StatemData]);
terminate(Reason, State, StatemData) -> 
    printout("~p, {Reason: ~p},\n\t{State: ~p},\n\t{StatemData: ~p}.", [?FUNCTION_NAME,Reason,State,StatemData]),
    ok.


%% custom init/stop wrappers
init_setup_state(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) -> {keep_state, StatemData, [{state_timeout, 0, wait_to_finish}]};
init_setup_state(state_timeout, wait_to_finish, #statem_data{coparty_id = _CoPartyID, state_stack = States, msg_stack = Msgs, options = Options} = _StatemData) ->
    % io:format("[~p|~p]: init_setup_state, waiting to finish setup.", [?NAME,self()]),
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupID, sup_init, CoID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME,CoID]),
            % io:format("[~p|~p]: init_setup_state, received coparty ID (~p).", [?NAME,self(),CoID]),
            % {next_state, state1_send_msg1, maps:put(coparty_id, CoID, StatemData)}
            {next_state, state1_send_msg1, #statem_data{ coparty_id = CoID, 
                                                         state_stack = States,
                                                         msg_stack = Msgs,
                                                         options = Options }}
    end.

stop_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = Data) -> {keep_state, Data, [{state_timeout, 0, exit_deferral}]};
stop_state(state_timeout, exit_deferral, #stop_data{reason = Reason, statem_data = #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData} = Data) -> 
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




%% states 
state1_send_msg1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = _StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state1_send_msg1(cast, {send_msg1, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % perform action
    CoPartyID ! {self(), Msg},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Msg]),
    % set up next configuration
    NextState = state2a_recv_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"send", Msg}] ++ Msgs,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1}.


state2a_recv_ack1(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    {keep_state, StatemData, [{state_timeout, 3000, state2b_send_msg2}]};
state2a_recv_ack1(info, {CoPartyID, Ack1}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Ack1]),
    % set up next configuration
    NextState = state1_send_msg1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
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
state2b_send_msg2(cast, {send_msg2, Msg}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % perform action
    CoPartyID ! {self(), Msg},
    printout("~p, sent (~p).", [?FUNCTION_NAME, Msg]),
    % set up next configuration
    NextState = state3a_recv_ack2,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
                                state_stack = [NextState] ++ States,
                                msg_stack = [{"send", Msg}] ++ Msgs,
                                options = Options },
    % move to next state
    {next_state, NextState, StatemData1}.


state3a_recv_ack2(enter, _OldState, #statem_data{ coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, options = _Options} = StatemData) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    {keep_state, StatemData, [{state_timeout, 3000, state3b_issue_timeout}]};
state3a_recv_ack2(info, {CoPartyID, Ack2}, #statem_data{ coparty_id = CoPartyID, state_stack = States, msg_stack = Msgs, options = #statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = Options} = _StatemData) ->
    % recv
    printout("~p, recv (~p).", [?FUNCTION_NAME, Ack2]),
    % set up next configuration
    NextState = state2a_recv_ack1,
    StatemData1 = #statem_data{ coparty_id = CoPartyID,
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

