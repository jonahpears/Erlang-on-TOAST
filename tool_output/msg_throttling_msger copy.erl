-module(msg_throttling_msger).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         custom_init_state/3,
         receive_ack1/1,
         receive_ack2/1,
         recv_after_state2/3,
         recv_after_state4/3,
         send_msg1/1,
         send_msg2/1,
         send_tout/1,
         start_link/0,
         std_state1/3,
         std_state3/3,
         std_state5/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

% init([]) -> {ok, std_state1, {}}.
init([]) -> {ok, custom_init_state, {}}.

% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, AckerID} -> {next_state, std_state1, {AckerID}} end.
% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, AckerID} -> {next_state, std_state1, #{acker_id => AckerID}} end.

custom_init_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_init}]};
custom_init_state(state_timeout, finish_init, _Data) -> 
    io:format("msger(~p) waiting.\n", [self()]), 
    receive {_SupID, sup_init, AckerID} -> 
        io:format("msger(~p) received [~p].\n", [self(),AckerID]), 
        {next_state, std_state1, #{acker_id => AckerID, state_trace => [], msgs => []}} 
    end.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(internal, {send_msg1, Msg1}, 
         #{acker_id := AckerID, state_trace := Trace, msgs := Msgs} = _Data) -> 
    %% send msg!
    AckerID ! Msg1,
    
    %% update msgs
    NewMsgs = Msgs,
    % NewMsgs = [Msg1] ++ Msgs,

    %% update trace
    NewTrace = [{std_state1, Msg1}] ++ Trace,

    %% next state
    {next_state, recv_after_state2, #{
        acker_id => AckerID,
        stack_trace => NewTrace,
        msgs => NewMsgs
    }}.

recv_after_state2(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [recv_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state3}]};
recv_after_state2(cast, {receive_ack1, Ack1}, 
                #{acker_id := AckerID, state_trace := Trace, msgs := Msgs} = _Data) -> 
    % %% send msg!
    % % AckerID ! Msg1,

    % %% recv msg
    % receive
    %     {AckerID, Ack1}
    
    % %% update msgs
    % NewMsgs = Msgs,
    % % NewMsgs = [Msg1] ++ Msgs,

    % %% update trace
    % NewTrace = [{std_state1, Msg1}] ++ Trace,

    %% next state
    {next_state, std_state1, #{
        acker_id => AckerID,
        stack_trace => NewTrace,
        msgs => NewMsgs
    }};
%% This is a timeout branch:
recv_after_state2(state_timeout, std_state3, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state3]),
    {next_state, std_state3, Data}.

std_state3(enter, _OldState, _Data) -> keep_state_and_data;
std_state3(internal, {send_msg2, _Msg2}, Data) -> {next_state, recv_after_state4, Data}.

recv_after_state4(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [recv_after_state4]),
    {keep_state, Data, [{state_timeout, 3000, std_state5}]};
recv_after_state4(cast, {receive_ack2, _Ack2}, Data) -> {next_state, recv_after_state4, Data};
%% This is a timeout branch:
recv_after_state4(state_timeout, std_state5, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state5]),
    {next_state, std_state5, Data}.

std_state5(enter, _OldState, _Data) -> keep_state_and_data;
std_state5(internal, {send_tout, _Tout}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_ack1(Ack1) -> gen_statem:cast(?SERVER, {receive_ack1, Ack1}).

receive_ack2(Ack2) -> gen_statem:cast(?SERVER, {receive_ack2, Ack2}).

send_msg1(Msg1) -> gen_statem:internal(?SERVER, {send_msg1, Msg1}).

send_msg2(Msg2) -> gen_statem:internal(?SERVER, {send_msg2, Msg2}).

send_tout(Tout) -> gen_statem:internal(?SERVER, {send_tout, Tout}).

stop() -> gen_statem:stop(?SERVER).