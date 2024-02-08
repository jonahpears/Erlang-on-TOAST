-module(msg_throttling_acker).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_msg1/1,
         receive_msg2/1,
         receive_tout/1,
         send_ack1/1,
         send_ack2/1,
         send_after_state2/3,
         send_after_state5/3,
         start_link/0,
         std_state1/3,
         std_state4/3,
         std_state7/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(cast, {receive_msg1, Msg1}, Data) -> {next_state, send_after_state2, Data}.

send_after_state2(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 3, std_state4}]};
send_after_state2(internal, {send_ack1, Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
send_after_state2(state_timeout, std_state4, Data) -> {next_state, std_state4, Data}.

std_state4(enter, _OldState, _Data) -> keep_state_and_data;
std_state4(cast, {receive_msg2, Msg2}, Data) -> {next_state, send_after_state5, Data}.

send_after_state5(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 3, std_state7}]};
send_after_state5(internal, {send_ack2, Ack2}, Data) -> {next_state, send_after_state5, Data};
%% This is a timeout branch:
send_after_state5(state_timeout, std_state7, Data) -> {next_state, std_state7, Data}.

std_state7(enter, _OldState, _Data) -> keep_state_and_data;
std_state7(cast, {receive_tout, Tout}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_msg1(Msg1) -> gen_statem:cast(?SERVER, {receive_msg1, Msg1}).

receive_msg2(Msg2) -> gen_statem:cast(?SERVER, {receive_msg2, Msg2}).

receive_tout(Tout) -> gen_statem:cast(?SERVER, {receive_tout, Tout}).

%% Timeout (3)
%% Duration `TimeDelay` may be long enough to trigger a timeout.
%% Timer represents some time consuming task that must be completed before performing send.
%% If TimeDelay>3 then timeout will trigger.
%% Otherwise, send action is performed.
send_ack1(Ack1) ->
    TimeDelay = rand:uniform(3 * 2),
    timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
    receive {delay_stop, TimeDelay, _MoreData} -> io:format("[send_accept]: delay(~p) stopped.\n", [TimeDelay]) end,
    gen_statem:internal(?SERVER, {send_ack1, Ack1}).

%% Timeout (3)
%% Duration `TimeDelay` may be long enough to trigger a timeout.
%% Timer represents some time consuming task that must be completed before performing send.
%% If TimeDelay>3 then timeout will trigger.
%% Otherwise, send action is performed.
send_ack2(Ack2) ->
    TimeDelay = rand:uniform(3 * 2),
    timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
    receive {delay_stop, TimeDelay, _MoreData} -> io:format("[send_accept]: delay(~p) stopped.\n", [TimeDelay]) end,
    gen_statem:internal(?SERVER, {send_ack2, Ack2}).

stop() -> gen_statem:stop(?SERVER).