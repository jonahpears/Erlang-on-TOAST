-module('_msg_throttling_msger').

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         custom_end_state/3,
         init/1,
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

-file("_msg_throttling_msger", 1).

-record(statem_data, {coparty_id = undefined, trace = [], msgs = [], state_map = {}, delayable_sends = false}).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(internal, {send_msg1, Msg1}, Data) -> {next_state, recv_after_state2, Data}.

recv_after_state2(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [recv_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state3}]};
recv_after_state2(cast, {receive_ack1, Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
recv_after_state2(state_timeout, std_state3, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state3]),
    {next_state, std_state3, Data}.

std_state3(enter, _OldState, _Data) -> keep_state_and_data;
std_state3(internal, {send_msg2, Msg2}, Data) -> {next_state, recv_after_state4, Data}.

recv_after_state4(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [recv_after_state4]),
    {keep_state, Data, [{state_timeout, 3000, std_state5}]};
recv_after_state4(cast, {receive_ack2, Ack2}, Data) -> {next_state, recv_after_state2, Data};
%% This is a timeout branch:
recv_after_state4(state_timeout, std_state5, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state5]),
    {next_state, std_state5, Data}.

std_state5(enter, _OldState, _Data) -> keep_state_and_data;
std_state5(internal, {send_tout, Tout}, Data) -> {next_state, custom_end_state, Data}.

custom_end_state(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [custom_end_state]),
    {keep_state, Data, [{state_timeout, 0, go_to_terminate}]};
custom_end_state(state_timeout, go_to_terminate, Data) -> {next_state, go_to_terminate, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_ack1(Ack1) -> gen_statem:cast(?SERVER, {receive_ack1, Ack1}).

receive_ack2(Ack2) -> gen_statem:cast(?SERVER, {receive_ack2, Ack2}).

send_msg1(Msg1) -> gen_statem:internal(?SERVER, {send_msg1, Msg1}).

send_msg2(Msg2) -> gen_statem:internal(?SERVER, {send_msg2, Msg2}).

send_tout(Tout) -> gen_statem:internal(?SERVER, {send_tout, Tout}).

stop() -> gen_statem:stop(?SERVER).