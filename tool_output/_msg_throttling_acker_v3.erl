-module('_msg_throttling_acker_v3').

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         custom_end_state/3,
         init/1,
         receive_msg1/1,
         receive_msg2/1,
         receive_tout/1,
         send_ack1/1,
         send_ack2/1,
         start_link/0,
         state1_std/3,
         state2_send_after/3,
         state4_std/3,
         state5_send_after/3,
         state7_std/3,
         stop/0,
         terminate/3]).

-file("_msg_throttling_acker_v3", 1).

-record(statem_data, {coparty_id = undefined, trace = [], msgs = [], state_map = {}, delayable_sends = false}).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

state1_std(enter, _OldState,
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    keep_state_and_data;
state1_std(cast, {receive_msg1, Msg1}, Data) -> {next_state, send_after_state2, Data}.

state2_send_after(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [state2_send_after]),
    {keep_state, Data, [{state_timeout, 3000, std_state4}]};
state2_send_after(internal, {send_ack1, Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
state2_send_after(state_timeout, std_state4, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state4]),
    {next_state, std_state4, Data}.

state4_std(enter, _OldState,
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    keep_state_and_data;
state4_std(cast, {receive_msg2, Msg2}, Data) -> {next_state, send_after_state5, Data}.

state5_send_after(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [state5_send_after]),
    {keep_state, Data, [{state_timeout, 3000, std_state7}]};
state5_send_after(internal, {send_ack2, Ack2}, Data) -> {next_state, send_after_state2, Data};
%% This is a timeout branch:
state5_send_after(state_timeout, std_state7, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state7]),
    {next_state, std_state7, Data}.

state7_std(enter, _OldState,
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msg_stack = _Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    keep_state_and_data;
state7_std(cast, {receive_tout, Tout}, Data) -> {next_state, custom_end_state, Data}.

custom_end_state(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [custom_end_state]),
    {keep_state, Data, [{state_timeout, 0, go_to_terminate}]};
custom_end_state(state_timeout, go_to_terminate, Data) -> {next_state, go_to_terminate, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_msg1(Msg1) -> gen_statem:cast(?SERVER, {receive_msg1, Msg1}).

receive_msg2(Msg2) -> gen_statem:cast(?SERVER, {receive_msg2, Msg2}).

receive_tout(Tout) -> gen_statem:cast(?SERVER, {receive_tout, Tout}).

send_ack1(Ack1) -> gen_statem:internal(?SERVER, {send_ack1, Ack1}).

send_ack2(Ack2) -> gen_statem:internal(?SERVER, {send_ack2, Ack2}).

stop() -> gen_statem:stop(?SERVER).