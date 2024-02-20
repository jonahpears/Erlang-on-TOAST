-module(msg_throttling_acker).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         custom_init_state/3,
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

% init([]) -> {ok, std_state1, {}}.
init([]) -> {ok, custom_init_state, {}}.
custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, MsgerID} -> {next_state, std_state1, #{msger_id => MsgerID}} end.
% custom_init_state(enter, _OldState, _Data) -> keep_state_and_data;
% custom_init_state(internal, _OldState, _Data) -> receive {_SupID, sup_init, MsgerID} -> {next_state, std_state1, #{msger_id => MsgerID}} end.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(cast, {receive_msg1, _Msg1}, Data) -> {next_state, send_after_state2, Data}.

send_after_state2(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [send_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state4}]};
send_after_state2(internal, {send_ack1, _Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
send_after_state2(state_timeout, std_state4, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state4]),
    {next_state, std_state4, Data}.

std_state4(enter, _OldState, _Data) -> keep_state_and_data;
std_state4(cast, {receive_msg2, _Msg2}, Data) -> {next_state, send_after_state5, Data}.

send_after_state5(enter, _OldState, Data) ->
    io:format("(~p ->.)\n", [send_after_state5]),
    {keep_state, Data, [{state_timeout, 3000, std_state7}]};
send_after_state5(internal, {send_ack2, _Ack2}, Data) -> {next_state, send_after_state5, Data};
%% This is a timeout branch:
send_after_state5(state_timeout, std_state7, Data) ->
    io:format("(timeout[~p] -> ~p.)\n", [3000, std_state7]),
    {next_state, std_state7, Data}.

std_state7(enter, _OldState, _Data) -> keep_state_and_data;
std_state7(cast, {receive_tout, _Tout}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_msg1(Msg1) -> gen_statem:cast(?SERVER, {receive_msg1, Msg1}).

receive_msg2(Msg2) -> gen_statem:cast(?SERVER, {receive_msg2, Msg2}).

receive_tout(Tout) -> gen_statem:cast(?SERVER, {receive_tout, Tout}).

send_ack1(Ack1) -> gen_statem:internal(?SERVER, {send_ack1, Ack1}).

send_ack2(Ack2) -> gen_statem:internal(?SERVER, {send_ack2, Ack2}).

stop() -> gen_statem:stop(?SERVER).