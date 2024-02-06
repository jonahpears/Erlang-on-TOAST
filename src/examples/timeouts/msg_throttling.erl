-module(msg_throttling).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([
    start_link/0,
    init/1,
    callback_mode/0,
    stop/0,
    terminate/3,
    state1/3,
    mixed_state2/3,
    mixed_state3/3,
    state4/3,
    act_s_msg1/1,
    act_s_msg2/1,
    act_r_ack1/1,
    act_r_ack2/1,
    act_s_tout/1
        ]).

    

%% basic template necessities
start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

terminate(_Reason, _State, _Data) -> ok.

stop() -> gen_statem:stop(?SERVER).


%% states
state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_s_msg1, _Msg1}, Data) -> {next_state, mixed_state2, Data}.


mixed_state2(enter, _OldState, _Data) -> keep_state_and_data;
mixed_state2(cast, {act_r_ack1, _Msg1}, Data) -> {next_state, state1, Data};
mixed_state2(timeout, {act_s_msg1, _Msg1}, Data) -> {next_state, mixed_state3, Data}.


mixed_state3(enter, _OldState, _Data) -> keep_state_and_data;
mixed_state3(cast, {act_r_ack2, _Msg1}, Data) -> {next_state, mixed_state2, Data};
mixed_state3(timeout, {act_s_msg2, _Msg1}, Data) -> {next_state, state4, Data}.


state4(enter, _OldState, _Data) -> keep_state_and_data;
state4(cast, {act_s_tout, _Tout}, Data) -> {stop, normal, Data}.


%% state actions
act_s_msg1(Msg1) -> gen_statem:cast(?SERVER, {act_s_msg1, Msg1}).
act_r_ack1(Ack1) -> gen_statem:cast(?SERVER, {act_r_ack1, Ack1}).
act_s_msg2(Msg2) -> gen_statem:cast(?SERVER, {act_s_msg2, Msg2}).
act_r_ack2(Ack2) -> gen_statem:cast(?SERVER, {act_r_ack2, Ack2}).
act_s_tout(Tout) -> gen_statem:cast(?SERVER, {act_s_tout, Tout}).
