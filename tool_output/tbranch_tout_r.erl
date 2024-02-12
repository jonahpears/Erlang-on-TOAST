-module(tbranch_tout_r).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_msg/1,
         receive_tout/1,
         select_after_state2/3,
         send_accept/1,
         send_reject/1,
         start_link/0,
         std_state1/3,
         std_state4/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(cast, {receive_msg, Msg}, Data) -> {next_state, select_after_state2, Data}.

select_after_state2(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 3, std_state4}]};
select_after_state2(internal, {send_accept, Accept}, Data) -> {stop, normal, Data};
select_after_state2(internal, {send_reject, Reject}, Data) -> {stop, normal, Data};
%% This is a timeout branch:
select_after_state2(state_timeout, std_state4, Data) -> {next_state, std_state4, Data}.

terminate(_Reason, _State, _Data) -> ok.

std_state4(enter, _OldState, _Data) -> keep_state_and_data;
std_state4(cast, {receive_tout, Tout}, Data) -> {stop, normal, Data}.

receive_msg(Msg) -> gen_statem:cast(?SERVER, {receive_msg, Msg}).

receive_tout(Tout) -> gen_statem:cast(?SERVER, {receive_tout, Tout}).

send_accept(Accept) -> gen_statem:internal(?SERVER, {send_accept, Accept}).

send_reject(Reject) -> gen_statem:internal(?SERVER, {send_reject, Reject}).

stop() -> gen_statem:stop(?SERVER).