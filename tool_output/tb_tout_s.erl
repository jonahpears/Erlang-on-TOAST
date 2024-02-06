-module(tb_tout_s).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         mixed_state2/3,
         receive_ack/1,
         send_msg/1,
         send_tout/1,
         start_link/0,
         std_state1/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(internal, {send_msg, Msg}, Data) ->
    {next_state, mixed_choice_state2, Data}.

mixed_state2(enter, _OldState, _Data) -> keep_state_and_data;
mixed_state2(cast, {receive_ack, Ack}, Data) -> {stop, normal, Data};
mixed_state2(internal, {send_tout, Tout}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

receive_ack(Ack) -> gen_statem:cast(?SERVER, {receive_ack, Ack}).

send_msg(Msg) -> gen_statem:internal(?SERVER, {send_msg, Msg}).

send_tout(Tout) -> gen_statem:internal(?SERVER, {send_tout, Tout}).

stop() -> gen_statem:stop(?SERVER).