-module(tbranch_tout_r).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         mixed_state2/3,
         mixed_state2_2/3,
         send_accept/1,
         recv_msg/1,
         recv_tout/1,
         start_link/0,
         std_state1/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, std_state1, {}}.

std_state1(enter, _OldState, _Data) -> keep_state_and_data;
std_state1(internal, {recv_msg, Msg}, Data) -> {next_state, mixed_choice_state2, Data}.

mixed_state2(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout,10000,mixed_state2_2}]};
mixed_state2(internal, {send_accept, Accept}, Data) -> {stop, normal, Data};
mixed_state2(state_timeout, TimeoutState, Data) -> {next_state, TimeoutState, Data}.

mixed_state2_2(enter, _OldState, Data) -> keep_state_and_data;
mixed_state2_2(cast, {recv_tout, Tout}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

send_accept(Accept) -> 
    %% default delay (3):
    TimeDelay = (rand:uniform(3*2))*1000,
    %% some time consuming task:
    timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
    %% if task ends early enough, action can be sent
    receive {delay_stop, TimeDelay, _MoreData} -> io:format("[send_accept]: delay(~p) stopped.\n", [TimeDelay]) end,
        
    gen_statem:internal(?SERVER, {send_accept, Accept}).


recv_msg(Msg) -> gen_statem:cast(?SERVER, {recv_msg, Msg}).

recv_tout(Tout) -> gen_statem:cast(?SERVER, {recv_tout, Tout}).

stop() -> gen_statem:stop(?SERVER).