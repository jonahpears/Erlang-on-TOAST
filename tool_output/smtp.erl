-module(smtp).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_220/1,
         act_235/1,
         act_250/1,
         act_250d/1,
         act_535/1,
         act_auth/1,
         act_ehlo/1,
         act_quit/1,
         act_startTls/1,
         callback_mode/0,
         init/1,
         start_link/0,
         state1/3,
         state10/3,
         state11/3,
         state2/3,
         state3/3,
         state5/3,
         state6/3,
         state7/3,
         state8/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_220, 220}, Data) -> {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(cast, {act_ehlo, Ehlo}, Data) -> {next_state, state3, Data};
state2(cast, {act_quit, Quit}, Data) -> {stop, normal, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
state3(cast, {act_250d, 250d}, Data) -> {next_state, state3, Data};
state3(cast, {act_250, 250}, Data) -> {next_state, state5, Data}.

state5(enter, _OldState, _Data) -> keep_state_and_data;
state5(cast, {act_startTls, StartTls}, Data) -> {next_state, state6, Data};
state5(cast, {act_quit, Quit}, Data) -> {stop, normal, Data}.

state6(enter, _OldState, _Data) -> keep_state_and_data;
state6(cast, {act_220, 220}, Data) -> {next_state, state7, Data}.

state7(enter, _OldState, _Data) -> keep_state_and_data;
state7(cast, {act_ehlo, Ehlo}, Data) -> {next_state, state8, Data};
state7(cast, {act_quit, Quit}, Data) -> {stop, normal, Data}.

state8(enter, _OldState, _Data) -> keep_state_and_data;
state8(cast, {act_250d, 250d}, Data) -> {next_state, state8, Data};
state8(cast, {act_250, 250}, Data) -> {next_state, state10, Data}.

state10(enter, _OldState, _Data) -> keep_state_and_data;
state10(cast, {act_auth, Auth}, Data) -> {next_state, state11, Data};
state10(cast, {act_quit, Quit}, Data) -> {stop, normal, Data}.

state11(enter, _OldState, _Data) -> keep_state_and_data;
state11(cast, {act_235, 235}, Data) -> {stop, normal, Data};
state11(cast, {act_535, 535}, Data) -> {next_state, state10, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_220(220) -> gen_statem:cast(?SERVER, {act_220, 220}).

act_235(235) -> gen_statem:cast(?SERVER, {act_235, 235}).

act_250(250) -> gen_statem:cast(?SERVER, {act_250, 250}).

act_250d(250d) -> gen_statem:cast(?SERVER, {act_250d, 250d}).

act_535(535) -> gen_statem:cast(?SERVER, {act_535, 535}).

act_auth(Auth) -> gen_statem:cast(?SERVER, {act_auth, Auth}).

act_ehlo(Ehlo) -> gen_statem:cast(?SERVER, {act_ehlo, Ehlo}).

act_quit(Quit) -> gen_statem:cast(?SERVER, {act_quit, Quit}).

act_startTls(StartTls) -> gen_statem:cast(?SERVER, {act_startTls, StartTls}).

stop() -> gen_statem:stop(?SERVER).