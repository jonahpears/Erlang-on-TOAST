-module(test_template).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([
    start_link/0,
    init/1,
    callback_mode/0,
    stop/0,
    terminate/3,
    state1/3
        ]).


%% basic template necessities
start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

terminate(_Reason, _State, _Data) -> ok.

stop() -> gen_statem:stop(?SERVER).


%% states
state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {event, _Var}, Data) -> {next_state, state2, Data}.


%% state actions
event(Var) -> gen_statem:cast(?SERVER, {act_fail, Var}).
