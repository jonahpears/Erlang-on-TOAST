-module('basic_if_then_loop__ali_test.erl').

-file("basic_if_then_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-include("stub.hrl").

-export([]).

%% @doc Adds default empty list for Data.
%% @see run/2.
run(CoParty) -> run(CoParty, []).

%% @doc Called immediately after a successful initialisation.
%% Add any setup functionality here, such as for the contents of Data.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
run(CoParty, Data) -> main(CoParty, Data). %% add any init/start preperations below, before entering main

main(CoParty, Data) ->
    {Data1, _TID_t} = set_timer(t, 5000, Data),
    receive
        {timeout, t, {timer, t}} ->
            Payload_Finished = payload,
            CoParty ! {self(), finished, Payload_Finished},
            loop_state2_if(CoParty, Data1_3)
        after 0 -> error(urgent_lower_bound_violation)
    end.

loop_state2_if(CoParty, Data2) ->
    receive
        {timeout, t, {timer, t}} ->
            Payload_Finished = payload,
            CoParty ! {self(), finished, Payload_Finished},
            loop_state2_if(CoParty, Data1_3)
        after 0 -> error(urgent_lower_bound_violation)
    end.