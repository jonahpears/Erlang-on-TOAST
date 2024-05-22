-module('_ali_test').

-file("_ali_test", 1).

-define(MONITORED, false).

-export([loop_state1_std/2, main/2, run/1, run/2]).

%% @doc Adds default empty list for Data.
%% @see run/2.
run(CoParty) -> run(CoParty, []).

%% @doc Called immediately after a successful initialisation.
%% Add any setup functionality here, such as for the contents of Data.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
run(CoParty, Data) -> main(CoParty, Data). %% add any init/start preperations below, before entering main

main(CoParty, Data) ->
    Data1 = Data,
    receive {CoParty, msg1, Payload_Msg1} -> loop_state1_std(CoParty, Data1) end.

loop_state1_std(CoParty, Data1) ->
    Data1_1 = Data1,
    receive {CoParty, msg1, Payload_Msg1} -> loop_state1_std(CoParty, Data1_1) end.