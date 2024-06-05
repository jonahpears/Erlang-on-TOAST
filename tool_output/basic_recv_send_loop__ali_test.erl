-module('basic_recv_send_loop__ali_test.erl').

-file("basic_recv_send_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-define(PROTOCOL_SPEC, {act, r_msg1, {rec, "a", {act, s_msgA, {rvar, "a"}}}}).

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
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data1 = save_msg(msg1, Payload_Msg1, Data),
            Data2 = Data1,
            Payload_MsgA = payload,
            CoParty ! {self(), msgA, Payload_MsgA},
            loop_state2_std(CoParty, Data2)
    end.

loop_state2_std(CoParty, Data2) ->
    Data2_2 = Data2,
    Payload_MsgA = payload,
    CoParty ! {self(), msgA, Payload_MsgA},
    loop_state2_std(CoParty, Data2_2).