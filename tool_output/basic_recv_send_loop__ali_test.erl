-module('basic_recv_send_loop__ali_test.erl').

-file("basic_recv_send_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state, map => #{state1_std => #{recv => #{msg1 => {state2_std, []}}}, state2_std => #{send => #{msgA => {state2_std, []}}}},
          timeouts => #{}, resets => #{}, states_to_resolve => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg1, {rec, "a", {act, s_msgA, {rvar, "a"}}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data = save_msg(msg1, Payload_Msg1, Data),
            loop_standard_state(CoParty, Data)
    end.

loop_standard_state(CoParty, Data2) ->
    CoParty ! {self(), msgA, Payload},
    loop_standard_state(CoParty, Data2).