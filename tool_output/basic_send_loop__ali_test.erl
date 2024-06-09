-module('basic_send_loop__ali_test.erl').

-file("basic_send_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{init => state1_std, map => #{state1_std => #{send => #{msgA => state1_std}}}, timeouts => #{}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {rec, "a", {act, s_msgA, {rvar, "a"}}}).

-include("stub.hrl").

-export([loop_standard_state/2, main/2, run/1, run/2]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) -> loop_standard_state(CoParty, Data).

loop_standard_state(CoParty, Data1) ->
    CoParty ! {self(), msgA, Payload},
    loop_standard_state(CoParty, Data1).