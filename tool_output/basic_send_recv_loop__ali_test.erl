-module(basic_send_recv_loop__ali_test).

-file("basic_send_recv_loop__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_std, map => #{state1_std => #{send => #{msgA => state2_std}}, state2_std => #{recv => #{msg1 => state2_std}}}, timeouts => #{},
          resets => #{}}).

-define(PROTOCOL_SPEC, {act, s_msgA, {rec, "a", {act, r_msg1, {rvar, "a"}}}}).

-include("stub.hrl").

-export([loop_standard_state/2, main/2, run/1, run/2]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    CoParty ! {self(), msgA, Payload},
    loop_standard_state(CoParty, Data).

loop_standard_state(CoParty, Data2) ->
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data2 = save_msg(msg1, Payload_Msg1, Data2),
            loop_standard_state(CoParty, Data2)
    end.