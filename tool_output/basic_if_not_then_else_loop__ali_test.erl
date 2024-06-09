-module(basic_if_not_then_else_loop__ali_test).

-file("basic_if_not_then_else_loop__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_if_else, map => #{state3_std => #{send => #{finished => stop_state}}, state6_std => #{send => #{data => state2_if_else}}},
          timeouts => #{}, resets => #{init_state => #{t => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {rec, "a", {if_not_timer, "t", {act, s_finished, endP}, else, {act, s_data, {rvar, "a"}}}}}).

-include("stub.hrl").

-export([loop_if_then_else_state/2, main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    loop_if_then_else_state(CoParty, Data).

loop_if_then_else_state(CoParty, Data2) ->
    receive
        {timeout, _TID_t, timer_t} ->
            CoParty ! {self(), data, Payload},
            loop_if_then_else_state(CoParty, Data2_6)
        after 0 -> CoParty ! {self(), finished, Payload}, stopping(CoParty, Data2_4)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).