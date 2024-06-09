-module('basic_timer_before__ali_test.erl').

-file("basic_timer_before__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state, map => #{state2_std => #{send => #{after_t => {stop_state, []}}}}, timeouts => #{},
          resets => #{state1_unexpected_timer_start_state => #{t => 5000}, {state2_std, 1, resolved} => 5000},
          states_to_resolve => #{2 => {state1_unexpected_timer_start_state, 1, unresolved}}, timers => #{}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {act, s_after_t, endP}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    CoParty ! {self(), after_t, Payload},
    stopping(CoParty, Data).

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).