-module(basic_delay__ali_test).

-file("basic_delay__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_std,
          map => #{state1_std => #{send => #{before_5s => state2_unexpected_delay_state}}, state3_std => #{send => #{after_5s => stop_state}}},
          timeouts => #{state2_unexpected_delay_state => {5000, state3_std}}, resets => #{}}).

-define(PROTOCOL_SPEC, {act, s_before_5s, {delay, 5000, {act, s_after_5s, endP}}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    CoParty ! {self(), before_5s, Payload},
    timer:sleep(5000),
    CoParty ! {self(), after_5s, Payload},
    stopping(CoParty, Data).

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).