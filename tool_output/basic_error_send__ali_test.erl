-module('basic_error_send__ali_test.erl').

-file("basic_error_send__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state, map => #{state1_std => #{send => #{msg => {state2_unexpected_error_state, []}}}}, timeouts => #{}, resets => #{unresolved => #{}},
          timers => #{}}).

-define(PROTOCOL_SPEC, {act, s_msg, error}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    CoParty ! {self(), msg, Payload},
    error(unspecified_error),
    stopping(CoParty, Data).

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).