-module('basic_error_select__ali_test.erl').

-file("basic_error_select__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_unexpected_select_state, map => #{state1_unexpected_select_state => #{send => #{act_msgA => stop_state, act_msgB => error_state}}},
          timeouts => #{}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {select, [{msgA, endP}, {msgB, error}]}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    case {Label, Payload} = get_state_1_payload([Data]) of
        act_msgA ->
            CoParty ! {self(), act_msgA, Payload},
            stopping(CoParty, Data);
        act_msgB ->
            CoParty ! {self(), act_msgB, Payload},
            error(unspecified_error),
            stopping(CoParty, Data);
        _ -> error(unexpected_label_selected)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).