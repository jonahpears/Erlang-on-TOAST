-module('basic_error_send_after__ali_test.erl').

-file("basic_error_send_after__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state, map => #{state1_send_after => #{send => #{msg => {stop_state, []}}}}, timeouts => #{}, resets => #{unresolved => #{}},
          timers => #{}}).

-define(PROTOCOL_SPEC, {act, s_msg, endP, aft, 50, error}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    AwaitPayload = nonblocking_payload(fun get_state_1_payload/1, Data, self(), 50),
    receive
        {AwaitPayload, ok, {Label, Payload}} ->
            case Label of
                msg ->
                    CoParty ! {self(), msg, Payload},
                    stopping(CoParty, Data);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            error(unspecified_error),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.