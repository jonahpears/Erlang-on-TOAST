-module(basic_send_after_send__ali_test).

-file("basic_send_after_send__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_send_after, map => #{state5_std => #{send => #{after_5s => stop_state}}, state1_send_after => #{send => #{before_5s => stop_state}}},
          timeouts => #{state1_send_after => {5000, state5_std}}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {act, s_before_5s, endP, aft, 5000, {act, s_after_5s, endP}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    AwaitPayload = nonblocking_payload(fun get_state_1_payload/1, Data, self(), 5000),
    receive
        {AwaitPayload, ok, {Label, Payload}} ->
            case Label of
                before_5s ->
                    CoParty ! {self(), before_5s, Payload},
                    stopping(CoParty, Data);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            CoParty ! {self(), after_5s, Payload},
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).