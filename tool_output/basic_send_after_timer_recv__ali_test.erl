-module('basic_send_after_timer_recv__ali_test.erl').

-file("basic_send_after_timer_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state,
          map => #{state2_send_after => #{send => #{before_5s => {stop_state, []}}}, state6_std => #{recv => #{after_5s => {stop_state, []}}}}, timeouts => #{},
          resets => #{init_state => #{t => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {act, s_before_5s, endP, aft, "t", {act, r_after_5s, endP}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    AwaitPayload = nonblocking_payload(fun get_state_2_payload/1, Data, self(), get_timer(t, Data)),
    receive
        {AwaitPayload, ok, {Label, Payload}} ->
            case Label of
                before_5s ->
                    CoParty ! {self(), before_5s, Payload},
                    stopping(CoParty, Data);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, after_5s, Payload_After_5s} ->
                    Data = save_msg(after_5s, Payload_After_5s, Data),
                    stopping(CoParty, Data)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.