-module(advanced_mixed_choice_send_first__ali_test).

-file("advanced_mixed_choice_send_first__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_send_after,
          map =>
              #{state2_send_after => #{send => #{first => state3_recv_after}}, state3_recv_after => #{recv => #{second => stop_state}},
                state9_recv_after => #{recv => #{second => stop_state}}, state11_std => #{send => #{third => stop_state}}},
          timeouts => #{state2_send_after => {3000, state9_recv_after}}, resets => #{init_state => #{t1 => 5000}},
          timers => #{t1 => #{state3_recv_after => error_state, state9_recv_after => standard_state}}}).

-define(PROTOCOL_SPEC,
        {timer, "t1", 5000, {act, s_first, {act, r_second, endP, aft, "t1", error}, aft, 3000, {act, r_second, endP, aft, "t1", {act, s_third, endP}}}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t1} = set_timer(t1, 5000, Data),
    AwaitPayload = nonblocking_payload(fun get_state_2_payload/1, Data, self(), 3000),
    receive
        {AwaitPayload, ok, {Label, Payload}} ->
            case Label of
                first ->
                    CoParty ! {self(), first, Payload},
                    receive
                        {CoParty, second, Payload_Second} ->
                            Data = save_msg(second, Payload_Second, Data),
                            stopping(CoParty, Data)
                    end;
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, second, Payload_Second} ->
                    Data = save_msg(second, Payload_Second, Data),
                    stopping(CoParty, Data)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_2_payload(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.