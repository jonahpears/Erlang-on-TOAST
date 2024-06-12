-module(advanced_mixed_choice_recv_first_loop__ali_test).

-file("advanced_mixed_choice_recv_first_loop__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_recv_after,
          map =>
              #{state2_recv_after => #{recv => #{first => state3_send_after}}, state3_send_after => #{send => #{second => stop_state}},
                state9_send_after => #{send => #{second => state1_unexpected_timer_start_state}}, state12_std => #{recv => #{third => state12_std}}},
          timeouts => #{state2_recv_after => {3000, state9_send_after}, state3_send_after => {t1, error_state}, state9_send_after => {t1, state12_std}},
          resets => #{init_state => #{t1 => 5000}}}).

-define(PROTOCOL_SPEC,
        {rec,
         "r1",
         {timer,
          "t1",
          5000,
          {act,
           r_first,
           {act, s_second, endP, aft, "t1", error},
           aft,
           3000,
           {act, s_second, {rvar, "r1"}, aft, "t1", {rec, "r2", {act, r_third, {rvar, "r2"}}}}}}}).

-include("stub.hrl").

-export([loop_standard_state/2, loop_timer_start_state/2, main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) -> loop_timer_start_state(CoParty, Data).

loop_timer_start_state(CoParty, Data1) ->
    {Data1, _TID_t1} = set_timer(t1, 5000, Data1),
    receive
        {CoParty, first, Payload_First} ->
            Data1_2 = save_msg(first, Payload_First, Data1),
            AwaitPayload = nonblocking_payload(fun get_state_3_payload/1, Data1_3, self(), get_timer(t1, Data1_3)),
            receive
                {AwaitPayload, ok, {Label, Payload}} ->
                    case Label of
                        second ->
                            CoParty ! {self(), second, Payload},
                            stopping(CoParty, Data1_4);
                        _ -> error(unexpected_label_selected)
                    end;
                {AwaitPayload, ko} ->
                    error(unspecified_error),
                    stopping(CoParty, Data1_4)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

loop_standard_state(CoParty, Data12) ->
    receive
        {CoParty, third, Payload_Third} ->
            Data12 = save_msg(third, Payload_Third, Data12),
            loop_standard_state(CoParty, Data12)
    end.

get_state_2_payload(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.