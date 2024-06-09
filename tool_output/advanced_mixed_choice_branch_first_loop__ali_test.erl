-module('advanced_mixed_choice_branch_first_loop__ali_test.erl').

-file("advanced_mixed_choice_branch_first_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_unexpected_timer_start_state,
          map =>
              #{state2_select_after => #{send => #{receive_first => state3_send_after, receive_third => stop_state}},
                state3_send_after => #{send => #{second => state2_select_after}}, state13_std => #{recv => #{sixth => state14_select_after}},
                state10_branch_after => #{recv => #{send_fourth => state2_select_after, send_fifth => stop_state}},
                state14_select_after => #{send => #{receive_seventh => state15_send_after, receive_nine => stop_state}},
                state15_send_after => #{send => #{eighth => state2_select_after}},
                state21_branch_after => #{recv => #{send_ten => state14_select_after, send_eleven => stop_state}},
                state24_std => #{recv => #{twleve => stop_state}}},
          timeouts => #{}, resets => #{init_state => #{t1 => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC,
        {timer,
         "t1",
         5000,
         {rec,
          "r1",
          {select,
           [{r_first, {act, s_second, {rvar, "r1"}, aft, "t1", error}}, {r_third, endP}],
           aft,
           3000,
           {branch,
            [{s_fourth, {rvar, "r1"}}, {s_fifth, endP}],
            aft,
            "t1",
            {act,
             r_sixth,
             {rec,
              "r2",
              {select,
               [{r_seventh, {act, s_eighth, {rvar, "r1"}, aft, "t1", error}}, {r_nine, endP}],
               aft,
               3000,
               {branch, [{s_ten, {rvar, "r2"}}, {s_eleven, endP}], aft, "t1", {act, r_twleve, endP}}}}}}}}}).

-include("stub.hrl").

-export([loop_select_after_state/2, main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t1} = set_timer(t1, 5000, Data),
    loop_select_after_state(CoParty, Data).

loop_select_after_state(CoParty, Data2) ->
    AwaitSelection = nonblocking_selection(fun select_state2/1, Data2, self(), 3000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                receive_first ->
                    CoParty ! {self(), receive_first, Payload},
                    AwaitPayload = nonblocking_payload(fun get_state_3_payload/1, Data2_3, self(), get_timer(t1, Data2_3)),
                    receive
                        {AwaitPayload, ok, {Label, Payload}} ->
                            case Label of
                                second ->
                                    CoParty ! {self(), second, Payload},
                                    loop_select_after_state(CoParty, Data2_3);
                                _ -> error(unexpected_label_selected)
                            end;
                        {AwaitPayload, ko} ->
                            error(unspecified_error),
                            stopping(CoParty, Data2_7)
                    end;
                receive_third ->
                    CoParty ! {self(), receive_third, Payload},
                    stopping(CoParty, Data2_7);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, send_fifth, Payload_Fifth} ->
                    Data2_10 = save_msg(send_fifth, Payload_Fifth, Data2),
                    stopping(CoParty, Data2_7);
                {CoParty, send_fourth, Payload_Fourth} ->
                    Data2_10 = save_msg(send_fourth, Payload_Fourth, Data2),
                    loop_select_after_state(CoParty, Data2_10)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

loop_select_after_state(CoParty, Data14) ->
    AwaitSelection = nonblocking_selection(fun select_state14/1, Data14, self(), 3000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                receive_nine ->
                    CoParty ! {self(), receive_nine, Payload},
                    stopping(CoParty, Data14_7);
                receive_seventh ->
                    CoParty ! {self(), receive_seventh, Payload},
                    AwaitPayload = nonblocking_payload(fun get_state_15_payload/1, Data14_15, self(), get_timer(t1, Data14_15)),
                    receive
                        {AwaitPayload, ok, {Label, Payload}} ->
                            case Label of
                                eighth ->
                                    CoParty ! {self(), eighth, Payload},
                                    loop_select_after_state(CoParty, Data14_15);
                                _ -> error(unexpected_label_selected)
                            end;
                        {AwaitPayload, ko} ->
                            error(unspecified_error),
                            stopping(CoParty, Data14_7)
                    end;
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, send_eleven, Payload_Eleven} ->
                    Data14_21 = save_msg(send_eleven, Payload_Eleven, Data14),
                    stopping(CoParty, Data14_7);
                {CoParty, send_ten, Payload_Ten} ->
                    Data14_21 = save_msg(send_ten, Payload_Ten, Data14),
                    loop_select_after_state(CoParty, Data14_21)
            end
    end.

get_state_14_payload(Data) -> extend_with_functionality.

select_state14(Data) -> extend_with_functionality.

get_state_10_payload(Data) -> extend_with_functionality.

get_state_2_payload(Data) -> extend_with_functionality.

select_state2(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.