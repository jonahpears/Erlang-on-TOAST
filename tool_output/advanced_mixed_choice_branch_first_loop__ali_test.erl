-module(advanced_mixed_choice_branch_first_loop__ali_test).

-file("advanced_mixed_choice_branch_first_loop__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_branch_after,
          map =>
              #{state2_branch_after => #{recv => #{first => state3_send_after, third => stop_state}},
                state3_send_after => #{send => #{second => state2_branch_after}},
                state10_select_after => #{send => #{fourth => state2_branch_after, fifth => stop_state}},
                state13_std => #{recv => #{sixth => state14_branch_after}},
                state14_branch_after => #{recv => #{seventh => state15_send_after, nine => stop_state}},
                state15_send_after => #{send => #{eighth => state2_branch_after}},
                state21_select_after => #{send => #{ten => state14_branch_after, eleven => stop_state}}, state24_std => #{recv => #{twleve => stop_state}}},
          timeouts => #{state2_branch_after => {3000, state10_select_after}, state14_branch_after => {3000, state21_select_after}},
          resets => #{init_state => #{t1 => 5000}},
          timers =>
              #{t1 =>
                    #{state3_send_after => error_state, state10_select_after => standard_state, state15_send_after => error_state,
                      state21_select_after => standard_state}}}).

-define(PROTOCOL_SPEC,
        {timer,
         "t1",
         5000,
         {rec,
          "r1",
          {branch,
           [{r_first, {act, s_second, {rvar, "r1"}, aft, "t1", error}}, {r_third, endP}],
           aft,
           3000,
           {select,
            [{s_fourth, {rvar, "r1"}}, {s_fifth, endP}],
            aft,
            "t1",
            {act,
             r_sixth,
             {rec,
              "r2",
              {branch,
               [{r_seventh, {act, s_eighth, {rvar, "r1"}, aft, "t1", error}}, {r_nine, endP}],
               aft,
               3000,
               {select, [{s_ten, {rvar, "r2"}}, {s_eleven, endP}], aft, "t1", {act, r_twleve, endP}}}}}}}}}).

-include("stub.hrl").

-export([loop_branch_after_state/2, main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t1} = set_timer(t1, 5000, Data),
    loop_branch_after_state(CoParty, Data).

loop_branch_after_state(CoParty, Data2) ->
    receive
        {CoParty, first, Payload_First} ->
            Data2 = save_msg(first, Payload_First, Data2),
            AwaitPayload = nonblocking_payload(fun get_state_3_payload/1, Data2_3, self(), get_timer(t1, Data2_3)),
            receive
                {AwaitPayload, ok, {Label, Payload}} ->
                    case Label of
                        second ->
                            CoParty ! {self(), second, Payload},
                            loop_branch_after_state(CoParty, Data2_3);
                        _ -> error(unexpected_label_selected)
                    end;
                {AwaitPayload, ko} ->
                    error(unspecified_error),
                    stopping(CoParty, Data2_7)
            end;
        {CoParty, third, Payload_Third} ->
            Data2 = save_msg(third, Payload_Third, Data2),
            stopping(CoParty, Data2_7)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

loop_branch_after_state(CoParty, Data14) ->
    receive
        {CoParty, nine, Payload_Nine} ->
            Data14 = save_msg(nine, Payload_Nine, Data14),
            stopping(CoParty, Data14_7);
        {CoParty, seventh, Payload_Seventh} ->
            Data14 = save_msg(seventh, Payload_Seventh, Data14),
            AwaitPayload = nonblocking_payload(fun get_state_15_payload/1, Data14_15, self(), get_timer(t1, Data14_15)),
            receive
                {AwaitPayload, ok, {Label, Payload}} ->
                    case Label of
                        eighth ->
                            CoParty ! {self(), eighth, Payload},
                            loop_branch_after_state(CoParty, Data14_15);
                        _ -> error(unexpected_label_selected)
                    end;
                {AwaitPayload, ko} ->
                    error(unspecified_error),
                    stopping(CoParty, Data14_7)
            end
    end.

select_state21(Data) -> extend_with_functionality.

get_state_14_payload(Data) -> extend_with_functionality.

get_state_10_payload(Data) -> extend_with_functionality.

select_state10(Data) -> extend_with_functionality.

get_state_2_payload(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.