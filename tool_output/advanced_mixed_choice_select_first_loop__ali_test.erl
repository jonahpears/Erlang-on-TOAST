-module(advanced_mixed_choice_select_first_loop__ali_test).

-file("advanced_mixed_choice_select_first_loop__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_select_after,
          map =>
              #{state2_select_after => #{send => #{first => state3_recv_after, third => stop_state}},
                state3_recv_after => #{recv => #{second => state2_select_after}}, state12_std => #{send => #{sixth => state13_select_after}},
                state9_branch_after => #{recv => #{fourth => state2_select_after, fifth => stop_state}},
                state13_select_after => #{send => #{seventh => state14_recv_after, nine => stop_state}},
                state14_recv_after => #{recv => #{eighth => state2_select_after}},
                state19_branch_after => #{recv => #{ten => state13_select_after, eleven => stop_state}}, state22_std => #{send => #{twleve => stop_state}}},
          timeouts => #{state2_select_after => {3000, state9_branch_after}, state13_select_after => {3000, state19_branch_after}},
          resets => #{init_state => #{t1 => 5000}},
          timers =>
              #{t1 =>
                    #{state3_recv_after => error_state, state9_branch_after => standard_state, state14_recv_after => error_state,
                      state19_branch_after => standard_state}}}).

-define(PROTOCOL_SPEC,
        {timer,
         "t1",
         5000,
         {rec,
          "r1",
          {select,
           [{s_first, {act, r_second, {rvar, "r1"}, aft, "t1", error}}, {s_third, endP}],
           aft,
           3000,
           {branch,
            [{r_fourth, {rvar, "r1"}}, {r_fifth, endP}],
            aft,
            "t1",
            {act,
             s_sixth,
             {rec,
              "r2",
              {select,
               [{s_seventh, {act, r_eighth, {rvar, "r1"}, aft, "t1", error}}, {s_nine, endP}],
               aft,
               3000,
               {branch, [{r_ten, {rvar, "r2"}}, {r_eleven, endP}], aft, "t1", {act, s_twleve, endP}}}}}}}}}).

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
                first ->
                    CoParty ! {self(), first, Payload},
                    receive
                        {CoParty, second, Payload_Second} ->
                            Data2_3 = save_msg(second, Payload_Second, Data2),
                            loop_select_after_state(CoParty, Data2_3)
                    end;
                third ->
                    CoParty ! {self(), third, Payload},
                    stopping(CoParty, Data2_6);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, fifth, Payload_Fifth} ->
                    Data2_9 = save_msg(fifth, Payload_Fifth, Data2),
                    stopping(CoParty, Data2_6);
                {CoParty, fourth, Payload_Fourth} ->
                    Data2_9 = save_msg(fourth, Payload_Fourth, Data2),
                    loop_select_after_state(CoParty, Data2_9)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

loop_select_after_state(CoParty, Data13) ->
    AwaitSelection = nonblocking_selection(fun select_state13/1, Data13, self(), 3000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                nine ->
                    CoParty ! {self(), nine, Payload},
                    stopping(CoParty, Data13_6);
                seventh ->
                    CoParty ! {self(), seventh, Payload},
                    receive
                        {CoParty, eighth, Payload_Eighth} ->
                            Data13_14 = save_msg(eighth, Payload_Eighth, Data13),
                            loop_select_after_state(CoParty, Data13_14)
                    end;
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, eleven, Payload_Eleven} ->
                    Data13_19 = save_msg(eleven, Payload_Eleven, Data13),
                    stopping(CoParty, Data13_6);
                {CoParty, ten, Payload_Ten} ->
                    Data13_19 = save_msg(ten, Payload_Ten, Data13),
                    loop_select_after_state(CoParty, Data13_19)
            end
    end.

get_state_13_payload(Data) -> extend_with_functionality.

select_state13(Data) -> extend_with_functionality.

get_state_9_payload(Data) -> extend_with_functionality.

get_state_2_payload(Data) -> extend_with_functionality.

select_state2(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.