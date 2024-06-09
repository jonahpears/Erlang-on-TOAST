-module('advanced_mixed_choice_select_first_loop__ali_test.erl').

-file("advanced_mixed_choice_select_first_loop__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-define(PROTOCOL_SPEC,
        {rec,
         "r1",
         {timer,
          "t1",
          5000,
          {rec,
           "r2",
           {select,
            [{s_first, {act, r_second, {rvar, "r1"}, aft, "t1", error}}, {s_third, endP}],
            aft,
            3000,
            {branch, [{r_fourth, {rvar, "r2"}}, {r_fifth, endP}], aft, "t1", {act, s_sixth, endP}}}}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) -> loop_timer_start_state(CoParty, Data).

loop_timer_start_state(CoParty, Data1) ->
    {Data1, _TID_t1} = set_timer(t1, 5000, Data1),
    loop_select_after_state(CoParty, Data1_2).

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

get_state_2_payload(Data) -> extend_with_functionality.

select_state2(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.