-module('advanced_mixed_choice_select_first__ali_test.erl').

-file("advanced_mixed_choice_select_first__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state,
          map =>
              #{state2_select_after => #{send => #{first => {state3_recv_after, []}, third => {stop_state, []}}},
                state3_recv_after => #{recv => #{second => {stop_state, []}}}, state12_std => #{send => #{sixth => {stop_state, []}}},
                state9_branch_after => #{recv => #{fourth => {stop_state, []}, fifth => {stop_state, []}}}},
          timeouts => #{}, resets => #{init_state => #{t1 => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC,
        {timer,
         "t1",
         5000,
         {select,
          [{s_first, {act, r_second, endP, aft, "t1", error}}, {s_third, endP}],
          aft,
          3000,
          {branch, [{r_fourth, endP}, {r_fifth, endP}], aft, "t1", {act, s_sixth, endP}}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t1} = set_timer(t1, 5000, Data),
    AwaitSelection = nonblocking_selection(fun select_state2/1, Data, self(), 3000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                first ->
                    CoParty ! {self(), first, Payload},
                    receive
                        {CoParty, second, Payload_Second} ->
                            Data = save_msg(second, Payload_Second, Data),
                            stopping(CoParty, Data)
                    end;
                third ->
                    CoParty ! {self(), third, Payload},
                    stopping(CoParty, Data);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, fifth, Payload_Fifth} ->
                    Data = save_msg(fifth, Payload_Fifth, Data),
                    stopping(CoParty, Data);
                {CoParty, fourth, Payload_Fourth} ->
                    Data = save_msg(fourth, Payload_Fourth, Data),
                    stopping(CoParty, Data)
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