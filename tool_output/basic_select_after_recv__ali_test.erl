-module('basic_select_after_recv__ali_test.erl').

-file("basic_select_after_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_select_after,
          map =>
              #{state2_std => #{recv => #{msg1 => stop_state}}, state5_std => #{recv => #{msg2 => stop_state}}, state7_std => #{recv => #{msg3 => stop_state}},
                state9_std => #{recv => #{timeout => stop_state}},
                state1_select_after => #{send => #{msgA => state2_std, msgB => state5_std, msgC => state7_std}}},
          timeouts => #{state1_select_after => {5000, state9_std}}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC,
        {select, [{s_msgA, {act, r_msg1, endP}}, {s_msgB, {act, r_msg2, endP}}, {s_msgC, {act, r_msg3, endP}}], aft, 5000, {act, r_timeout, endP}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    AwaitSelection = nonblocking_selection(fun select_state1/1, Data, self(), 5000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                msgA ->
                    CoParty ! {self(), msgA, Payload},
                    receive
                        {CoParty, msg1, Payload_Msg1} ->
                            Data = save_msg(msg1, Payload_Msg1, Data),
                            stopping(CoParty, Data)
                    end;
                msgB ->
                    CoParty ! {self(), msgB, Payload},
                    receive
                        {CoParty, msg2, Payload_Msg2} ->
                            Data = save_msg(msg2, Payload_Msg2, Data),
                            stopping(CoParty, Data)
                    end;
                msgC ->
                    CoParty ! {self(), msgC, Payload},
                    receive
                        {CoParty, msg3, Payload_Msg3} ->
                            Data = save_msg(msg3, Payload_Msg3, Data),
                            stopping(CoParty, Data)
                    end;
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, timeout, Payload_Timeout} ->
                    Data = save_msg(timeout, Payload_Timeout, Data),
                    stopping(CoParty, Data)
            end
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

select_state1(Data) -> extend_with_functionality.