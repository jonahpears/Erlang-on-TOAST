-module('basic_select_after_timer_recv__ali_test.erl').

-file("basic_select_after_timer_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state,
          map =>
              #{state3_std => #{recv => #{msg1 => {stop_state, []}}}, state6_std => #{recv => #{msg2 => {stop_state, []}}},
                state8_std => #{recv => #{msg3 => {stop_state, []}}}, state10_std => #{recv => #{timeout => {stop_state, []}}},
                state2_select_after => #{send => #{msgA => {state3_std, []}, msgB => {state6_std, []}, msgC => {state8_std, []}}}},
          timeouts => #{}, resets => #{init_state => #{t => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC,
        {timer,
         "t",
         5000,
         {select, [{s_msgA, {act, r_msg1, endP}}, {s_msgB, {act, r_msg2, endP}}, {s_msgC, {act, r_msg3, endP}}], aft, "t", {act, r_timeout, endP}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    AwaitSelection = nonblocking_selection(fun select_state2/1, Data, self(), get_timer(t, Data)),
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

select_state2(Data) -> extend_with_functionality.

get_state_1_payload(Data) -> extend_with_functionality.