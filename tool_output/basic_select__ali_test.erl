-module('basic_select__ali_test.erl').

-file("basic_select__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state,
          map =>
              #{state2_std => #{recv => #{msg1 => {stop_state, []}}}, state5_std => #{recv => #{msg2 => {stop_state, []}}},
                state7_std => #{recv => #{msg3 => {stop_state, []}}},
                state1_unexpected_select_state => #{send => #{msgA => {state2_std, []}, msgB => {state5_std, []}, msgC => {state7_std, []}}}},
          timeouts => #{}, resets => #{unresolved => #{}}, timers => #{}}).

-define(PROTOCOL_SPEC, {select, [{s_msgA, {act, r_msg1, endP}}, {s_msgB, {act, r_msg2, endP}}, {s_msgC, {act, r_msg3, endP}}]}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    case {Label, Payload} = get_state_1_payload([Data]) of
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
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).