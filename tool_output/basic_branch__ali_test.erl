-module('basic_branch__ali_test.erl').

-file("basic_branch__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-define(PROTOCOL_SPEC, {branch, [{r_msg1, {act, s_msgA, endP}}, {r_msg2, {act, s_msgB, endP}}, {r_msg3, {act, s_msgC, endP}}]}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data = save_msg(msg1, Payload_Msg1, Data),
            CoParty ! {self(), msgA, Payload},
            stopping(CoParty, Data);
        {CoParty, msg2, Payload_Msg2} ->
            Data = save_msg(msg2, Payload_Msg2, Data),
            CoParty ! {self(), msgB, Payload},
            stopping(CoParty, Data);
        {CoParty, msg3, Payload_Msg3} ->
            Data = save_msg(msg3, Payload_Msg3, Data),
            CoParty ! {self(), msgC, Payload},
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).