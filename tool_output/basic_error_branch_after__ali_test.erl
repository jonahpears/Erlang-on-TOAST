-module(basic_error_branch_after__ali_test).

-file("basic_error_branch_after__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_branch_after, map => #{state1_branch_after => #{recv => #{act_msgA => stop_state, act_msgB => stop_state}}},
          timeouts => #{state1_branch_after => {50, error_state}}, resets => #{}}).

-define(PROTOCOL_SPEC, {branch, [{msgA, endP}, {msgB, endP}], aft, 50, error}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, act_msgA, Payload_MsgA} ->
            Data = save_msg(act_msgA, Payload_MsgA, Data),
            stopping(CoParty, Data);
        {CoParty, act_msgB, Payload_MsgB} ->
            Data = save_msg(act_msgB, Payload_MsgB, Data),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.