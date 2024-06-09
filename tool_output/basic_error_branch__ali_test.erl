-module(basic_error_branch__ali_test).

-file("basic_error_branch__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_unexpected_branch_state, map => #{state1_unexpected_branch_state => #{recv => #{act_msgA => stop_state, act_msgB => error_state}}},
          timeouts => #{}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {branch, [{msgA, endP}, {msgB, error}]}).

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
            error(unspecified_error),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).