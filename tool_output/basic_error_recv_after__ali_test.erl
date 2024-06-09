-module('basic_error_recv_after__ali_test.erl').

-file("basic_error_recv_after__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state, map => #{state1_recv_after => #{recv => #{msg => {stop_state, []}}}}, timeouts => #{}, resets => #{unresolved => #{}},
          timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg, endP, aft, 50, error}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg, Payload_Msg} ->
            Data = save_msg(msg, Payload_Msg, Data),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.