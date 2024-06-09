-module('basic_error_recv__ali_test.erl').

-file("basic_error_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{init => state1_std, map => #{state1_std => #{recv => #{msg => error_state}}}, timeouts => #{}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg, error}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg, Payload_Msg} ->
            Data = save_msg(msg, Payload_Msg, Data),
            error(unspecified_error),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).