-module(basic_recv__ali_test).

-file("basic_recv__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{init => state1_std, map => #{state1_std => #{recv => #{msg1 => stop_state}}}, timeouts => #{}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg1, endP}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data = save_msg(msg1, Payload_Msg1, Data),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).