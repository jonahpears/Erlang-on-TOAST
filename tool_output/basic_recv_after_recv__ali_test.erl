-module('basic_recv_after_recv__ali_test.erl').

-file("basic_recv_after_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_recv_after, map => #{state4_std => #{recv => #{after_5s => stop_state}}, state1_recv_after => #{recv => #{before_5s => stop_state}}},
          timeouts => #{state1_recv_after => {5000, state4_std}}, resets => #{}, timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_before_5s, endP, aft, 5000, {act, r_after_5s, endP}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, before_5s, Payload_Before_5s} ->
            Data = save_msg(before_5s, Payload_Before_5s, Data),
            stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).