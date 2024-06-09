-module('basic_recv_after_timer_recv__ali_test.erl').

-file("basic_recv_after_timer_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => init_state,
          map => #{state2_recv_after => #{recv => #{before_5s => {stop_state, []}}}, state5_std => #{recv => #{after_5s => {stop_state, []}}}}, timeouts => #{},
          resets => #{init_state => #{t => 5000}}, timers => #{}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {act, r_before_5s, endP, aft, "t", {act, r_after_5s, endP}}}).

-include("stub.hrl").

-export([]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
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

get_state_1_payload(Data) -> extend_with_functionality.