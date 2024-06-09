-module('basic_if_then__ali_test.erl').

-file("basic_if_then__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_if_else, map => #{state3_std => #{send => #{finished => stop_state}}}, timeouts => #{}, resets => #{init_state => #{t => 5000}},
          timers => #{}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {if_timer, "t", {act, s_finished, endP}}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    receive
        {timeout, _TID_t, timer_t} ->
            CoParty ! {self(), finished, Payload},
            stopping(CoParty, Data)
        after 0 -> error(unspecified_error), stopping(CoParty, Data)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.