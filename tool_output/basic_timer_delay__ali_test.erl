-module(basic_timer_delay__ali_test).

-file("basic_timer_delay__ali_test", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state2_std,
          map => #{state2_std => #{send => #{before_t => state3_unexpected_delay_state}}, state4_std => #{send => #{after_t => stop_state}}}, timeouts => #{},
          resets => #{init_state => #{t => 5000}}, timers => #{t => #{state3_unexpected_delay_state => standard_state}}}).

-define(PROTOCOL_SPEC, {timer, "t", 5000, {act, s_before_t, {delay, "t", {act, s_after_t, endP}}}}).

-include("stub.hrl").

-export([main/2, run/1, run/2, stopping/2, stopping/3]).

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    {Data, _TID_t} = set_timer(t, 5000, Data),
    CoParty ! {self(), before_t, Payload},
    case get_timer(t, Data) of
        {ok, TID_t} -> receive {timeout, TID_t, timer_t} -> ok end;
        {ko, no_such_timer} -> error(no_such_timer)
    end,
    CoParty ! {self(), after_t, Payload},
    stopping(CoParty, Data).

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

get_state_1_payload(Data) -> extend_with_functionality.