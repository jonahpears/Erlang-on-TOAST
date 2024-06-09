-module('basic_recv_send__ali_test').

-file("basic_recv_send__ali_test.erl", 1).

-define(MONITORED, true).

-define(MONITOR_SPEC,
        #{init => state1_std, 
          map =>  #{state1_std => #{recv => #{msg1 => state2_std}}, 
                    state2_std => #{send => #{msgA => stop_state}}}, 
          timeouts => #{},
          resets => #{}, 
          timers => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg1, {act, s_msgA, endP}}).

-export([run/1,run/2,stopping/2,start_link/0,start_link/1,init/1]).

-include("stub.hrl").

%% @doc 
start_link() -> start_link([]).

%% @doc 
start_link(Args) -> stub_start_link(Args).

%% @doc 
init(Args) -> 
  printout("~p, args:\n\t ~p.",[?FUNCTION_NAME,Args]),
  stub_init(Args),self().

run(CoParty) -> run(CoParty, #{timers => #{}, msgs => #{}}).

run(CoParty, Data) -> main(CoParty, Data).

main(CoParty, Data) ->
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data1 = save_msg(msg1, Payload_Msg1, Data),
            Data2 = Data1,
            Payload = payload,
            CoParty ! {self(), msgA, Payload},
            stopping(CoParty, Data2)
    end.

stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).