-module('basic_send_recv__ali_test').

-file("basic_send_recv__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, 
  #{ init => state1_send_msg1,
     timeouts => #{  },
     map => #{ state1_send_msg1 => #{send => #{msg1 => state2_recv_msgA}},
               state2_recv_msgA => #{recv => #{msgA => stop_state}}          } }).

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

%% @doc Adds default empty list for Data.
%% @see run/2.
run(CoParty) -> run(CoParty, []).

%% @doc Called immediately after a successful initialisation.
%% Add any setup functionality here, such as for the contents of Data.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
run(CoParty, Data) -> printout("~p, CoParty: ~p.",[?FUNCTION_NAME,CoParty]),main(CoParty, Data). %% add any init/start preperations below, before entering main

main(CoParty, Data) ->
    Data1 = Data,
    Payload_MsgA = payload,
    CoParty ! {self(), msg1, Payload_MsgA},
    receive
        {CoParty, msgA, Payload_Msg1} ->
            Data2 = save_msg(msgA, Payload_Msg1, Data1),
            stopping(CoParty, Data2)
    end.

%% @doc Adds default reason 'normal' for stopping.
%% @see stopping/3.
stopping(CoParty, Data) -> printout("~p.",[?FUNCTION_NAME]), stopping(normal, CoParty, Data).

%% @doc Adds default reason 'normal' for stopping.
%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}.
stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
%% @doc stopping with error.
%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
%% @doc Adds default Details to error.
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
%% @doc stopping with Unexpected Reason.
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).