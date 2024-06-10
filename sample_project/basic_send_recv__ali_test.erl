-module('basic_send_recv__ali_test').

-file("basic_send_recv__ali_test.erl", 1).

-define(SHOW_ENABLED, true).

-define(SHOW(Str,Args,Data), 
  case ?SHOW_ENABLED of
    true -> printout(Data, ?SHOW_MONITORED++"~p, "++Str,[?FUNCTION_NAME]++Args);
    _ -> ok
  end).

-define(DO_SHOW(Str,Args,Data), printout(Data, ?SHOW_MONITORED++"~p, "++Str,[?FUNCTION_NAME]++Args)).

-define(SHOW_MONITORED, case ?MONITORED of true -> "(monitored) "; _ -> "" end).

-define(MONITORED, false).

-define(MONITOR_SPEC,
        #{init => state1_std, 
          map =>  #{state1_std => #{send => #{msg1 => state2_std}}, 
                    state2_std => #{recv => #{msgA => stop_state}}}, 
          timeouts => #{},
          resets => #{}, 
          timers => #{}}).

-export([run/1,run/2,stopping/2,start_link/0,start_link/1,init/1]).

-include("stub.hrl").

%% @doc 
start_link() -> start_link([]).

%% @doc 
start_link(Args) -> stub_start_link(Args).

%% @doc 
init(Args) -> 
  ?DO_SHOW("args:\n\t~p.",[Args],Args),

  {ok,Data} = stub_init(Args),
  ?DO_SHOW("data:\n\t~p.",[Data],Data),

  CoParty = maps:get(coparty_id,Data),
  SessionID = maps:get(session_id,Data),
  
  case (?MONITORED=:=true) of 
    true -> 
    %% add calls to specify behaviour of monitor here (?)
    
    ?DO_SHOW("finished setting options for monitor.",[],Data);
    _ -> ok
  end,

  %% wait for signal from session
  receive {SessionID, start} -> 
    ?SHOW("received start signal from session.",[],Data),
    % % run(CoPartyID, default_map()) 
    % {ok, Data2}
    run(CoParty, Data)
  end.

run(CoParty) -> run(CoParty, #{coparty_id => CoParty, timers => #{}, msgs => #{}}).

run(CoParty, Data) -> 
  ?DO_SHOW("Data:\n\t~p.\n",[Data],Data),
  main(CoParty, Data).

main(CoParty, Data) ->
    Data1 = Data,
    Payload_Msg1 = payload,
    ?SHOW("sent msg1: ~p.",[Payload_Msg1],Data1),
    CoParty ! {self(), msg1, Payload_Msg1},
    ?SHOW("waiting to recv msgA.",[],Data1),
    receive
        {CoParty, msgA, Payload_MsgA} ->
            Data2 = save_msg(msgA, Payload_MsgA, Data1),
            ?SHOW("recv'd msgA: ~p.",[Payload_MsgA],Data2),
            stopping(CoParty, Data2)
    end.

stopping(CoParty, Data) -> 
  ?SHOW("\nData:\t~p.",[Data],Data),
  stopping(normal, CoParty, Data).
  
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