-module('basic_recv_send').

-file("basic_recv_send.erl", 1).

%% @doc both the below used to determine whether a kind of log should be allowed to output to the terminal.
-define(SHOW_ENABLED, true).
-define(SHOW_VERBOSE, ?SHOW_ENABLED and true).

%% @doc macro for showing output to terminal, enabled only when ?SHOW_ENABLED is true.
%% also shows whether this is coming from a monitored process.
%% @see function `printout/3`.
-define(SHOW(Str,Args,Data), 
  case ?SHOW_ENABLED of
    true -> printout(Data, ?SHOW_MONITORED++"~p, "++Str,[?FUNCTION_NAME]++Args);
    _ -> ok
  end).

%% @doc similar to ?SHOW, but for outputs marked as verbose, using ?SHOW_VERBOSE.
%% @see macro `?SHOW`.
-define(VSHOW(Str,Args,Data), 
  case ?SHOW_VERBOSE of
    true -> printout(Data, ?SHOW_MONITORED++"~p, "++Str,[?FUNCTION_NAME]++Args);
    _ -> ok
  end).

%% @doc an override to output to terminal, regardless of ?SHOW_ENABLED.
%% @see macro `?SHOW`.
-define(DO_SHOW(Str,Args,Data), printout(Data, ?SHOW_MONITORED++"~p, "++Str,[?FUNCTION_NAME]++Args)).

%% @doc macro used in ?SHOW (and derived) for signalling if the process is monitored or not.
-define(SHOW_MONITORED, case ?MONITORED of true -> "(monitored) "; _ -> "" end).

%% @doc if true, process will be started with an inline monitor within the session.
%% note: monitored processes use ?MONITOR_SPEC as a protocol specification for communication.
%% @see function `stub_start_link/1` (in `stub.hrl`).
-define(MONITORED, true).

%% @doc protocol specification in FSM form, used by monitors.
%% @see macro `?PROTOCOL_SPEC', from which this FSM map is derived.
-define(MONITOR_SPEC,
        #{init => state1_std, 
          map =>  #{state1_std => #{recv => #{msg1 => state2_std}}, 
                    state2_std => #{send => #{msgA => stop_state}}}, 
          timeouts => #{},
          resets => #{}, 
          timers => #{}}).

%% @doc original input protocol specification used to generate this stub, and derive the FSM map used by monitors (when enabled).
%% protocol language used is a subset of TOAST. 
-define(PROTOCOL_SPEC, {act, r_msg1, {act, s_msgA, endP}}).

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
    
    CoParty ! {self(), ready, finished_setup},
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

  % %% signal to coparty that we are done setting up and ready to begin
  % CoParty ! {self(), ready, finished_setup},
  % ?DO_SHOW("sent (~p) ready signal.",[CoParty],Data),
  % receive {CoParty, ready, finished_setup} ->
  %   %% begin session
  %   run(CoParty, Data)
  % end.

run(CoParty) -> run(CoParty, #{coparty_id => CoParty, timers => #{}, msgs => #{}}).

run(CoParty, Data) -> 
  ?DO_SHOW("Data:\n\t~p.\n",[Data],Data),
  main(CoParty, Data).

main(CoParty, Data) ->
    ?SHOW("waiting to recv msg1.",[],Data),
    receive
        {CoParty, msg1, Payload_Msg1} ->
            Data1 = save_msg(msg1, Payload_Msg1, Data),
            ?SHOW("recv'd msg1: ~p.",[Payload_Msg1],Data1),
            Data2 = Data1,
            Payload = payload,
            ?SHOW("sent msgA: ~p.",[Payload],Data2),
            CoParty ! {self(), msgA, Payload},
            stopping(CoParty, Data2)
    end.

stopping(CoParty, Data) -> 
  ?SHOW("\nData:\t~p.",[Data],Data),
  stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).