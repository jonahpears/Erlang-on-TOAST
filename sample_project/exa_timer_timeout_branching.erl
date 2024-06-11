-module(exa_timer_timeout_branching).

-file("exa_timer_timeout_branching.erl", 1).

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
-define(MONITORED, false).

%% @doc protocol specification in FSM form, used by monitors.
%% @see macro `?PROTOCOL_SPEC', from which this FSM map is derived.
-define(MONITOR_SPEC,
        #{init => state2_branch_after,
          map =>  #{state2_branch_after => #{recv =>  #{msg1 => state3_std, 
                                                        msg2 => state6_std, 
                                                        msg3 => state8_std}}, 
                    state3_std => #{send => #{msgA => stop_state}},
                    state6_std => #{send => #{msgB => stop_state}},
                    state8_std => #{send => #{msgC => stop_state}},
                    state10_std => #{send => #{timeout => stop_state}}},
          timeouts => #{}, 
          resets => #{init_state => #{t => 5000}}, 
          timers => #{t => #{state2_branch_after => state10_std}}}).

-define(PROTOCOL_SPEC,
        {timer, "t", 5000, {
          branch, [ {r_msg1, {act, s_msgA, endP}}, 
                    {r_msg2, {act, s_msgB, endP}}, 
                    {r_msg3, {act, s_msgC, endP}} ], 
          aft, "t", {act, s_timeout, endP}}}).

-export([run/1,run/2,stopping/2,start_link/0,start_link/1,init/1]).

-include("stub.hrl").

%% @doc 
start_link() -> start_link([]).

%% @doc 
start_link(Args) -> stub_start_link(Args).

%% @doc 
init(Args) -> 
  ?VSHOW("args:\n\t~p.",[Args],Args),

  {ok,Data} = stub_init(Args),
  ?VSHOW("data:\n\t~p.",[Data],Data),

  CoParty = maps:get(coparty_id,Data),
  SessionID = maps:get(session_id,Data),
  
  case (?MONITORED=:=true) of 
    true -> 
    %% add calls to specify behaviour of monitor here (?)
    
    CoParty ! {self(), ready, finished_setup},
    ?VSHOW("finished setting options for monitor.",[],Data);
    _ -> ok
  end,

  %% wait for signal from session
  receive {SessionID, start} -> 
    ?SHOW("received start signal from session.",[],Data),
    % % run(CoPartyID, default_map()) 
    % {ok, Data2}
    run(CoParty, Data)
  end.

%% @doc 
run(CoParty) -> run(CoParty, #{coparty_id => CoParty, timers => #{}, msgs => #{}}).

%% @doc 
run(CoParty, Data) -> 
  ?DO_SHOW("Data:\n\t~p.\n",[Data],Data),
  main(CoParty, Data).

main(CoParty, Data) ->
  Data1 = set_timer(t, 5000, Data),
  TID_t = get_timer(t,Data1),
  ?VSHOW("set timer t (~p).",[TID_t],Data1),
  ?VSHOW("waiting to recv.",[],Data1),
  receive
    {CoParty, msg1, Payload_Msg1} ->
      Data2 = save_msg(msg1, Payload_Msg1, Data1),
      Payload = "payload_msgA",
      CoParty ! {self(), msgA, Payload},
      ?SHOW("sent msgA.",[],Data2),
      stopping(CoParty, Data2);
    {CoParty, msg2, Payload_Msg2} ->
      Data2 = save_msg(msg2, Payload_Msg2, Data1),
      Payload = "payload_msgB",
      CoParty ! {self(), msgB, Payload},
      ?SHOW("sent msgB.",[],Data2),
      stopping(CoParty, Data2);
    {CoParty, msg3, Payload_Msg3} ->
      Data2 = save_msg(msg3, Payload_Msg3, Data1),
      Payload = "payload_msgC",
      CoParty ! {self(), msgC, Payload},
      ?SHOW("sent msg3.",[],Data2),
      stopping(CoParty, Data2);
    {timeout, TID_t, t} ->
      ?DO_SHOW("timeout triggered on timer t.",[],Data1),
      Payload = "payload_timeout",
      CoParty ! {self(), timeout, Payload},
      ?SHOW("sent timeout.",[],Data1)
  end.

stopping(CoParty, Data) -> 
  ?VSHOW("stopping,\nData:\t~p.",[Data],Data),
  stopping(normal, CoParty, Data).

stopping(normal = _Reason, _CoParty, _Data) -> 
  ?SHOW("stopping normally.",[],_Data),
  exit(normal);

stopping({error, Reason, Details}, _CoParty, _Data) 
when is_atom(Reason) -> 
  ?SHOW("error, stopping...\nReason:\t~p,\nDetails:\t~p,\nCoParty:\t~p,\nData:\t~p.",[Reason,Details,_CoParty,_Data],_Data),
  erlang:error(Reason, Details);

stopping({error, Reason}, CoParty, Data) 
when is_atom(Reason) -> 
  ?SHOW("error, stopping...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.",[Reason,CoParty,Data],Data),
  stopping({error, Reason, []}, CoParty, Data);

stopping(Reason, _CoParty, _Data) 
when is_atom(Reason) -> 
  ?SHOW("unexpected stop...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.",[Reason,_CoParty,_Data],_Data),
  exit(Reason).
