-module(exa_timer_timeout_selection).

-file("exa_timer_timeout_selection.erl", 1).

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
        #{init => state2_select_after,
          map =>  #{state2_select_after => #{send => #{ msg1 => state3_std, 
                                                        msg2 => state6_std, 
                                                        msg3 => state8_std}},
                    state3_std => #{recv => #{msgA => stop_state}}, 
                    state6_std => #{recv => #{msgB => stop_state}}, 
                    state8_std => #{recv => #{msgC => stop_state}},
                    state10_std => #{recv => #{timeout => stop_state}}},
          timeouts => #{}, 
          resets => #{init_state => #{t => 5000}}, 
          timers => #{t => #{state2_select_after => state10_std}}}).

-define(PROTOCOL_SPEC,
        {timer, "t", 5000, {
          select, [ {s_msg1, {act, r_msgA, endP}},
                    {s_msg2, {act, r_msgB, endP}}, 
                    {s_msg3, {act, r_msgC, endP}} ], 
          aft, "t", {act, r_timeout, endP}}}).

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

    %% set printout to be verbose
    CoParty ! {self(), setup_options, {printout, #{enabled=>true,verbose=>true,termination=>true}}},
    
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
  ?VSHOW("set timer t.",[],Data1),
  Selection = [msg1,msg2,msg3],
  AwaitSelection = nonblocking_selection(fun select_state2/1, {Selection,Data1}, self(), t, Data1),
  ?SHOW("waiting for selection to be made (from: ~p).",[Selection],Data1),
  receive
    {AwaitSelection, ok, {Label, Payload}} ->
      ?SHOW("selection made: ~p.",[Label],Data1),
      case Label of
        msg1 ->
          CoParty ! {self(), msg1, Payload},
          ?SHOW("sent msg1.",[],Data1),
          ?VSHOW("waiting to recv.",[],Data1),
          receive
            {CoParty, msgA, Payload_Msg1} ->
              Data2 = save_msg(msgA, Payload_Msg1, Data1),
              ?SHOW("recv'd msgA.",[],Data2),
              stopping(CoParty, Data2)
          end;
        msg2 ->
          CoParty ! {self(), msg2, Payload},
          ?SHOW("sent msg2.",[],Data1),
          ?VSHOW("waiting to recv.",[],Data1),
          receive
            {CoParty, msgB, Payload_Msg2} ->
              Data2 = save_msg(msgB, Payload_Msg2, Data1),
              ?SHOW("recv'd msgB.",[],Data2),
              stopping(CoParty, Data2)
          end;
        msg3 ->
          CoParty ! {self(), msg3, Payload},
          ?SHOW("sent msg3.",[],Data1),
          ?VSHOW("waiting to recv.",[],Data1),
          receive
            {CoParty, msgC, Payload_Msg3} ->
              Data2 = save_msg(msgC, Payload_Msg3, Data1),
              ?SHOW("recv'd msgC.",[],Data2),
              stopping(CoParty, Data2)
          end;
        _Err -> 
          ?SHOW("error, unexpected selection: ~p.",[_Err],Data1),
          error(unexpected_label_selected)
      end;
    {AwaitSelection, ko} ->
      ?VSHOW("selection returned ko. (probably took too long)",[],Data1),
      ?VSHOW("waiting to recv.",[],Data1),
      receive
        {CoParty, timeout, Payload_Timeout} ->
          Data2 = save_msg(timeout, Payload_Timeout, Data1),
          ?SHOW("recv'd timeout.",[],Data2),
          stopping(CoParty, Data2)
      end
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

% select_state2(Data) -> extend_with_functionality. %% <- default generated by tool
select_state2({Labels,Data}) when is_list(Labels) and is_map(Data) -> 

  %% randomally select from labels
  Label = lists:nth(rand:uniform(length(Labels)),Labels),
  ?VSHOW("selected (~p) out of (~p).",[Label,Labels],Data),

  %% function for getting payload
  Fun = fun({_Label,_Data}) -> {_Label, "payload_"++atom_to_list(_Label)} end,

  %% add functionality to make it potentially too long
  Offset = rand:uniform() * 3, %% number between 0-3
  Delay = floor(3000 + (Offset*1000)), %% delay between 3 and 6 seconds (timeout is at 5 seconds)
  ?VSHOW("delay of: ~pms.",[Delay],Data),
  timer:sleep(Delay),
  %% return if possible
  {Fun,{Label,Data}}.
%%
