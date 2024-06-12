-export([ stub_start_link/0,
          stub_start_link/1,
          stub_init/1,
          send/3,
          recv/3,
          send_before/4, 
          recv_before/4,
          set_timer/3,
          get_timer/2,
          default_map/0,
          save_msg/3,
          get_msg/2,
          get_msgs/2,
          nonblocking_selection/5,
          nonblocking_payload/5
        ]).
-compile(nowarn_export_all).


-include_lib("stdlib/include/assert.hrl").

-include("printout.hrl").

%% @doc determines if start_link/0/1 creates monitor in the same node
-ifdef(MONITORED).
-else.
-define(MONITORED, false).
-endif.

%% @doc if monitored, put specification here (i.e.: module:spec_fun() or spec_map=#{...}) OR, pass in with Args list
-ifdef(MONITOR_SPEC).
-else.
-define(MONITOR_SPEC, #{}).
-endif.

%% @doc 
stub_start_link() -> stub_start_link([]).

%% @doc 
stub_start_link(Args) when ?MONITORED=:=true ->
  Params = maps:from_list(Args),
  ?SHOW("Params:\n\t~p.",[Params],Params),

  %% if ?MONITORED, then either MONITOR_SPEC is map, or params contains 
  % case maps:size(?MONITOR_SPEC)>0 of 
  case is_boolean(?MONITOR_SPEC) of 
    true -> %% if monitor spec not in this file, check in param map
      ?assert(maps:is_key(monitor_spec,Params)), 
      MonitorSpec = maps:get(monitor_spec, Params);
    _ -> MonitorSpec = ?MONITOR_SPEC
  end,

  Name = maps:get(name, maps:get(role, Params)),
  MonitorName = list_to_atom("mon_"++atom_to_list(Name)),

  MonitorArgs = maps:to_list(maps:put(role,maps:put(name,MonitorName,maps:get(role,Params)),Params)),

  PID = self(),

  %% spawn monitor within same node 
  MonitorID = erlang:spawn_link(node(), gen_monitor, start_link, [MonitorArgs++[{fsm,MonitorSpec},{sus_init_id, PID},{sus_id, erlang:spawn_link(?MODULE, init, [Args++[{sus_init_id, PID}]])}]]),

  ?SHOW("MonitorID: ~p.",[MonitorID],Params),
  {ok, MonitorID};
%%

%% @doc normal unmonitored start_link
stub_start_link(Args) -> 
  Params = maps:from_list(Args),
  ?VSHOW("Params:\n\t~p.",[Args],Params),
  InitID = erlang:spawn_link(?MODULE, init, [Args]),
  
  ?VSHOW("ID: ~p.",[InitID],Params),
  {ok, InitID}.
%%

%% @doc called after start_link returns
stub_init(Args) when ?MONITORED=:=true ->
  Params = maps:from_list(Args),
  ?VSHOW("args:\n\t~p.",[Args],Params),

  %% unpack from param map
  Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  ?assert(is_pid(SessionID)),

  ?VSHOW("session_id: ~p.",[SessionID],Params),

  Data = maps:put(session_id,SessionID,default_map()),
  Data1 = maps:put(role,Role,Data),

  %% receive message from monitor
  SusInitID = maps:get(sus_init_id,Params),
  ?VSHOW("waiting to recv real monitor ID.",[],Data1),
  receive 
    {{MonitorID, is_monitor},{SusInitID, proof_init_id},{in_session,SessionID}} ->
      Data2 = maps:put(coparty_id,MonitorID,Data1),
      ?VSHOW("recv'd monitor id: ~p.",[MonitorID],Data2),
      {ok, Data2}
  end;
%%

%% @doc init function for unmonitored process.
stub_init(Args) ->
  Params = maps:from_list(Args),
  ?VSHOW("Params:\n\t~p.",[Params],Params),
  PID = self(),

  %% unpack from param map
  Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  ?VSHOW("session_id: ~p.",[SessionID],Params),
  ?assert(is_pid(SessionID)),

  SessionID ! {self(),role,Role},

  Data = maps:put(session_id,SessionID,default_map()),
  Data1 = maps:put(role,maps:get(role,Params),Data),

  % %% wait to receive coparty id from session
  ?VSHOW("waiting to receive CoPartyID from session.",[],Data1),
  receive {SessionID, coparty_id, CoPartyID} ->
    ?VSHOW("received CoPartyID (~p), now waiting for init message from CoParty.",[CoPartyID],Data1),
    Data2 = maps:put(coparty_id,CoPartyID,Data1),
    %% exchange init message
    CoPartyID ! {PID, init},
    receive {CoPartyID, init} ->
      ?VSHOW("now telling session ready, and waiting for signal to start.",[],Data1),
      %% tell session ready to proceed
      SessionID ! {self(), ready},
      {ok, Data2}
    end
  end.
%%

%% @docs default map for stubs
default_map() -> #{timers=>maps:new(),msgs=>maps:new(),coparty_id=>undefined}.

%%%%%%%%%%%%%%%%%%%%%
%%% timer management
%%%%%%%%%%%%%%%%%%%%%

%% @docs tells the monitor of the process to set the timer of Name to Duration.
%% if the timer is already set, then the timer is reset.
%% similar to the other definitions of `set_timer`, except goes via the monitor.
set_timer(Name, Duration, #{coparty_id:=CoParty,timers:=Timers}=Data) when ?MONITORED=:=true -> 
  ?VSHOW("about to call (~p).",[CoParty],Data),
  %% call monitor synchronously to do this
  %% monitor returns their updated copy of timer map.
  Timers1 = gen_statem:call(CoParty,{set_timer, {Name, Duration}, Timers}),
  ?VSHOW("received response from (~p),\nold timers:\t~p,\nnew timers:\t~p.",[CoParty,Timers,Timers1],Data),
  %% return with updated timers
  maps:put(timers,Timers1,Data);
%%

%% @docs resets timer that already exists (for unmonitored process).
set_timer(Name, Duration, #{timers:=Timers}=Data) when is_map_key(Name,Timers) -> 
  %% cancel timer
  erlang:cancel_timer(maps:get(Name,Timers)),
  %% start new timer
  TID = erlang:start_timer(Duration, self(), Name),
  Data1 = maps:put(timers, maps:put(Name, TID, Timers), Data),
  ?VSHOW("reset timer t (~p),\nold data:\t~p,\nnew data:\t~p.",[TID,Data,Data1],Data1),
  Data;
%%

%% @docs starts new timer that did not exist (for unmonitored process).
set_timer(Name, Duration, #{timers:=Timers}=Data) -> 
  %% start new timer
  TID = erlang:start_timer(Duration, self(), Name),
  Data1 = maps:put(timers, maps:put(Name, TID, Timers), Data),
  ?VSHOW("set new timer t (~p),\nold data:\t~p,\nnew data:\t~p.",[TID,Data,Data1],Data1),
  Data1.
%%

%% @doc retrieves timer pid from data if exists.
%% note: does not require different functionality when monitored since the stub process always has a copy of the timers in their Data.
get_timer(Name, #{timers:=Timers}=_Data) 
when is_atom(Name) and is_map_key(Name, Timers) -> 
  maps:get(Name, Timers);

%% @doc returns `{ko, no_such_timer}` if timer does not exist.
get_timer(_Name, #{timers:=_Timers}=_Data)
when not is_map_key(_Name, _Timers) -> 
  ?VSHOW("timer (~p) not found in timers:\n\t~p.",[_Name,_Timers],_Data),
  {ko, no_such_timer}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% message management
%%%%%%%%%%%%%%%%%%%%%%%

%% @doc saves message to data to front of list under that label
save_msg(Label, Payload, #{msgs:=Msgs}=Data) ->
  %% add to head of corresponding list
  maps:put(msgs, maps:put(Label, [Payload]++maps:get(Label,Msgs,[]), Msgs), Data).
%%

%% @doc retrieves message from front of list
get_msg(Label, #{msgs:=Msgs}=_Data) ->
  case maps:get(Label, Msgs, []) of
    [] -> undefined;
    [H|_T] -> H
  end.
%%

%% @doc retrieves all messages under label
get_msgs(Label, #{msgs:=Msgs}=_Data) ->
  case maps:get(Label, Msgs, []) of
    [] -> undefined;
    [_H|_T]=All -> All
  end.
%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% non-blocking behaviour: payloads
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Calls Fun with Args and forwards the result to PID.
%% If Fun completes within Timeout milliseconds, returns Result.
%% Otherwise signals PID 'ko' since it took too long.
nonblocking_payload(Fun, Args, PID, Timeout, Data) when is_integer(Timeout) ->
  spawn( fun() -> 
    Waiter = self(),
    TimeConsumer = spawn( fun() -> Waiter ! {self(), ok, Fun(Args)} end ),
    receive 
      {TimeConsumer, ok, Result} -> 
        ?VSHOW("payload completed within (~pms),\nResult:\t~p.",[Timeout,Result],Data),
        PID ! {self(), ok, Result}
    after Timeout -> 
      ?VSHOW("payload did not complete within (~pms) sending ko.",[Timeout],Data),
      PID ! {self(), ko}, 
      exit(TimeConsumer, normal) 
    end 
  end );
%%

%% @doc Calls Fun with Args and forwards the result to PID.
%% this version takes a reference to a timer directly
%% @see nonblocking_payload with Timeout
nonblocking_payload(Fun, Args, PID, TimerRef, Data) 
when is_reference(TimerRef) ->
  ?VSHOW("fetching value of timer-ref (~p).",[TimerRef],Data),
  Duration = erlang:read_timer(TimerRef),
  ?VSHOW("remaining duration timer is (~pms).",[Duration],Data),
  nonblocking_payload(Fun, Args, PID, Duration, Data);
%%

%% @doc Calls Fun with Args and forwards the result to PID.
%% @see nonblocking_payload with Timeout
nonblocking_payload(Fun, Args, PID, Timer, Data) 
when is_atom(Timer) ->
  ?VSHOW("fetching ref of timer (~p).",[Timer],Data),
  TimerRef = get_timer(Timer,Data),
  nonblocking_payload(Fun, Args, PID, TimerRef, Data).
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% non-blocking behaviour: selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Calls Fun with Args and forwards the result to nonblocking_payload and then to PID.
%% If Fun completes within Timeout milliseconds, then Result is passed to nonblocking_payload.
%% If nonblocking_payload responds too late, ko is sent back to PID.
nonblocking_selection(Fun, Args, PID, Timeout, Data) when is_integer(Timeout) ->
  spawn( fun() -> 
    %% spawn timer in the case of being passed to nonblocking_payload, need to take into account the duration of Fun
    Timer = erlang:start_timer(Timeout, self(), nonblocking_selection),
    ?VSHOW("started timer (~p)",[Timer],Data),
    Selector = self(),
    %% spawn process in same node to send back results of selection
    SelectingWorker = spawn( fun() -> Selector ! {self(), ok, Fun(Args)} end ),
    ?VSHOW("waiting to get selection back from (~p)",[SelectingWorker],Data),
    receive 
      %% if worker process determines selection, and returns necessary function/args to obtain payload to send
      {SelectingWorker, ok, {PayloadFun, PayloadArgs}=R} -> 
        ?VSHOW("selection made before (~p) completes,\nResult:\t~p.",[Timer,R],Data),
        %% spawn new process using function to obtain payload
        NonBlocking = nonblocking_payload(PayloadFun, PayloadArgs, self(), Timer, Data),
        ?VSHOW("waiting to get payload back from (~p).",[NonBlocking],Data),
        %% forward result of nonblocking_waiter to PID
        receive 
          {NonBlocking, ok, {_Label, _Payload}=Result} -> 
            ?VSHOW("payload received: (~p: ~p).",[_Label, _Payload],Data),
            PID ! {self(), ok, Result};
          {NonBlocking, ko} -> 
            ?VSHOW("payload returned ko.",[],Data),
            PID ! {self(), ko}, 
            exit(NonBlocking, normal) 
          end;
      %% if timer expires first, signal PID that after-branch should be taken and terminate workers
      {timeout, Timer, nonblocking_selection} -> 
        ?VSHOW("selecting worker took longer than (~p) to complete.",[Timer],Data),
        PID ! {self(), ko}, 
        exit(SelectingWorker, normal) 
    end 
  end );
%%

%% @doc re-directs to function that takes integer value, which is here derived from the timer.
nonblocking_selection(Fun, Args, PID, Timer, Data) when is_atom(Timer) ->
  TID = get_timer(Timer,Data),
  ?VSHOW("fetching value of timer (~p, ~p).",[Timer,TID],Data),
  Duration = erlang:read_timer(TID),
  ?VSHOW("remaining duration of (~p) is: ~pms.",[Timer,Duration],Data),
  nonblocking_selection(Fun, Args, PID, Duration, Data).
%%


%%%%%%%%%%%%%%%%%%%%%%
%%% proposed snippets 
%%%%%%%%%%%%%%%%%%%%%%

%% @doc Wrapper function for sending the result of Fun with Label if it finished within Timeout milliseconds
send_before(CoPartyID, {Label, {Fun, Args}}, Timeout, Data) 
when is_pid(CoPartyID) and is_atom(Label) -> 
  NonBlocking = nonblocking_payload(Fun, Args, self(), Timeout, Data),
  ?VSHOW("waiting to receive payload from (~p) to send (~p).",[NonBlocking,Label],Data),
  receive 
    {NonBlocking, ok, Payload} -> 
      ?VSHOW("for (~p), payload: ~p.",[Label,Payload],Data),
      send(CoPartyID, {Label, Payload}, Data);
    {NonBlocking, ko} -> 
      ko
  end.

%% @doc 
% recv_before(MonitorID, Label, Timeout) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label, Timeout});

%% @doc 
recv_before(CoPartyID, Label, Timeout, Data) 
when is_pid(CoPartyID) and is_atom(Label) -> 
  ?VSHOW("waiting to receive (~p) within (~pms),",[Label,Timeout],Data),
  receive 
    {CoPartyID, Label, Payload} -> 
      ?VSHOW("recv'd (~p: ~p).",[Label,Payload],Data),
      {ok, Payload} 
  after Timeout -> 
    ?VSHOW("timeout, did not recv (~p).",[Label],Data),
    {ko, timeout} 
  end.

%% @doc Wrapper function for sending messages asynchronously via synchronous Monitor
% send(MonitorID, {Label, _Payload}=Msg) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {send, Msg});

%% @doc Wrapper function for sending messages asynchronously
send(CoPartyID, {Label, _Payload}=Msg, Data) 
when is_pid(CoPartyID) and is_atom(Label) -> 
  CoPartyID ! {self(), Msg}, 
  ?VSHOW("sent (~p).",[Msg],Data),
  ok.

%% @doc Wrapper function for receiving message asynchronously via synchronous Monitor
% recv(MonitorID, Label) when is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label});

%% @doc Wrapper function for receiving message asynchronously
recv(CoPartyID, Label, Data) 
when is_pid(CoPartyID) and is_atom(Label) -> 
  ?VSHOW("waiting to receive (~p).",[Label],Data),
  receive 
    {CoPartyID, Label, Payload} -> 
      ?VSHOW("recv'd (~p: ~p).",[Label,Payload],Data),
      {ok, Payload} 
  end.



