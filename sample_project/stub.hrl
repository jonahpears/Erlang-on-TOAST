-export([ stub_start_link/0,
          stub_start_link/1,
          stub_init/1,
          send/2,
          recv/2,
          send_before/3, 
          recv_before/3,
          set_timer/3,
          get_timer/2,
          default_map/0,
          save_msg/3,
          get_msg/2,
          get_msgs/2,
          nonblocking_selection/4,
          nonblocking_payload/4
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
  % RegID = maps:get(reg_id, _Params, ?MODULE),
  % case RegID of
  %   ?MODULE -> Params = _Params;
  %   % _ -> Params = maps:remove(reg_id, _Params)
  % end,

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

  MonitorArgs = maps:to_list(Params),

  %% spawn monitor within same node 
  % {MonitorID, _MonitorRef} = erlang:spawn_monitor(node(), gen_monitor, start_link, [Args++[{fsm,MonitorSpec}]]),
  MonitorID = erlang:spawn_link(node(), gen_monitor, start_link, [MonitorArgs++[{fsm,MonitorSpec},{name,MonitorName},{sus_init_id,self()},{sus_id,erlang:spawn_link(?MODULE, init, [Args++[{sus_init_id, self()}, {name,Name}]])}]]),

  ?SHOW("MonitorID: ~p.",[MonitorID],Params),

  %% wait for message from monitor with actual ID
  % receive {InitMonitorID, starting_as, MonitorID} -> printout(RegID, "~p, InitMonitorID: ~p,\n\tMonitorID: ~p.",[?FUNCTION_NAME,InitMonitorID,MonitorID]) end,
  

  % %% exchange IDs with monitor
  % PID = self(),

  % timer:sleep(500),

  % receive {InitMonitorID, mon_starting_as, MonitorID} ->
  
  % SusID = erlang:spawn_link(?MODULE, init, [Args++[{sus_init_id, self()}, {monitor_id, MonitorID}, {name,Name}]]),

  % MonitorID ! (self(), sus_starting_as, SusID),

  {ok, MonitorID};
  % end;

  % case gen_statem:call(MonitorID, {init, {exchange_ids, self(), InitID}}) of 
  %   ok -> 
  %     % printout(RegID,"~p, call(~p, {init, {exchange_ids, ~p, ~p}}) -> ok.",[?FUNCITON_NAME,MonitorID,self(),InitID]),
  %     {ok, InitMonitorID};
  %   Err -> 
  %     % printout(RegID,"~p, call(~p, {init, {exchange_ids, ~p, ~p}}) -> error:\n\t~p.",[?FUNCITON_NAME,MonitorID,self(),InitID,Err]),
  %     {error, Err}
  % end;
%%

%% @doc normal unmonitored start_link
stub_start_link(Args) -> 
  Params = maps:from_list(Args),
  ?SHOW("args:\n\t~p.",[Args],Params),
  InitID = erlang:spawn_link(?MODULE, init, [Args]),
  
  ?SHOW("ID: ~p.",[InitID],Params),
  % printout("~p (unmonitored): ~p.",[?FUNCTION_NAME,self()]),
  % {ok, stub_init(Args)}.
  {ok, InitID}.
%%

%% @doc called after start_link returns
stub_init(Args) when ?MONITORED=:=true ->
  Params = maps:from_list(Args),
  ?SHOW("args:\n\t~p.",[Args],Params),

  % PID = self(),

  %% unpack from param map
  Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  ?assert(is_pid(SessionID)),

  ?SHOW("session_id: ~p.",[SessionID],Params),

  Data = maps:put(session_id,SessionID,default_map()),
  Data1 = maps:put(role,Role,Data),

  %% receive message from monitor
  receive 
    {{MonitorID, is_monitor},{SusInitID, proof_init_id},{in_session,SessionID}} ->
      %% 
      Data2 = maps:put(coparty_id,MonitorID,Data1),
      {ok, Data2}
  end;

  % % SessionID ! {self(),role,Role},

  % %% wait to receive coparty id from session
  % receive {SessionID, coparty_id, CoPartyID} ->
  %   %% exchange init message
  %   CoPartyID ! {PID, init},
  %   receive {CoPartyID, init} -> ok end,
  %   SessionID ! {self(), ready},
  %   %% wait for signal from session
  %   receive {SessionID, start} -> run(CoPartyID, default_map()) end 
  % end;



  % Params = maps:from_list(Args),
  % printout(Params,"~p (monitored), args:\n\t~p.",[?FUNCTION_NAME,Args]),
  % _PID = self(),

  % %% unpack from param map
  % _Role = maps:get(role,Params,role_unspecified),

  % StartID = maps:get(start_id,Params),
  % MonitorID = maps:get(monitor_id,Params),
  % printout(Params,"~p, StartID: ~p.",[?FUNCTION_NAME,StartID]),
  % printout(Params,"~p, MonitorID: ~p.",[?FUNCTION_NAME,MonitorID]),

  % %% enter setup phase with monitor
  % case gen_statem:call(MonitorID, {StartID,setup,self()}) of 
  %   ok -> %% configure monitor settings
  %     ok = gen_statem:call(MonitorID, {options, printout, #{ enabled => true, verbose => true }}),
  %     ok = gen_statem:call(MonitorID, {options, queue, #{enabled=>true,flush_after_recv=>#{enabled => false}}}),
  %     ok = gen_statem:call(MonitorID, {options, support_auto_label, #{enabled=>true}}),
  %     %% wait for monitor to receive coparty id, and return here
  %     {ok, _CoPartyID} = gen_statem:call(MonitorID, {get_coparty_id}),
  %     %% enter run phase of program with monitor
  %     MainID = erlang:spawn_link(?MODULE, run, [MonitorID]), %% as coparty ID
  %     gen_statem:call(MonitorID, {leave_setup, #{party_id => MainID}}),
  %     % {ok, MonitorID};
  %     MainID;
  %   Err -> {error, Err}
  % end;
%%

%% @doc 
stub_init(Args) ->
  Params = maps:from_list(Args),
  ?SHOW("Params:\n\t~p.",[Params],Params),
  PID = self(),

  %% unpack from param map
  Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  ?SHOW("session_id: ~p.",[SessionID],Params),
  ?assert(is_pid(SessionID)),

  SessionID ! {self(),role,Role},

  Data = maps:put(session_id,SessionID,default_map()),
  Data1 = maps:put(role,maps:get(role,Params),Data),

  % %% wait to receive coparty id from session
  ?SHOW("waiting to receive CoPartyID from session.",[],Data1),
  receive {SessionID, coparty_id, CoPartyID} ->
    ?SHOW("received CoPartyID (~p), now waiting for init message from CoParty.",[CoPartyID],Data1),
    Data2 = maps:put(coparty_id,CoPartyID,Data1),
    %% exchange init message
    CoPartyID ! {PID, init},
    receive {CoPartyID, init} ->
      ?SHOW("now telling session ready, and waiting for signal to start.",[],Data1),
      %% tell session ready to proceed
      SessionID ! {self(), ready},
      % SessionID ! {self(), ready},
      % %% wait for signal from session
      % receive {SessionID, start} -> 
      %   ?SHOW("received start signal from session.",[],Data1),
      %   % run(CoPartyID, default_map()) 
        {ok, Data2}
      % end 
    end
  end.
%%

%% @docs default map for stubs
default_map() -> #{timers=>maps:new(),msgs=>maps:new(),coparty_id=>undefined}.

%% @docs resets timer that already exists
set_timer(Name, Duration, #{timers:=Timers}=Data) when is_map_key(Name,Timers) -> 
  %% cancel timer
  erlang:cancel_timer(maps:get(Name,Timers)),
  %% start new timer
  TID = erlang:start_timer(Duration, self(), {timer, Name}),
  {maps:put(timers, maps:put(Name, TID, Timers), Data), TID};
%% @docs starts new timer that did not exist
set_timer(Name, Duration, #{timers:=Timers}=Data) -> 
  %% start new timer
  TID = erlang:start_timer(Duration, self(), {timer, Name}),
  {maps:put(timers, maps:put(Name, TID, Timers), Data), TID}.

%% @doc retrieves timer pid from data if exists
get_timer(Name, #{timers:=Timers}=_Data) when is_map_key(Name, Timers) -> {ok, maps:get(Name, Timers)};
get_timer(_Name, #{timers:=_Timers}=_Data) -> {ko, no_such_timer}.

%% @doc saves message to data to front of list under that label
save_msg(Label, Payload, #{msgs:=Msgs}=Data) ->
  %% add to head of corresponding list
  maps:put(msgs, maps:put(Label, [Payload]++maps:get(Label,Msgs,[]), Msgs), Data).

%% @doc retrieves message from front of list
get_msg(Label, #{msgs:=Msgs}=_Data) ->
  case maps:get(Label, Msgs, []) of
    [] -> undefined;
    [H|_T] -> H
  end.
  
%% @doc retrieves all messages under label
get_msgs(Label, #{msgs:=Msgs}=_Data) ->
  case maps:get(Label, Msgs, []) of
    [] -> undefined;
    [_H|_T]=All -> All
  end.

%% @doc Calls Fun with Args and forwards the result to nonblocking_payload and then to PID.
%% If Fun completes within Timeout milliseconds, then Result is passed to nonblocking_payload.
%% If nonblocking_water 
nonblocking_selection(Fun, Args, PID, Timeout) when is_integer(Timeout) ->
  spawn( fun() -> 
    %% spawn timer in the case of being passed to nonblocking_payload, need to take into account the duration of Fun
    Timer = erlang:start_timer(Timeout, self(), nonblocking_selection),
    Selector = self(),
    %% spawn process in same node to send back results of selection
    SelectingWorker = spawn( fun() -> Selector ! {self(), ok, Fun(Args)} end ),
    receive 
      %% if worker process determines selection, and returns necessary function/args to obtain payload to send
      {SelectingWorker, ok, {PayloadFun, PayloadArgs}} -> 
        %% spawn new process using function to obtain payload
        NonBlocking = nonblocking_payload(PayloadFun, PayloadArgs, self(), Timer),
        %% forward result of nonblocking_waiter to PID
        receive {NonBlocking, ok, {_Label, _Payload}=Result} -> PID ! {self(), ok, Result};
        {NonBlocking, ko} -> PID ! {self(), ko}, exit(NonBlocking, normal) end;
      %% if timer expires first, signal PID that after-branch should be taken and terminate workers
      {timeout, Timer, nonblocking_selection} -> PID ! {self(), ko}, exit(SelectingWorker, normal) 
    end 
  end );

%% @doc 
nonblocking_selection(Fun, Args, PID, Timer) when is_pid(Timer) ->
  nonblocking_selection(Fun, Args, PID, erlang:read_timer(Timer)).

%% @doc Calls Fun with Args and forwards the result to PID.
%% If Fun completes within Timeout milliseconds, returns Result.
%% Otherwise signals PID 'ko' since it took too long.
nonblocking_payload(Fun, Args, PID, Timeout) when is_integer(Timeout) ->
  spawn( fun() -> 
    Waiter = self(),
    TimeConsumer = spawn( fun() -> Waiter ! {self(), ok, Fun(Args)} end ),
    receive {TimeConsumer, ok, Result} -> PID ! {self(), ok, Result}
    after Timeout -> PID ! {self(), ko}, exit(TimeConsumer, normal) end 
  end );

%% @doc Calls Fun with Args and forwards the result to PID.
%% @see nonblocking_payload with Timeout
nonblocking_payload(Fun, Args, PID, Timer) when is_pid(Timer) ->
  nonblocking_payload(Fun, Args, PID, erlang:read_timer(Timer)).

%% @doc Wrapper function for sending the result of Fun with Label if it finished within Timeout milliseconds
send_before(CoPartyID, {Label, {Fun, Args}}, Timeout) when is_pid(CoPartyID) and is_atom(Label) -> 
  NonBlocking = nonblocking_payload(Fun, Args, self(), Timeout),
  receive {NonBlocking, ok, Result} -> send(CoPartyID, {Label, Result});
    {NonBlocking, ko} -> ko
  end.

%% @doc 
% recv_before(MonitorID, Label, Timeout) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label, Timeout});

%% @doc 
recv_before(CoPartyID, Label, Timeout) when is_pid(CoPartyID) and is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, Payload} after Timeout -> {ko, timeout} end.

%% @doc Wrapper function for sending messages asynchronously via synchronous Monitor
% send(MonitorID, {Label, _Payload}=Msg) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {send, Msg});

%% @doc Wrapper function for sending messages asynchronously
send(CoPartyID, {Label, _Payload}=Msg) when is_pid(CoPartyID) and is_atom(Label) -> CoPartyID ! {self(), Msg}, ok.

%% @doc Wrapper function for receiving message asynchronously via synchronous Monitor
% recv(MonitorID, Label) when is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label});

%% @doc Wrapper function for receiving message asynchronously
recv(CoPartyID, Label) when is_pid(CoPartyID) and is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, Payload} end.
