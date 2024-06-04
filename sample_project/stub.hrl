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
  printout("~p (set to be monitored).",[?FUNCTION_NAME]),
  Params = maps:from_list(Args),

  %% if ?MONITORED, then either MONITOR_SPEC is map, or params contains 
  case is_boolean(?MONITOR_SPEC) of 
    true -> %% if monitor spec not in this file, check in param map
      ?assert(maps:is_key(monitor_spec,Params)), 
      MonitorSpec = maps:get(monitor_spec, Params);
    _ -> MonitorSpec = ?MONITOR_SPEC
  end,

  %% spawn monitor within same node 
  {MonitorID, _MonitorRef} = spawn_monitor(node(), gen_monitor, start_link, [MonitorSpec]),

  %% exchange IDs with monitor
  PID = self(),
  InitID = erlang:spawn_link(?MODULE, init, [Args, {start_id, PID}, {monitor_id, MonitorID}]),
  case gen_statem:call(MonitorID, {init, exchange_ids, PID, InitID}) of 
    ok -> {ok, MonitorID};
    Err -> {error, Err}
  end;

%% @doc normal unmonitored start_link
stub_start_link(Args) -> 
  printout("~p (unmonitored), args:\n\t~p.",[?FUNCTION_NAME,Args]),
  InitID = erlang:spawn_link(?MODULE, init, [Args]),
  printout("~p (unmonitored), ID: ~p.",[?FUNCTION_NAME,InitID]),
  % printout("~p (unmonitored): ~p.",[?FUNCTION_NAME,self()]),
  % {ok, stub_init(Args)}.
  {ok, InitID}.

%% @doc called after start_link returns
stub_init(Args) when ?MONITORED=:=true ->
  printout("~p (monitored), args:\n\t~p.",[?FUNCTION_NAME,Args]),
  Params = maps:from_list(Args),
  _PID = self(),

  %% unpack from param map
  _Role = maps:get(role,Params,role_unspecified),

  _StartID = maps:get(role,Params,start_id),
  MonitorID = maps:get(role,Params,monitor_id),

  %% enter setup phase with monitor
  case gen_statem:call(MonitorID, {setup}) of 
    ok -> %% configure monitor settings
      ok = gen_statem:call(MonitorID, {options, printout, #{ enabled => true, verbose => true }}),
      ok = gen_statem:call(MonitorID, {options, queue, #{enabled=>true,flush_after_recv=>#{enabled => false}}}),
      ok = gen_statem:call(MonitorID, {options, support_auto_label, #{enabled=>true}}),
      %% wait for monitor to receive coparty id, and return here
      {ok, CoPartyID} = gen_statem:call(MonitorID, {get_coparty_id}),
      %% enter run phase of program with monitor
      MainID = erlang:spawn_link(?MODULE, run, [MonitorID]), %% as coparty ID
      gen_statem:call(MonitorID, {leave_setup, #{party_id => MainID}}),
      % {ok, MonitorID};
      MainID;
    Err -> {error, Err}
  end;

%% @doc 
stub_init(Args) ->
  printout("~p (unmonitored), args:\n\t~p.",[?FUNCTION_NAME,Args]),
  Params = maps:from_list(Args),
  PID = self(),

  %% unpack from param map
  Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  printout("~p (unmonitored), session_id: ~p.",[?FUNCTION_NAME,SessionID]),
  ?assert(is_pid(SessionID)),

  SessionID ! {self(),role,Role},

  %% wait to receive coparty id from session
  receive {SessionID, coparty_id, CoPartyID} ->
    %% exchange init message
    CoPartyID ! {PID, init},
    receive {CoPartyID, init} -> ok end,
    SessionID ! {self(), ready},
    %% wait for signal from session
    receive {SessionID, start} -> run(CoPartyID, default_map()) end 
  end.

%% @docs default map for stubs
default_map() -> #{timers=>maps:new(),msgs=>maps:new()}.

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
recv_before(MonitorID, Label, Timeout) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label, Timeout});

%% @doc 
recv_before(CoPartyID, Label, Timeout) when is_pid(CoPartyID) and is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, Payload} after Timeout -> {ko, timeout} end.

%% @doc Wrapper function for sending messages asynchronously via synchronous Monitor
send(MonitorID, {Label, _Payload}=Msg) when ?MONITORED and is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {send, Msg});

%% @doc Wrapper function for sending messages asynchronously
send(CoPartyID, {Label, _Payload}=Msg) when is_pid(CoPartyID) and is_atom(Label) -> CoPartyID ! {self(), Msg}, ok.

%% @doc Wrapper function for receiving message asynchronously via synchronous Monitor
recv(MonitorID, Label) when is_pid(MonitorID) and is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label});

%% @doc Wrapper function for receiving message asynchronously
recv(CoPartyID, Label) when is_pid(CoPartyID) and is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, Payload} end.
