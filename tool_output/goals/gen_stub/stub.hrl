-export([ start_link/0,
          start_link/1,
          init/1,
          send/2,
          recv/2,
          send_before/3, 
          recv_before/3
        ]).

-include_lib("stdlib/include/assert.hrl").


%% @doc determines if start_link/0/1 creates monitor in the same node
-ifdef(MONITORED).
-else.
-define(MONITORED, true).
-endif.

%% @doc if monitored, put specification here (i.e.: module:spec_fun() or spec_map=#{...}) OR, pass in with Args list
-ifdef(MONITOR_SPEC).
-else.
-define(MONITOR_SPEC, false).
-endif.

%% @doc 
start_link() -> start_link([]).

%% @doc 
start_link(Args) when ?MONITORED==true ->
  Params = maps:from_list(Args),

  %% if ?MONITORED, then either MONITOR_SPEC is map, or params contains 
  case is_boolean(?MONITOR_SPEC) of 
    true -> %% if monitor spec not in this file, check in param map
      ?assert(maps:is_key(monitor_spec,Params)), 
      MonitorSpec = maps:get(monitor_spec, Params);
    _ -> MonitorSpec = ?MONITOR_SPEC
  end,

  {MonitorID, _MonitorRef} = spawn_monitor(gen_monitor, start_link, [MonitorSpec]),

  %% exchange IDs with monitor
  PID = self(),
  InitID = erlang:spawn_link(?MODULE, init, [Args, {start_id, PID}, {monitor_id, MonitorID}]),
  case gen_statem:call(MonitorID, {init, exchange_ids, PID, InitID}) of 
    ok -> {ok, MonitorID};
    Err -> {error, Err}
  end;

%% @doc normal unmonitored start_link
start_link(Args) -> {ok, _InitID} = erlang:spawn_link(?MODULE, init, [Args]).

%% @doc called after start_link returns
init(Args) when ?MONITORED==true ->
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
      %% wait for monitor to receive coparty id 
      {ok, CoPartyID} = gen_statem:call(MonitorID, {get_coparty_id}),
      %% enter main phase of program with monitor
      MainID = erlang:spawn_link(?MODULE, main, [MonitorID]),
      {ok, CoPartyID} = gen_statem:call(MonitorID, {leave_setup, #{party_id => MainID}});
    Err -> {error, Err}
  end;

%% @doc 
init(Args) ->
  Params = maps:from_list(Args),
  PID = self(),

  %% unpack from param map
  _Role = maps:get(role,Params,role_unspecified),

  SessionID = maps:get(session_id,Params,no_session_id_found),
  ?assert(is_pid(SessionID)),

  %% wait to receive coparty id from session
  receive {SessionID, coparty_id, CoPartyID} ->
    %% exchange init message
    CoPartyID ! {PID, init},
    receive {CoPartyID, init} -> ok end,
    %% wait for signal from session
    receive {SessionID, start} -> main(CoPartyID) end 
  end.

%% @doc Calls Fun with Args and forwards the result, if Fun completes within Timeout milliseconds, otherwise signals PID 
send_after(Func, Args, PID, Timeout) ->
  spawn( fun() -> 
    Waiter = self(),
    TimeConsumer = spawn( fun() -> Waiter ! {self(), ok, Func(Args)} end ),
    receive {TimeConsumer, ok, Result} -> PID ! {self(), ok, Result}
    after Timeout -> PID ! {self(), ko} end 
  end ).

%% @doc Wrapper function for sending the result of Fun with Label if it finished within Timeout milliseconds
send_before(CoPartyID, {Label, {Fun, Args}}, Timeout) -> 
  NonBlocking = send_after(Fun, Args, self(), Timeout),
  receive {NonBlocking, ok, Result} -> send(CoPartyID, {Label, Result});
    {NonBlocking, ko} -> ko
  end.

%% @doc 
recv_before(MonitorID, Label, Timeout) when ?MONITORED and  is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label, Timeout});

%% @doc 
recv_before(CoPartyID, Label, Timeout) when is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, PayLoad} after Timeout -> {ko, timeout} end.

%% @doc Wrapper function for sending messages asynchronously via synchronous Monitor
send(MonitorID, {Label, _Payload}=Msg) when ?MONITORED and is_atom(Label) -> gen_statem:call(MontiorID, {send, Msg});

%% @doc Wrapper function for sending messages asynchronously
send(CoPartyID, {Label, _Payload}=Msg) when is_atom(Label) -> CoPartyID ! {self(), Msg}, ok.

%% @doc Wrapper function for receiving message asynchronously via synchronous Monitor
recv(MonitorID, Label) when is_atom(Label) -> gen_statem:call(MonitorID, {recv, Label});

%% @doc Wrapper function for receiving message asynchronously
recv(CoPartyID, Label) when is_atom(Label) -> receive {CoPartyID, Label, Payload} -> {ok, Payload} end.
