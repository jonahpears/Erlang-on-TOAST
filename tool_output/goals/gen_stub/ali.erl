-module(ali).

-export([ start_link/0,
          start_link/1,
          init/1 
        ]).

-include_lib("stdlib/include/assert.hrl").


%% @doc determines if start_link/0/1 creates monitor in the same node
-define(MONITORED, true).

%% @doc if monitored, put specification here (i.e.: module:spec_fun() or spec_map=#{...}) OR, pass in with Args list
-define(MONITOR_SPEC, false).

%% @doc if monitored, how long in milliseconds to wait for monitor to confirm setup
-define(MONITOR_SETUP_DURATION, 1000).

start_link() -> start_link([]).

start_link(Args) when ?MONITORED==true ->
  Params = maps:from_list(Args),

  %% if ?MONITORED, then either MONITOR_SPEC is map, or params contains 
  case is_boolean(?MONITOR_SPEC) of 
    true -> ?assert(maps:is_key(monitor_spec,Params)), MonitorSpec = maps:get(monitor_spec, Params);
    _ -> MonitorSpec = ?MONITOR_SPEC
  end,

  {MonitorID, _MonitorRef} = spawn_monitor(gen_monitor, start_link, [MonitorSpec]),

  %% exchange IDs with monitor
  PID = self(),
  % MonitorID ! {PID, monitor_me, ?MODULE},
  MonitorID = gen_statem:call(MonitorID, {init, exchange_ids, PID}),
  InitID = erlang:spawn_link(?MODULE, init, [Args]),
  receive {MonitorID, monitoring, PID} -> 
    %% send monitor initID
    MonitorID ! {PID, confirmed, InitID},
    %% return monitor to supervisor
    {ok, MonitorID}
  after ?MONITOR_SETUP_DURATION -> 
    %% terminate monitor if it took too long
    exit(MonitorID, setup_took_too_long), 
    %% continue by returning initID to supervisor
    {ok, InitID} 
  end;

%% @doc normal unmonitored start_link
start_link(Args) ->
  InitID = erlang:spawn_link(?MODULE, init, [Args]),
  {ok, InitID}.

%% @doc called after start_link returns
init(Args) when ?MONITORED==true ->
  Params = maps:from_list(Args),

  Role = maps:get(role,Params,role_unspecified),

  PID = self(),
  receive {MonitorID, init, PID} -> 


