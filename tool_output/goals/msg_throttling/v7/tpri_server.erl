-module(tpri_server).
-file("tpri_server.erl", 1).
-behaviour(gen_server).

-include("tpri_data_records.hrl").

%% gen_server API
-export([ start_link/0,
          init/1,
          stop/0,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3 ]).

%% custom wrappers for params
-export([ start_link/1,
          start_link/2,
          init/2 ]).

-export([ act/2 ]).

-record(state, {}).

-define(SERVER,?MODULE).
-define(NAME,?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

%% no params given, skip to final case
start_link() -> start_link([], []).
%% only params given, begin parsing them
start_link(Params) -> start_link(Params, []).
%% only one more argument to parse
start_link([H], Params) -> 
  Params1 = Params ++ [H],
  start_link([], Params1);
%% more params to parse
start_link([H|T], Params) -> 
  Params1 = Params ++ [H],
  start_link(T, Params1);
%% no more arguments to parse
start_link([], Params) -> 
    printout("~p, ~p", [?FUNCTION_NAME, erlang:timestamp()]),
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []),
    {ok, Pid}.

stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init(Params) -> init(Params, #server_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.

init([{supervisor_options,#supervisor_options{ strategy = _Strategy1, intensity = _Intensity1, period = _Period1, child_options = _ChildOptions1, statem_options = _StatemOptions1, child_spec = _ChildSpec1 } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions, child_spec = _ChildSpec } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = HVal },
  init(T, Params1);

init([{child_options,#child_options{ restart = _Restart, shutdown = _Shutdown, type = _Type } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = _ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = HVal,
                                                                       statem_options = StatemOptions,
                                                                       child_spec =  ChildSpec } },
  init(T, Params1);
    
init([{statem_options,#statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = _StatemOptions, child_spec = ChildSpec } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = ChildOptions,
                                                                       statem_options = HVal,
                                                                       child_spec =  ChildSpec } },
  init(T, Params1);
    
init([{child_spec,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = ChildOptions,
                                                                       statem_options = StatemOptions,
                                                                       child_spec = maps:merge(HVal, ChildSpec) } },
  init(T, Params1);


init([HKey|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions, child_spec = _ChildSpec } = _SupervisorOptions } = Params) -> 
  printout("~p, unexpected HKey: ~p,\nParams: ~p.", [?FUNCTION_NAME, HKey, Params]),
  init(T, Params);
init([], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec } = _SupervisorOptions } = _Params) -> 
    % printout("~p, setup, SupervisorOptions: ~p.\n", [?FUNCTION_NAME, SupervisorOptions]),
    tpri_supervisor:start_link(maps:to_list(#{ strategy => Strategy,
                                               intensity => Intensity,
                                               period => Period,
                                               child_options => ChildOptions,
                                               statem_options => StatemOptions,
                                               child_spec => ChildSpec })),
    tpri_supervisor:run_setup(),
    % msg_supervisor:run_setup(ChildOptions, StatemOptions, ChildSpec),
    % register(mserver, self()),
    printout("~p, finished setup.\n", [?FUNCTION_NAME]),
    % printout("~p, registered processes: ~p.\n", [?FUNCTION_NAME, registered()]),
    {ok, #state{}}.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


% handle_call({_AppID, tpri_app, get_worker_id, WorkerName}, _From, State) ->
%   Reply = tpri_supervisor:get_child(WorkerName, tpri_worker),
%   {reply, Reply, State};




handle_call({WorkerID, send, Label, Payload}, _From, State) ->
  Reply = gen_server:call(WorkerID, {send, Label, Payload}),
  printout("~p, {~p, send, ~p, ~p} => ~p.", [?FUNCTION_NAME, WorkerID, Label, Payload, Reply]),
  {reply, Reply, State};

handle_call({WorkerID, recv, Label}, _From, State) ->
  Reply = gen_server:call(WorkerID, {recv, Label}),
  printout("~p, {~p, recv, ~p} => ~p.", [?FUNCTION_NAME, WorkerID, Label, Reply]),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.




handle_cast(_Request, State) ->
  {noreply, State}.



handle_info({AppID, tpri_app, get_worker_id, WorkerName}, State) ->
  Reply = tpri_supervisor:get_child(WorkerName, tpri_worker),
  AppID ! {self(), worker_id, WorkerName, Reply},
  {noreply, State};


handle_info(_Info, State) ->
  {noreply, State}.




terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% % % % % % %
%% handle external action calls
%% % % % % % %
act(WorkerName, {send, Label, Payload}) ->
  {_, WorkerID} = msg_supervisor:get_child(WorkerName, msg_worker),

  % handle_call(WorkerID, Action).
  gen_server:call(WorkerID, {send, Label, Payload});
act(WorkerName, {recv, Label}) ->
  {_, WorkerID} = msg_supervisor:get_child(WorkerName, msg_worker),

  % handle_call(WorkerID, Action).
  gen_server:call(WorkerID, {recv, Label}).

