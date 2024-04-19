-module(msg_server).
-behaviour(gen_server).

-include("data_records.hrl").

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
          init/2 ]).

-export([ act/2 ]).

-record(state, {}).

-define(SERVER,?MODULE).
-define(NAME,?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).


start_link() -> 
    Msger = [{ init_state, state1_send_msg1},
             { timeouts, #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
                            state3a_recv_ack2 => {5000, issue_timeout} }},
             { state_map, #{ state1_send_msg1  => #{send => #{msg1 => state2a_recv_ack1} },
                             state2a_recv_ack1 => #{recv => #{ack1 => state1_send_msg1}  },
                             state2b_send_msg2 => #{send => #{msg2 => state3a_recv_ack2} },
                             state3a_recv_ack2 => #{recv => #{ack2 => state2a_recv_ack1} }} }],
    Acker = [{ init_state, state1_recv_msg1},
             { timeouts, #{ state2a_send_ack1 => {5000, state2b_recv_msg2} }},
             { state_map, #{ state1_recv_msg1  => #{recv => #{msg1 => state2a_send_ack1} },
                             state2a_send_ack1 => #{send => #{ack1 => state1_recv_msg1}  },
                             state2b_recv_msg2 => #{recv => #{msg2 => state3a_send_ack2} },
                             state3a_send_ack2 => #{send => #{ack2 => state2a_send_ack1} }} }],
    % add default parameters
    Params = [{ default_child_spec, #{ msger => Msger,
                                       acker => Acker,
                                       names => [msger, acker] } }],
  start_link(Params).
start_link([H|_T]) -> 
    io:format("\n\n\n\n\n--------------\n\n"),
    case H of
      {default_child_spec, DefaultChildSpec} ->
          printout("~p, no parameters given. Using default (Msger & Acker).", [?FUNCTION_NAME]),
          Params1 = [{child_spec, DefaultChildSpec}];
      {child_spec, ChildSpec} ->
          Names = maps:get(names, ChildSpec),
          printout("~p, child spec found, using names: ~p.", [?FUNCTION_NAME, Names]),
          Parties = #{ names => Names },
          lists:foreach(fun(Name) -> 
                Schema = maps:get(Name, ChildSpec),
                InitState = maps:get(init_state, Schema),
                Timeouts = maps:get(timeouts, Schema),
                StateMap = maps:get(state_map, Schema),
                Party = [InitState, Timeouts, StateMap],
                %% add to Params
                maps:put(Name, Party, Parties)
            end, Names),
          Params1 = [{child_spec, Parties}];
          % Params1 = H;
      _ -> 
          printout("~p, error: no child spec provided and no default found.", [?FUNCTION_NAME]),
          Params1 = [],
          exit(error_no_child_spec_found)
    end,
      
    % Msger = [{ init_state, state1_send_msg1},
    %          { timeouts, #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
    %                         state3a_recv_ack2 => {5000, issue_timeout} }},
    %          { state_map, #{ state1_send_msg1  => #{send => #{msg1 => state2a_recv_ack1} },
    %                          state2a_recv_ack1 => #{recv => #{ack1 => state1_send_msg1}  },
    %                          state2b_send_msg2 => #{send => #{msg2 => state3a_recv_ack2} },
    %                          state3a_recv_ack2 => #{recv => #{ack2 => state2a_recv_ack1} }} }],
    % Acker = [{ init_state, state1_recv_msg1},
    %          { timeouts, #{ state2a_send_ack1 => {5000, state2b_recv_msg2} }},
    %          { state_map, #{ state1_recv_msg1  => #{recv => #{msg1 => state2a_send_ack1} },
    %                          state2a_send_ack1 => #{send => #{ack1 => state1_recv_msg1}  },
    %                          state2b_recv_msg2 => #{recv => #{msg2 => state3a_send_ack2} },
    %                          state3a_send_ack2 => #{send => #{ack2 => state2a_send_ack1} }} }],
    % % add default parameters
    % Params1 = [{ child_spec, #{ msger => Msger,
    %                             acker => Acker } }] ++ Params,
    % Params1 = [{ child_spec, #{ msger => Msger } }] ++ Params,
    printout("~p, ~p", [?FUNCTION_NAME, erlang:timestamp()]),
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params1]),
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Params1, []),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.

stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init(Params) -> init(Params, #server_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.

init([{supervisor_options,#supervisor_options{ strategy = _Strategy1, intensity = _Intensity1, period = _Period1, child_options = _ChildOptions1, statem_options = _StatemOptions1, child_spec = _ChildSpec1 } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions, child_spec = _ChildSpec, child_names = _ChildNames } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = HVal },
  init(T, Params1);

init([{child_options,#child_options{ restart = _Restart, shutdown = _Shutdown, type = _Type } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = _ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec, child_names = ChildNames } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = HVal,
                                                                       statem_options = StatemOptions,
                                                                       child_spec =  ChildSpec,
                                                                       child_names = ChildNames } },
  init(T, Params1);
    
init([{statem_options,#statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = _StatemOptions, child_spec = ChildSpec, child_names = ChildNames } = _SupervisorOptions } = _Params) ->
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = ChildOptions,
                                                                       statem_options = HVal,
                                                                       child_spec =  ChildSpec,
                                                                       child_names = ChildNames } },
  init(T, Params1);
    
init([{child_spec,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec, child_names = ChildNames } = _SupervisorOptions } = _Params) ->
  %% remove list of names from ChildSpec
  Names = maps:get(names, HVal),
  HVal1 = maps:remove(names, ChildSpec),
  Params1 = #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy,
                                                                       intensity = Intensity,
                                                                       period = Period,
                                                                       child_options = ChildOptions,
                                                                       statem_options = StatemOptions,
                                                                       child_spec = maps:merge(HVal1, ChildSpec), 
                                                                       child_names = lists:uniq(ChildNames ++ Names) } },
  init(T, Params1);


init([HKey|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions, child_spec = _ChildSpec, child_names = _ChildNames } = _SupervisorOptions } = Params) -> 
  printout("~p, unexpected HKey: ~p,\nParams: ~p.", [?FUNCTION_NAME, HKey, Params]),
  init(T, Params);
init([], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec, child_names = ChildNames } = _SupervisorOptions } = _Params) -> 
    % printout("~p, setup, SupervisorOptions: ~p.\n", [?FUNCTION_NAME, SupervisorOptions]),
    msg_supervisor:start_link(maps:to_list(#{ strategy => Strategy,
                                              intensity => Intensity,
                                              period => Period,
                                              child_options => ChildOptions,
                                              statem_options => StatemOptions,
                                              child_spec => ChildSpec,
                                              child_names => ChildNames })),
    msg_supervisor:run_setup(ChildNames),
    % msg_supervisor:run_setup(ChildOptions, StatemOptions, ChildSpec),
    % register(mserver, self()),
    printout("~p, finished setup.\n", [?FUNCTION_NAME]),
    % printout("~p, registered processes: ~p.\n", [?FUNCTION_NAME, registered()]),
    {ok, #state{}}.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call({WorkerID, send, Label, Payload}, _From, State) ->
  % {_, WorkerID} = msg_supervisor:get_child(WorkerName, msg_worker),
  
  Reply = gen_server:call(WorkerID, {send, Label, Payload}),

  printout("~p, {~p, send, ~p, ~p} => ~p.", [?FUNCTION_NAME, WorkerID, Label, Payload, Reply]),

  {reply, Reply, State};

handle_call({WorkerID, recv, Label}, _From, State) ->
  % {_, WorkerID} = msg_supervisor:get_child(WorkerName, msg_worker),
  
  Reply = gen_server:call(WorkerID, {recv, Label}),

  printout("~p, {~p, recv, ~p} => ~p.", [?FUNCTION_NAME, WorkerID, Label, Reply]),

  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

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

