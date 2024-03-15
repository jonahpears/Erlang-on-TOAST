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

-record(state, {}).

-define(SERVER,?MODULE).
-define(NAME,?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).


start_link() -> start_link([]).
start_link(Params) -> 
    io:format("\n\n\n\n\n--------------\n\n"),
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
    Params1 = [{ child_spec, #{ msger => Msger,
                                acker => Acker } }] ++ Params,
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
    msg_supervisor:start_link(maps:to_list(#{ strategy => Strategy,
                                              intensity => Intensity,
                                              period => Period,
                                              child_options => ChildOptions,
                                              statem_options => StatemOptions,
                                              child_spec => ChildSpec })),
    msg_supervisor:run_setup(),
    % msg_supervisor:run_setup(ChildOptions, StatemOptions, ChildSpec),
    printout("~p, finished setup.\n", [?FUNCTION_NAME]),
    {ok, #state{}}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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



