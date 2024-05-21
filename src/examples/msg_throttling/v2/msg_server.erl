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
    printout("~p, ~p", [?FUNCTION_NAME, erlang:timestamp()]),
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.

stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init(Params) -> init(Params, #server_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions } = _SupervisorOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, Params) -> init(T, maps:put(HKey, HVal, Params));
init([{HKey,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions } = SupervisorOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, SupervisorOptions) -> init(T, maps:put(supervisor_options, maps:put(HKey, HVal, SupervisorOptions), Params));
init([{HKey,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = ChildOptions, statem_options = _StatemOptions } = SupervisorOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, ChildOptions) -> init(T, maps:put(supervisor_options, maps:put(child_options, maps:put(HKey, HVal, ChildOptions), SupervisorOptions), Params));
init([{HKey,HVal}|T], #server_options{ supervisor_options = #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = StatemOptions } = SupervisorOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, StatemOptions) -> init(T, maps:put(supervisor_options, maps:put(statem_options, maps:put(HKey, HVal, StatemOptions), SupervisorOptions), Params));
init([_H|T], Params) -> init(T, Params);
init([], #server_options{ supervisor_options = #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions } = SupervisorOptions } = _Params) -> 
    printout("~p, setup, SupervisorOptions: ~p.\n", [?FUNCTION_NAME, SupervisorOptions]),
    msg_supervisor:start_link(maps:to_list(#{ strategy => Strategy,
                                              intensity => Intensity,
                                              period => Period,
                                              child_options => ChildOptions,
                                              statem_options => StatemOptions })),
    msg_supervisor:run_setup(),
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



