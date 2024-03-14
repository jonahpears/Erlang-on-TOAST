-module(msg_supervisor).
-behaviour(supervisor).

-include("data_records.hrl").


% %% gen_server API
-export([ start_link/0,
          init/1,
          stop/0 ]).

%% custom wrappers for gen_statem
-export([ start_link/1,
          init/2 ]).

%% for exchanging PIDs between children
-export([ run_setup/0 ]).

-define(SERVER, ?MODULE).
-define(NAME, ?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

% -record(state, {}).



start_link() -> msg_supervisor:start_link([]).

start_link(Params) -> 
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params]),
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, Params),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.


stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init(Params) -> init(Params, #supervisor_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{HKey,HVal}|T], #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = _StatemOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, Params) -> init(T, maps:put(HKey, HVal, Params));
init([{HKey,HVal}|T], #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = ChildOptions, statem_options = _StatemOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, ChildOptions) -> init(T, maps:put(child_options, maps:put(HKey, HVal, ChildOptions), Params));
init([{HKey,HVal}|T], #supervisor_options{ strategy = _Strategy, intensity = _Intensity, period = _Period, child_options = _ChildOptions, statem_options = StatemOptions } = Params) 
    when is_atom(HKey) and is_map_key(HKey, StatemOptions) -> init(T, maps:put(statem_options, maps:put(HKey, HVal, StatemOptions), Params));
init([_H|T], Params) -> init(T, Params);
init([], #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions } = _Params) -> 
    SupFlags = #{ strategy  => Strategy,
                  intensity => Intensity,
                  period    => Period },
    ChildSpecList = [ child(msg_msger, ChildOptions, StatemOptions),
                      child(msg_acker, ChildOptions, StatemOptions) ],
    printout("~p,\n\tSupFlags: ~p\n\tChildSpecList: ~p.", [?FUNCTION_NAME, SupFlags, ChildSpecList]),
    {ok, {SupFlags, ChildSpecList}}.


child(Module, #child_options{ restart = Restart, shutdown = Shutdown, type = Type } = _ChildOptions, #statem_options{ allow_delayable_sends = AllowDelayableSends, printout_enabled = PrintoutEnabled } = _StatemOptions) -> 
    Args = maps:to_list(#{ allow_delayable_sends => AllowDelayableSends, printout_enabled => PrintoutEnabled }),
    printout("~p, (~p) Args: ~p.", [?FUNCTION_NAME, Module, Args]),
    #{ id => Module,
       start => {Module, start_link, [Args]},
       restart => Restart,
       shutdown => Shutdown,
       type => Type,
       modules => [Module] }.


run_setup() ->
    printout("~p.", [?FUNCTION_NAME]),

    Children = supervisor:which_children(?MODULE),

    {_, MsgerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
                                                                    {_, _, _, [msg_msger]} -> true;
                                                                    _Else -> false end end, Children)),

    {_, AckerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
                                                                    {_, _, _, [msg_acker]} -> true;
                                                                    _Else -> false end end, Children)),
    
    printout("~p, msger: ~p.", [?FUNCTION_NAME, MsgerID]),
    printout("~p, acker: ~p.", [?FUNCTION_NAME, AckerID]),

    MsgerID ! {self(), sup_init, AckerID},
    AckerID ! {self(), sup_init, MsgerID},
    printout("~p, complete.", [?FUNCTION_NAME]).
                                      

