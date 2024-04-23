-module(trm_supervisor).
-file("trm_supervisor.erl", 1).
-behaviour(supervisor).

-include("trm_data_records.hrl").


% %% gen_server API
-export([ start_link/0,
          init/1,
          stop/0 ]).

%% custom wrappers for gen_statem
-export([ start_link/1,
          init/2 ]).

%% for exchanging PIDs between children
-export([ run_setup/0 ]).
-export([ run_setup/3 ]).

%% for providing the PIDs for testing
-export([ get_child/2 ]).

-define(NAME, ?MODULE).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?NAME, self()] ++ Params).

% -record(state, {}).



start_link() -> ?MODULE:start_link([]).

start_link([_H|_T]=Params) -> 
    % printout("~p, Params: ~p.", [?FUNCTION_NAME, Params]),
    {ok, Pid} = supervisor:start_link({local, ?NAME}, ?MODULE, Params),
    % printout("~p, Pid: ~p.", [?FUNCTION_NAME, Pid]),
    {ok, Pid}.


stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init([_H|_T]=Params) -> init(Params, #supervisor_options{});
init(Params) -> init([Params], #supervisor_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([{child_options,
      #child_options{ restart = _Restart, 
                      shutdown = _Shutdown, 
                      type = _Type } = HVal} | T], 
      #supervisor_options{ strategy = Strategy, 
                           intensity = Intensity, 
                           period = Period, 
                           child_options = _ChildOptions, 
                           statem_options = StatemOptions, 
                           child_spec = ChildSpec } = _Params) -> 
    Params1 = #supervisor_options{ strategy = Strategy,
                                   intensity = Intensity,
                                   period = Period,
                                   child_options = HVal,
                                   statem_options = StatemOptions,
                                   child_spec = ChildSpec },
    init(T, Params1);

init([{statem_options,#statem_options{ allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled } = HVal}|T], #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = _StatemOptions, child_spec = ChildSpec } = _Params) -> 
    Params1 = #supervisor_options{ strategy = Strategy,
                                   intensity = Intensity,
                                   period = Period,
                                   child_options = ChildOptions,
                                   statem_options = HVal,
                                   child_spec = ChildSpec },
    init(T, Params1);

init([{child_spec, HVal}|T], #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec } = _Params) -> 
    Params1 = #supervisor_options{ strategy = Strategy,
                                   intensity = Intensity,
                                   period = Period,
                                   child_options = ChildOptions,
                                   statem_options = StatemOptions,
                                   child_spec = maps:merge(HVal, ChildSpec) },
    init(T, Params1);

%% skip unhandled params
init([_H|T], Params) -> init(T, Params);

%% processed all params
init([], #supervisor_options{ strategy = Strategy, intensity = Intensity, period = Period, child_options = ChildOptions, statem_options = StatemOptions, child_spec = ChildSpec } = _Params) -> 
    SupFlags = #{ strategy  => Strategy,
                  intensity => Intensity,
                  period    => Period },
    % ChildSpecList = [ child_spec(msg_msger, ChildOptions, StatemOptions),
    %                   child_spec(msg_acker, ChildOptions, StatemOptions) ],
    % printout("~p, ChildSpec: ~p.", [?FUNCTION_NAME, ChildSpec]),
    ChildSpecList = maps:fold(fun(Key,Val,Acc) -> 
                                NewChild = child_spec(msg_worker, Key, ChildOptions, StatemOptions, Val),
                                % printout("~p, ChildSpecList(~p,\n\t~p,\n\t~p) = ~p.", [?FUNCTION_NAME, Key, Val, Acc, NewChild]),
                                Acc ++ [NewChild]
                            end, [], ChildSpec),

    % ChildSpecList = [],

    % printout("~p,\n\tSupFlags: ~p\n\tChildSpecList: ~p.", [?FUNCTION_NAME, SupFlags, ChildSpecList]),
    % register(msup, self()),
    {ok, {SupFlags, ChildSpecList}}.


child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type } = _ChildOptions, #statem_options{ allow_delayable_sends = AllowDelayableSends, printout_enabled = PrintoutEnabled } = _StatemOptions, ChildSpec) -> 
    Args = maps:to_list(#{ allow_delayable_sends => AllowDelayableSends, printout_enabled => PrintoutEnabled }) ++ ChildSpec ++ [],
    printout("~p, (~p: ~p).", [?FUNCTION_NAME, Module, ID]),
    % printout("~p, (~p: ~p) Args: ~p.", [?FUNCTION_NAME, ID, Module, Args]),
    #{ id => ID,
    %    start => {Module, start_link, []},
       start => {Module, start_link, [[{name,ID}] ++ Args]},
    %    start => {Module, start_link, Args},
       restart => Restart,
       shutdown => Shutdown,
       type => Type,
       modules => [Module] }.


run_setup() ->
    printout("~p.", [?FUNCTION_NAME]),

    Children = supervisor:which_children(?MODULE),

    {_, MsgerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
                                                                    {msger, _, _, [msg_worker]} -> true;
                                                                    _Else -> false end end, Children)),

    {_, AckerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
                                                                    {acker, _, _, [msg_worker]} -> true;
                                                                    _Else -> false end end, Children)),
    
    printout("~p, msger: ~p.", [?FUNCTION_NAME, MsgerID]),
    printout("~p, acker: ~p.", [?FUNCTION_NAME, AckerID]),

    MsgerID ! {self(), sup_init, AckerID},
    AckerID ! {self(), sup_init, MsgerID},

    %% register pid to name
    register(msger, MsgerID),
    register(acker, AckerID),

    

    printout("~p, complete.", [?FUNCTION_NAME]).


get_child(ID, Module) ->
    Children = supervisor:which_children(?MODULE),
    {_, ChildID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
                                                    {ID, _, _, [Module]} -> true;
                                                    _Else -> false end end, Children)),
    {ok, ChildID}.



run_setup(ChildOptions, StatemOptions, ChildSpec) ->
    printout("~p.", [?FUNCTION_NAME]),


    ChildMap = maps:from_list(maps:fold(fun(Key,Val,Acc) -> 
                                NewChildSpec = child_spec(msg_worker, Key, ChildOptions, StatemOptions, Val),
                                % case supervisor:start_child({local, Key}, NewChildSpec) of
                                case supervisor:start_child({via, msg_worker, Key}, NewChildSpec) of
                                    {ok, NewChild} -> 
                                        Acc ++ [{Key, NewChild}];
                                    {error, Err} ->
                                        printout("~p, ~p error: ~p.", [?FUNCTION_NAME, Key, Err])
                                end
                                % NewChild = supervisor:start_link({local, Key}, msg_worker, NewChildSpec),
                            end, [], ChildSpec)),


    MsgerID = maps:get(msger, ChildMap),
    AckerID = maps:get(acker, ChildMap),

    % Children = supervisor:which_children(?MODULE),

    % {_, MsgerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
    %                                                                 {_, _, _, [msg_msger]} -> true;
    %                                                                 _Else -> false end end, Children)),

    % {_, AckerID, _, _} = lists:last(lists:filter(fun(Child) -> case Child of 
    %                                                                 {_, _, _, [msg_acker]} -> true;
    %                                                                 _Else -> false end end, Children)),
    
    printout("~p, msger: ~p.", [?FUNCTION_NAME, MsgerID]),
    printout("~p, acker: ~p.", [?FUNCTION_NAME, AckerID]),

    MsgerID ! {self(), sup_init, AckerID},
    AckerID ! {self(), sup_init, MsgerID},
    printout("~p, complete.", [?FUNCTION_NAME]).


