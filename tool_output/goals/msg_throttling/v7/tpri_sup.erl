-module(tpri_sup).
-file("tpri_sup.erl", 1).
-behaviour(supervisor).

-include("tpri_data_records.hrl").


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



%% no params given, skip to final case
start_link() -> ?MODULE:start_link([]).
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
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
    {ok, Pid}.


stop() -> exit(whereis(?MODULE), shutdown).


-spec init([]) -> {atom(), atom(), map()}.
init(Params) -> init(Params, #tpri_sup_options{}).


-spec init([{atom(),any()}|[]], map()) -> {atom(), atom(), map()}.
init([ {child_options, #child_options{} = HVal} | T ], 
       #tpri_sup_options{ 
              trm_options = #supervisor_options{ strategy = Strategy, 
                                                 intensity = Intensity, 
                                                 period = Period, 
                                                 child_options = _ChildOptions, 
                                                 statem_options = StatemOptions },
              imp_options = ImpOptions,
              tpri_child_specs = ChildSpecs } = _Params) -> 
    %% add as child_options
    Params1 = #supervisor_options{ strategy = Strategy,
                                   intensity = Intensity,
                                   period = Period,
                                   child_options = HVal,
                                   statem_options = StatemOptions },
    init(T, #tpri_sup_options{ trm_options = Params1, imp_options = ImpOptions, tpri_child_specs = ChildSpecs} );

init([ {statem_options,#statem_options{} = HVal} | T ], 
       #tpri_sup_options{ 
              trm_options = #supervisor_options{ strategy = Strategy, 
                                                 intensity = Intensity, 
                                                 period = Period, 
                                                 child_options = _ChildOptions, 
                                                 statem_options = StatemOptions },
              imp_options = ImpOptions,
              tpri_child_specs = ChildSpecs } = _Params) -> 
    %% add as statem_options
    Params1 = #supervisor_options{ strategy = Strategy,
                                   intensity = Intensity,
                                   period = Period,
                                   child_options = ChildOptions,
                                   statem_options = HVal },
    init(T, #tpri_sup_options{ trm_options = Params1, imp_options = ImpOptions, tpri_child_specs = ChildSpecs} );

init([ {child_spec, HVal} | T ], 
       #tpri_sup_options{ 
              trm_options = TRMOptions,
              imp_options = ImpOptions,
              tpri_child_specs = ChildSpecs } = _Params) -> 
    %% add/merge as child_spec
    Params1 = maps:merge(HVal, ChildSpecs) 
                                   
    init(T, #tpri_sup_options{ trm_options = TRMOptions, imp_options = ImpOptions, tpri_child_specs = Params1} );

%% skip unhandled params
init([_H|T], Params) -> init(T, Params);

%% processed all params
init([], #tpri_sup_options{ 
              trm_options = #supervisor_options{ strategy = Strategy1, 
                                                 intensity = Intensity1, 
                                                 period = Period1, 
                                                 child_options = ChildOptions1, 
                                                 statem_options = StatemOptions1 },
              imp_options = #supervisor_options{ strategy = Strategy2, 
                                                 intensity = Intensity2, 
                                                 period = Period2, 
                                                 child_options = ChildOptions2 },
              tpri_child_specs = ChildSpecs } = _Params) -> 
    %% setup supervisory flags
    SupFlags = #{ strategy  => Strategy,
                  intensity => Intensity,
                  period    => Period },
    %% for timed runtime monitors (FSM)
    TRMChildSpecList = maps:fold(fun(Key,Val,Acc) -> 
                                NewChild = child_spec(msg_worker, Key, ChildOptions1, StatemOptions1, Val),
                                Acc ++ [NewChild]
                            end, [], ChildSpecs),
    %% for implementations
    ImpChildSpecList = maps:fold(fun(Key,Val,Acc) -> 
                                NewChild = child_spec(Key, Key, ChildOptions2, #{}, Val),
                                Acc ++ [NewChild]
                            end, [], ChildSpecs),
    ChildSpecList = TRMChildSpecList ++ ImpChildSpecList,
    {ok, {SupFlags, ChildSpecList}}.


child_spec( Module, ID, 
            #child_options{ restart = Restart, 
                            shutdown = Shutdown, 
                            type = Type } = _ChildOptions, 
            #statem_options{ allow_delayable_sends = AllowDelayableSends, 
                             printout_enabled = PrintoutEnabled } = _StatemOptions, 
            ChildSpec ) -> 
    Args = maps:to_list(#{ allow_delayable_sends => AllowDelayableSends, printout_enabled => PrintoutEnabled }) ++ ChildSpec ++ [],
    printout("~p, (~p: ~p).", [?FUNCTION_NAME, Module, ID]),
    #{ id => ID,
       start => {Module, start_link, [[{name,ID}] ++ Args]},
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


