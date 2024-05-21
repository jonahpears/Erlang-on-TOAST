-module(tpri_sup).
-file("tpri_sup.erl", 1).
-behaviour(supervisor).

-include("tpri_data_records.hrl").

%% supervisor exports
-export([ start_link/0,
          init/1 ]).
        
-export([ start_link/1 ]).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

%% supervisor -- start_link
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, Params),
  {ok, PID}.


%% init
init(Params) -> 
  Default = [ {child_options, #child_options{ restart = transient, 
                                              shutdown = 2000,
                                              type = worker }} ],
  ?MODULE:init(Params, Default, Default, Default).

init([], TpriParams, TrmParams, ImpParams) -> ?MODULE:init(finished, TpriParams, TrmParams, ImpParams);

init([H|T], TpriParams, TrmParams, ImpParams) -> 
  case H of 
    {tpri, Key, Val} ->
      %% TODO - does this do anything?
      TpriParams1 = TpriParams ++ [{Key, Val}],
      %% imp & trm do not change
      TrmParams1 = TrmParams,
      ImpParams1 = ImpParams;
    {trm, Key, Val} ->
      TrmParams1 = TrmParams ++ [{Key, Val}],
      %% tpri & imp do not change
      TpriParams1 = TpriParams,
      ImpParams1 = ImpParams;
    {imp, Key, Val} ->
      ImpParams1 = ImpParams ++ [{Key, Val}],
      %% tpri & trm do not change
      TpriParams1 = TpriParams,
      TrmParams1 = TrmParams;
    {Key, Val} -> 
      %% add to all
      TpriParams1 = TpriParams ++ [{Key, Val}],
      TrmParams1 = TrmParams ++ [{Key, Val}],
      ImpParams1 = ImpParams ++ [{Key, Val}];
    _ -> 
      %% all do not change
      TpriParams1 = TpriParams,
      TrmParams1 = TrmParams,
      ImpParams1 = ImpParams
  end,
  ?MODULE:init(T, TpriParams1, TrmParams1, ImpParams1);

%% finished parsing both params
init(finished, TpriParams, TrmParams, ImpParams) ->
  printout("~p, init().", [?FUNCTION_NAME]),
  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {tpri, sup_id, self()},
  %% 
  ChildOptions = maps:from_list(TpriParams),
  %% 
  SupFlags = #{ strategy => one_for_all,
                intensity => 1,
                period => 5 },
  %% 
  ChildSpecs = [ child_spec(trm_sup, trm_sup, ChildOptions, TrmParams), 
                 child_spec(imp_sup, imp_sup, ChildOptions, ImpParams) ],
  %% 
  {ok, {SupFlags, ChildSpecs}}.


child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type } = _ChildOptions, ChildParams) ->
  #{ id => ID,
     start => {Module, start_link, [[{name,ID}] ++ ChildParams]},
     restart => Restart,
     shutdown => Shutdown,
     type => Type,
     modules => [Module] }.


run_setup() ->
  receive {trm, TrmID} -> ok end,
  receive {imp, ImpID} -> ok end,

  printout("~p, complete,\n\ttrm: ~p\n\timp: ~p.", [?FUNCTION_NAME, TrmID, ImpID]).


