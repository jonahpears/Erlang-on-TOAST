-module(trm_sup).
-file("trm_sup.erl", 1).
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
  SupFlags = #{ strategy => one_for_all,
                intensity => 1,
                period => 5 },
  ChildOptions = #child_options{ restart = transient, 
                                 shutdown = 2000,
                                 type = worker },
  ChildParams = [ #statem_options{ allow_delayable_sends = false,
                                   printout_enabled = true } ],
  ?MODULE:init(Params, SupFlags, ChildOptions, ChildParams, Children).

init([], SupFlags, ChildOptions, ChildParams, Children) -> 
  ?MODULE:init(finished, SupFlags, ChildOptions, ChildParams, Children);

init([H|T], SupFlags, ChildOptions, ChildParams, Children) ->
  case H of 
    {sup_flags, Val} -> 
      SupFlags1 = maps:merge(Val, SupFlags),
      %% childparams do not change
      ChildOptions1 = ChildOptions,
      ChildParams1 = ChildParams;
    {child_options, Val} -> 
      ChildOptions1 = Val,
      %% supflags & childparams do not change
      SupFlags1 = SupFlags,
      ChildParams1 = ChildParams;
    {statem_options, Val} -> 
      ChildParams1 = ChildParams ++ Val,
      %% supflags & childoptions do not change
      SupFlags1 = SupFlags,
      ChildOptions1 = ChildOptions;
    _ -> 
      %% nothing happens
      SupFlags1 = SupFlags,
      ChildOptions1 = ChildOptions,
      ChildParams1 = ChildParams
  end,
  ?MODULE:init(T, SupFlags1, ChildOptions1, ChildParams1, Children1);

init(finished, SupFlags, ChildOptions, ChildParams, Children) -> 
  printout("~p, init().", [?FUNCTION_NAME]),
  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {trm, sup_id, self()},
  %% 
  ChildSpecs = lists:foldl(fun({Key, Val}) -> 
      child_spec(trm_worker, Key, ChildOptions, ChildParams ++ [{schema, Val}])
    end, [], Children),
  %% 
  {ok, {SupFlags, ChildSpecs}}.

child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type } = _ChildOptions, ChildParams) ->
  #{ id => ID,
      start => {Module, start_link, [[{name,ID}] ++ ChildParams]},
      restart => Restart,
      shutdown => Shutdown,
      type => Type,
      modules => [Module] }.

run_setup(WorkerA, WorkerB) ->
  receive {WorkerA, WorkerAID}=MsgA -> ok end,
  receive {WorkerB, WorkerBID}=MsgB -> ok end,

  printout("~p, complete,\n\tworker a: ~p\n\tworker b: ~p.", [?FUNCTION_NAME, MsgA, MsgB]).



