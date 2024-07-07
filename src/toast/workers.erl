-module(workers).


-include_lib("stdlib/include/assert.hrl").
-include("utils.hrl").

%% @doc starts evaluation workers for each integer of the given range, and collects their IDs in a list to return
% -spec start_evaluation_workers(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, integer(), float(), float(), float()) -> list().

%% @doc for starting the workers for rules (del-delta and recv)
start_evaluation_workers(Kind, {_Gamma, _Theta, _P, _Delta}=Judgement, #{is_first:=IsFirst,is_last:=IsLast,is_lower_exclusive:=IsLowerExclusive,is_upper_exclusive:=IsUpperExclusive,lower:=Lower,upper:=Upper,decrement:=Decrement}=_Args) ->
  %% if upper is exclusive, set counter for first to be one precision out
  %% since the workers count down, this will ensure they do not evaluate inclusive of the bound
  %% if not first or last, then just set to 1.0
  Counter = case IsLast of true -> case IsUpperExclusive of true -> 1.0-Decrement; _ -> 1.0 end; _ -> 1.0 end,
  %% check if this is also first, (i.e., for a 1 integer non-det delay)
  {Counter1,IsLowerExclusive1} = case IsFirst of true -> case IsLowerExclusive of true -> {Counter-Decrement,false}; _ -> {Counter,false} end; _ -> {Counter,IsLowerExclusive} end,
  %% we floor and increment lower to ensure that it is an integer moving forward
  Lower1 = case (IsFirst or IsLast) of true -> floor(Lower)+1.0; _ -> Lower+1.0 end,
  %% determine T to pass to worker
  T = case IsLast of true -> case IsUpperExclusive of true -> Upper;  _ -> Lower1 end; _ -> Lower1 end,
  %% if is last, then the distance between Lower and Upper should be less than 1
  %% next is last if the difference between Lower1 and Upper is leq 1
  IsNextLast = case IsLast of true -> ?assert((abs(Upper-Lower)=<1)), false; _ -> (abs(Upper-Lower1)=<1.0) end,
  %% spawn worker for this integer
  PID = self(),
  WorkerID = spawn(?MODULE, evaluation_worker, [Kind, Judgement, precision_rounding(Counter1), precision_rounding(T), Decrement, PID, maps:get(show_print,_Args,?WORKER_SHOW_TRACE)]),
  %% create new map 
  Map1 = #{ is_upper_exclusive=>IsUpperExclusive,
            is_lower_exclusive=>IsLowerExclusive1,
            lower=>precision_rounding(Lower1),
            upper=>Upper,
            decrement=>Decrement,
            is_last=>IsNextLast,
            is_first=>false },
  %% call to start next worker 
  WorkerIDs = case IsLast of true -> []; _ -> start_evaluation_workers(Kind, Judgement, maps:put(show_print,maps:get(show_print,_Args,?WORKER_SHOW_TRACE),Map1)) end,
  %% add to worker IDs and return
  IDs = WorkerIDs ++ [WorkerID],
  IDs;
%%


%% @doc unexpected kind
start_evaluation_workers(Kind, {Gamma, Theta, P, Delta}=_Judgement, Map) ->
  io:format("\n\n(~p) Warning, unknown start_evaluation_workers called: ~p.\nGamma: ~p, Theta: ~p,\nP: ~p,\nDelta: ~p,\nMap: ~p.\n",[?LINE,Kind, Gamma, Theta, P, Delta, Map]),
  [].
%%



%% @doc worker that tail-recursively evaluates delay of incrementing value.
-spec evaluation_worker(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, float(), float(), float(), pid(), boolean()) -> atom().

%% @doc the first iteration of the worker sends back the result, evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_recv=Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}=Judgement, Counter, T, Decrement, PID, ShowPrint)
when is_float(T) and is_float(Decrement) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace1} = checker:rule(Gamma,Theta1,P,{Clocks2,Type},#{show_print=>ShowPrint}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement, ShowPrint),
  %% send results to PID
  Holds = Result and NextEval,
  PID ! {self(), {eval, {Holds,[Trace1]++Trace2}}, {range, T-Counter, T}},
  %% return OK
  ok;
%%

%% @doc the first iteration of the worker sends back the result, evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_del_delta=Kind, {Gamma, Theta, P, Delta}=Judgement, Counter, T, Decrement, PID, ShowPrint)
when is_float(T) and is_float(Decrement) ->
  %% evaluate for this current value of T
  {Result,Trace1} = checker:rule(Gamma,Theta,{'delay',T,P},Delta,#{show_print=>ShowPrint}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement, ShowPrint),
  %% send results to PID
  Holds = Result and NextEval,
  PID ! {self(), {eval, {Holds,[Trace1]++Trace2}}, {range, T-Counter, T}},
  %% return OK
  ok;
%%

%% @doc unexpected worker starter
evaluation_worker(Kind, {Gamma, Theta, P, Delta}=_Judgement, Counter, T, Decrement, PID, _ShowPrint)
when is_float(T) and is_float(Decrement) ->
  io:format("\n\n(~p) Warning, unknown evaluation_worker called: ~p.\nGamma: ~p, Theta: ~p,\nP: ~p,\nDelta: ~p,\nCounter: ~p,\nT: ~p,\nDecrement: ~p,\nPID: ~p.\n",[?LINE,Kind, Gamma, Theta, P, Delta, Counter, T, Decrement,PID]),
  ok.
%%


-spec evaluation_worker(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, integer(), float(), float(), boolean()) -> {boolean(),list()}.

%% @doc for catching the final evaluation
evaluation_worker(premise_recv=_Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}, Counter, T, _Decrement, ShowPrint)
when (Counter=<0) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace} = checker:rule(Gamma,Theta1,P,{Clocks2,Type},#{show_print=>ShowPrint}),
  %% return
  {Result,[Trace]};
%%

%% @doc evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_recv=Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}=Judgement, Counter, T, Decrement, ShowPrint)
when is_float(T) and is_float(Decrement) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace1} = checker:rule(Gamma,Theta1,P,{Clocks2,Type},#{show_print=>ShowPrint}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement, ShowPrint),
  %% return result
  Holds = Result and NextEval,
  {Holds,[Trace1]++Trace2};
%%

%% @doc for catching the final evaluation
evaluation_worker(premise_del_delta=_Kind, {Gamma, Theta, P, Delta}, Counter, T, _Decrement, ShowPrint)
when (Counter=<0) ->
  %% evaluate for this current value of T
  {Result,Trace} = checker:rule(Gamma,Theta,{'delay',T,P},Delta,#{show_print=>ShowPrint}),
  %% return
  {Result,[Trace]};
%%

%% @doc evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_del_delta=Kind, {Gamma, Theta, P, Delta}=Judgement, Counter, T, Decrement, ShowPrint)
when is_float(T) and is_float(Decrement) ->
  %% evaluate for this current value of T
  {Result,Trace1} = checker:rule(Gamma,Theta,{'delay',T,P},Delta,#{show_print=>ShowPrint}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement, ShowPrint),
  %% return result
  Holds = Result and NextEval,
  {Holds,[Trace1]++Trace2};
%%

%% @doc unknown worker
evaluation_worker(Kind, {Gamma, Theta, P, Delta}, Counter, T, Decrement, _ShowPrint) -> 
  io:format("\n\n(~p) Warning, unknown evaluation_worker called: ~p.\nGamma: ~p, Theta: ~p,\nP: ~p,\nDelta: ~p,\nCounter: ~p,\nT: ~p,\nDecrement: ~p.\n",[?LINE,Kind, Gamma, Theta, P, Delta, Counter, T, Decrement]),
  false.
%%
