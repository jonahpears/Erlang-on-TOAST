-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").


%% @doc python interface for z3
z3() ->

  Z3FileName = filename:absname("")++"/src/toast/",
  io:format("\nZ3FileName:\t~p.\n",[Z3FileName]),

  {ok, P} = python:start([{python_path, [Z3FileName]},{python, "python3"}]),


  R2 = python:call(P, z3_interface, test, [test(constraints)]),
  io:format("\nR2:\t~p.\n",[R2]),


  python:stop(P),

  ok.
%%

%% @doc python interface for calling specific functions that use z3
ask_z3(Name, Args) ->
  %% get filename for path to python file (same dir)
  Z3FileName = filename:absname("")++"/src/toast/",
  %% start python instance
  {ok, P} = python:start([{python_path, [Z3FileName]},{python, "python3"}]),
  %% call function
  Response = python:call(P, z3_interface, Name, [Args]),
  io:format("\nResponse:\t~p.\n",[Response]),
  %% close python instance
  python:stop(P).
%%





test(not_t_reading) -> checker:ask_z3(not_t_reading,{#{"global"=>0,"x"=>4},5}).









%% @doc type-checking rules
%% @returns tuple of bool denoting if the result of the evaluation, and a list detailing the traces of rules visited.
-spec rule(map(), map(), toast:process(), map()) -> {boolean(), [[atom()]]} | {boolean(), [atom()]}.




%% @doc rule [Send]
%% type-checking one send from type
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [Recv]
%% type-checking single reception
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [Branch]
%% type-checking each reception in branch 
%% @see rule [Recv]
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [Timeout]
%% type-checking branches with timeouts 
%% @see rule [Branch]
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [IfTrue]
%% type-checking if-true
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [IfFalse]
%% type-checking if-false
rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
->
  ok;
%%


%% @doc rule [Timer]
%% type-checking setting timers
rule(Gamma, Theta, {'set', Timer, P}=_Process, {_Clocks,_Type}=Delta)
->
  %% set timer to 0 (regardless of if it existed before or not)
  Theta1 = maps:put(Timer, 0, Theta),

  %% continue type-checking (yields premise)
  {Premise, Trace} = rule(Gamma, Theta1, P, Delta),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Timer'),

  %% return 
  {Premise, Trace1};
%%


% %% @doc rule [Del-delta]
% %% type-checking non-deterministic delays
% rule(_Gamma, _Theta, {'delay',{t, DBC, N},P}=_Process, {_Clocks,_Type}=_Delta)
% ->

%   %% TODO :: this one might be a problem, since it requires us to check for all t' that model {t,DBC,N}

%   ok;
% %%


%% @doc rule [Del-t]
%% type-checking determined delays
rule(Gamma, Theta, {'delay',T,P}=_Process, {Clocks,Type}=Delta)
->
  %% increment timers and clocks y T
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta1, P, {Clocks1,Type}),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Del-t'),

  %% ask z3 if Delta is not T-reading
  NotTReading = ask_z3(not_t_reading,{Clocks,T}),

  %% construct premise and return
  Premise = NotTReading and Continuation,
  {Premise, Trace1};
%%


%% @doc rule [Rec]
%% type-checking recursive definitions
rule(Gamma, Theta, {'def', P, 'as', {Var, Msgs}}=_Process, {_Clocks,_Type}=Delta)
->
  %% check name not already used
  FreeName = not is_map_key(Var,Gamma),
  %% assign into Gamma
  Gamma1 = maps:put(Var,Msgs,Gamma),
  
  %% continue type-checking
  {Continuation, Trace} = rule(Gamma1, Theta, P, Delta),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Var'),

  %% construct premise and return
  Premise = FreeName and Continuation,
  {Premise, Trace1};
%%


%% @doc rule [Var]
%% type-checking recursive calls
rule(Gamma, _Theta, {'call', {Var, Msgs}}=_Process, {_Clocks,{'call', Name}=_Type}=_Delta)
->
  %% check names match
  NamesMatch = (Var=:=Name),
  %% check Gamma for recursive call
  IsInGamma = is_map_key(Name,Gamma),
  
  %% since we dont check datatypes, 
  %% check the labels of each message are present in Msgs
  MsgsValid = maps:fold(fun(Label,_Val,Ps) ->
    Ps and is_map_key(Label,Msgs)
  end, true, maps:get(Name,Gamma,#{})),

  %% TODO, check if rest of the premise is accounted for (checking Delta and Theta)

  %% construct premise and return
  Premise = NamesMatch and IsInGamma and MsgsValid,
  {Premise, ['Var']};
%%


%% @doc rule [End]
%% type-checking termination type
rule(_Gamma, _Theta, 'term'=_Process, {_Clocks,Type}=_Delta)
->
  %% type must be end
  Premise = (Type=:='end'),
  {Premise, ['End']};
%%

%% @doc rule [Weak]
%% type-checking termination type
rule(Gamma, Theta, Process, {_Clocks,'end'=_Type}=Delta)
->
  io:format("\n\nWarning, rule [Weak] indicates that the type as terminated, but the process has not.\n\nGamma:\t~p,\nTheta:\t~p,\nProcess:\t~p,\nDelta:~p.\n\n",[Gamma, Theta, Process, Delta]),
  {false, ['Weak']};
%%

%% @doc special case, allow recursive type to be unfolded
rule(Gamma, Theta, Process, {Clocks,{'def', _Name, S}=_Type}=_Delta)
->
  rule(Gamma, Theta, Process, {Clocks, S});
%%

%% @doc unhandled case
rule(Gamma, Theta, Process, Delta) ->
  io:format("\n\nWarning, unhandled case:\nGamma:\t~p,\nTheta:\t~p,\nProcess:\t~p,\nDelta:~p.\n\n",[Gamma, Theta, Process, Delta]),
  %% return false since unhandled
  {false, ['unknown']}.
%%



%% @doc handles updating traces that may have split
add_to_trace(Trace,Rule) 
when is_atom(Rule) and is_list(Trace) ->
  case is_atom(hd(Trace)) of
    true -> %% stayed single trace
      [Rule]++Trace;
    _ -> %% split into list of traces
      lists:foldl(fun(T,In) -> [[Rule]++T]++In end, [], Trace)
  end.
%%


%% @doc increments all Timers by T
%% @see increment_clocks/2
increment_timers(Timers,T)
when is_map(Timers) ->
  Timers1 = maps:fold(fun(Name,Val,In) ->
    maps:put(Name,Val+T,In)
  end, #{}, Timers),
  %% return
  Timers1.
%%

%% @doc increments all Clocks by T
%% @see increment_timers/2
increment_clocks(Clocks,T)
when is_map(Clocks) ->
  Clocks1 = maps:fold(fun(Name,Val,In) ->
    maps:put(Name,Val+T,In)
  end, #{}, Clocks),
  %% return
  Clocks1.
%%
