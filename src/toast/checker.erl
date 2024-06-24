-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

-define(DECIMAL_PRECISION,8).

-include_lib("stdlib/include/assert.hrl").




test(not_t_reading) -> checker:ask_z3(not_t_reading,{#{"global"=>0,"x"=>4},5}).

%% @doc wrapper function for calling the default function in the python program.
-spec z3(list()) -> any().
z3(Args) -> z3(ask_z3, Args).

%% @doc python interface for calling specific functions that use z3
-spec z3(atom(), list()) -> any().
z3(Name, Args) ->
  %% get filepath to python file (same dir)
  Z3FilePath = filename:absname("")++"/src/toast/",
  %% start python instance
  {ok, P} = python:start([{python_path, [Z3FilePath]},{python, "python3"}]),
  %% call function
  Response = python:call(P, z3_interface, Name, Args),
  io:format("\nResponse:\t~p.\n",[Response]),
  %% close python instance
  python:stop(P),
  %% return response
  Response.
%%

%% @doc helper functions to ask z3 via python in a more sensible (but convoluted way)
%% this is necessary since some of the datatypes do not transfer well
-spec ask_z3(atom(), map()|list()) -> {atom(), boolean()|list()}.

%% @doc for each Clock valuation, asks z3 to check if 
ask_z3(t_reading, #{clocks:=Clocks,type:=Type,t:=_T}) ->
  %% round T down to accepted precision
  T = precision_rounding(_T),
  %% get as list of interactions
  Interactions = toast:interactions(Type),

  %% get only constraints of receptions
  ReceptionConstraints = lists:foldl(fun({Direction,_Msg,Constraints,_Resets,_S}=_I, In) ->
    case Direction of 
      send -> In; %% do nothing, skip this interaction
      recv -> In++[Constraints] %% add constraints to list
    end
  end, [], Interactions),

  %% for each reception, go through each clock and ask z3 if there exists t'<t such that constraints are satisfied
  AskZ3 = lists:foldl(fun(Constraint, {IsOk, InSat}) ->
    %% first, check constraint for each clock
    Results = lists:foldl(fun(Clock, InResponses) ->
      %% check if clock is constrained 
      case toast:is_clock_constrained(Clock,Constraint) of
        %% if constrained, evaluate and add to responses
        true -> 
          ExecString = to_python_exec_string(t_reading, #{clock=>Clock,delta=>Constraint,t=>T}),
          Z3Response = z3(t_reading,ExecString),
          InResponses++[Z3Response];

        %% if not constrained, skip
        _ -> InResponses

      end
      %%
    end, [], Clocks),
    %% next, check result of all clocks
    Result = lists:foldl(fun(R, {InnerIsOk, InResults}) -> 
      %% check if ok 
      case InnerIsOk of
        ok -> 
          %% check response
          case is_boolean(R) of

            %% if bool, preserve previous IsOk, and conj with current Sat
            true -> {InnerIsOk, (R and InResults)};

            %% if not bool something has gone wrong, and is not okay
            false -> {error, [InResults,R]}

          end;
        %% not ok, just add to list of InSat
        _Else -> 
          {InnerIsOk, InResults++[R]}
      end
    end, {ok, true}, Results),
    %% check if ok 
    case IsOk of
      ok -> 
        %% check response
        case is_boolean(Result) of

          %% if bool, preserve previous IsOk, and conj with current Sat
          true -> {IsOk, (Result and InSat)};

          %% if not bool something has gone wrong, and is not okay
          false -> {error, [InSat,Result]}

        end;

      %% not ok, just add to list of InSat
      _Else -> 
        {IsOk, InSat++[Result]}
    end
  end, {ok,true}, ReceptionConstraints),

  %% check response from z3
  case AskZ3 of %lists:foldl(AskZ3, {ok, true}, ReceptionConstraints)

    {ok, IsSatisfied} -> {ok, IsSatisfied};

    Err -> 
      io:format("\nError, t_reading did not return as expected...\nResult:\t~p.\n",[Err]),
      Err

  end;
%%

%% @doc inverse of t_reading
%% @see ask_z3(t_reading, {Clocks,T})
ask_z3(not_t_reading, #{clocks:=_Clocks,interactions:=_Interactions,t:=_T}=Map) ->
  %% inverse t_reading
  case ask_z3(t_reading, Map) of 
    {ok, Result} -> {not Result};
    Err -> 
      io:format("\nError, not_t_reading did not return as expected...\nResult:\t~p.\n",[Err]),
      Err
  end; 
%%


%% @doc catch for unrecognised (invalid) questions for z3
ask_z3(Unknown, Args) ->
  io:format("\n\nWarning, ~p, unrecognised question: ~p,\n\twith args: ~p.\n",[?FUNCTION_NAME,Unknown,Args]),
  {unknown,false}.
%%



%% @doc helper function for converting constraints to string to be used by python
-spec to_python_exec_string(atom(), map()) -> {atom(), string()}.

%% @doc for each clock in Clocks, call z3 to see if there exists t'<T such that delta holds 
%% ! make sure to initialise the variables of clocks always, including those within the delta -- later on be more clever about this and only test for clocks present in the delta
to_python_exec_string(t_reading, #{clock:=Clock,delta:=Delta,t:=_T}=_Args) ->
  %% TODO make python code for calling z3, for each clock against delta exec 

  %% ! from here
  String = "x = Int('x')\r\n# y = Int('y')",
  %% return string
  String;
%%

%% @doc catch unhandled kinds
to_python_exec_string(Unknown, Args) ->
  io:format("\n\nWarning, ~p, unrecognised mode: ~p,\n\twith args: ~p.\n",[?FUNCTION_NAME,Unknown,Args]),
  ok.
%%





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

%% @doc rounds to DECIMAL_PRECISION decimal places
precision_rounding(Float) 
when is_float(Float) -> 
  Offset = math:pow(10.0,?DECIMAL_PRECISION),
  round(Float*Offset)/Offset.
%%
