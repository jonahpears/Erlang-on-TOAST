-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

%% how fine grained to check decimals
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
  % io:format("\nResponse: ~p.\n",[Response]),
  %% close python instance
  python:stop(P),
  %% return response
  Response.
%%

%% @doc helper functions to ask z3 via python in a more sensible (but convoluted way)
%% this is necessary since some of the datatypes do not transfer well
-spec ask_z3(atom(), map()|list()) -> {atom(), boolean()|list()}.

%% @doc for each Clock valuation, asks z3 to check if 
ask_z3(t_reading, #{clocks:=Clocks,type:=Type,t:=_T})
when is_list(Clocks) and is_float(_T) ->
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

  %% if no receptions, then t_reading false
  case length(ReceptionConstraints)==0 of 
    true -> {ok, false};

    %% otherise, receptions
    _ ->
      %% for each reception, go through each clock and ask z3 if there exists t'<t such that constraints are satisfied
      AskZ3 = lists:foldl(fun(Constraint, {IsOk, InSat}) ->
        %% first, check constraint for each clock
        Results = lists:foldl(fun({Clock,Valuation}, InResponses) ->
          %% check if clock is constrained 
          case toast:is_clock_constrained(Clock,Constraint) of
            %% if constrained, evaluate and add to responses
            true -> 
              % io:format("\nclock (~p) is constrained by: ~p.\n",[Clock,Constraint]),
              %% get python string to execute using z3
              ExecString = to_python_exec_string(t_reading, #{clock=>{Clock,Valuation},delta=>Constraint,t=>T}),
              % io:format("\nexec string:\n~s.\n",[ExecString]),
              % timer:sleep(5000),
              %% send to python program and get response
              Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
              % io:format("\nZ3Response: ~p. (exists t'<~p: [~p:~p+t'] |= ~p.)\n",[Z3Response,T,Clock,Valuation,Constraint]),
              %% collect response
              InResponses++[Z3Response];

            %% if not constrained, skip
            _ -> InResponses
            % _ -> io:format("\nskipping, clock (~p) not constrained by: ~p.\n",[Clock,Constraint]), InResponses

          end
          %%
        end, [], Clocks),
        %% next, check result of all clocks
        % io:format("\nResults: ~p.\n",[Results]),
        Result = lists:foldl(fun(R, {InnerIsOk, InResults}) -> 
          %% check if ok 
          case InnerIsOk of
            ok -> 
              %% check response
              case is_boolean(R) of

                %% if bool, preserve previous IsOk, and conj with current Sat
                true -> {InnerIsOk, (R and InResults)};

                %% if not bool something has gone wrong, and is not okay
                false -> {error, [InResults]++[R]}

              end;
            %% not ok, just add to list of InSat
            _Else -> 
              {InnerIsOk, InResults++[R]}
          end
        end, {ok, true}, Results),
        % io:format("\nResult: ~p.\n",[Result]),
        %% check if ok 
        case IsOk of
          ok -> 
            %% check response
            case Result of 
              %% z3 worked successfully
              {ok, SatResult} -> 
                case is_boolean(SatResult) of

                  %% if bool, preserve previous IsOk, and conj with current Sat
                  true -> {IsOk, (SatResult and InSat)};

                  %% if not bool something has gone wrong, and is not okay
                  false -> {error, [InSat,SatResult]}

                end;
              %% some error occured
              _ -> Result

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

      end
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
-spec to_python_exec_string(atom(), map()) -> string()|{list(),string()}.

%% @doc for each clock in Clocks, call z3 to see if there exists t'<T such that delta holds 
%% this is only for simple constraints (no diagonal)
%% ! make sure to initialise the variables of clocks always, including those within the delta -- later on be more clever about this and only test for clocks present in the delta
to_python_exec_string(t_reading, #{clock:={Clock,Valuation},delta:=Delta,t:=T}=_Args)
when is_atom(Clock) and is_float(T) ->
  %% stringify Delta
  {VarList, DeltaString} = to_python_exec_string(constraints, #{delta=>Delta,t_reading=>true}),
  %% build code
  {ConstantNames, ConstantDeclarations} = lists:foldl(fun({VarName,VarValue}, {InNames,InDecls}) -> 
    % io:format("\nInNames: ~s.\n",[InNames]),
    % io:format("\nInDecls: ~s.\n",[InDecls]),
    %% this will appear inside the "Ints(...)" call in the python with z3
    NewNames = InNames++" "++VarName,
    %% this will be added to the "s.add(...)" to initialise the values of the constants
    DeclConnector = case length(InDecls)==0 of true -> ""; _ -> ", " end,
    NewDecls = io_lib:format("~s~s~s==~w",[InDecls,DeclConnector,VarName,VarValue]),
    %% return strings as tuple
    % io:format("\nNewNames: ~s.\n",[NewNames]),
    % io:format("\nNewDecls: ~s.\n",[NewDecls]),
    {NewNames,NewDecls}
  end, {"", ""}, VarList),
  % io:format("\nVarList: ~p.\n",[VarList]),
  % io:format("\nDeltaString: ~s.\n",[DeltaString]),
  % io:format("\nConstantNames: ~s.\n",[ConstantNames]),
  % io:format("\nConstantDeclarations: ~s.\n",[ConstantDeclarations]),
  % ExecString = io_lib:format("~w = Int('~w')\ns = Solver()\ns.add(~w==~w, ~s)\nresult = s.check()",[Clock,Clock,Clock,Valuation,DeltaString]),
  ExecString = io_lib:format("~w, n = Ints('~w~s')\nt, t_ = Reals('t t_')\ns = Solver()\ns.add(~w==~w, t==~w, ~s)\ns.add(Exists(t_,And(0<=t_,t_<t, ~s)))\nresult = s.check()",[Clock,Clock,ConstantNames,Clock,Valuation,T,ConstantDeclarations,DeltaString]),
  % io:format("\nExecString: ~s.\n",[ExecString]),
  %% return string
  ExecString;
%%

%% @doc catch constraints (negation)
to_python_exec_string(constraints, #{delta:={'not',Constraints}}=Args) -> 
  Offset = maps:get(constant_offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  {VarList, ConstraintString} = to_python_exec_string(constraints, #{delta=>Constraints,offset=>Offset,t_reading=>TReading}),
  {VarList, io_lib:format("Not(~s)",[ConstraintString])};
%%

%% @doc catch constraints (conjunction)
to_python_exec_string(constraints, #{delta:={Constraints1,'and',Constraints2}}=Args) -> 
  Offset = maps:get(constant_offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  {VarList1, ConstraintString1} = to_python_exec_string(constraints, #{delta=>Constraints1,offset=>Offset++"1",t_reading=>TReading}),
  {VarList2, ConstraintString2} = to_python_exec_string(constraints, #{delta=>Constraints2,offset=>Offset++"2",t_reading=>TReading}),
  %% if t_reading, no need to wrap in And
  DeltaString = case TReading of 
      true -> io_lib:format("~s, ~s",[ConstraintString1,ConstraintString2]);
      _ -> io_lib:format("And(~s, ~s)",[ConstraintString1,ConstraintString2])
  end,
  {VarList1++VarList2, DeltaString};
%%

%% @doc catch constraints (simple)
to_python_exec_string(constraints, #{delta:={Clock,DBC,Constant}}=Args)
when is_atom(Clock) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(constant_offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  ConstantString = "n"++Offset,
  DeltaString = case TReading of 
      true -> io_lib:format("(~w+t_) ~s ~s",[Clock,to_python_exec_string(constraints, #{dbc=>DBC}),ConstantString]);
      _ -> io_lib:format("~w ~s ~s",[Clock,to_python_exec_string(constraints, #{dbc=>DBC}),ConstantString])
  end,
  {[{ConstantString,Constant}],DeltaString};
%%

%% @doc catch constraints (diagonal)
to_python_exec_string(constraints, #{delta:={Clock1,'-',Clock2,DBC,Constant}}=Args)
when is_atom(Clock1) and is_atom(Clock2) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(constant_offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  ConstantString = "n"++Offset,
  DeltaString = case TReading of 
      true -> io_lib:format("(~w+t_) - (~w+t_) ~s ~s",[Clock1,Clock2,to_python_exec_string(constraints, #{dbc=>DBC}),ConstantString]);
      _ -> io_lib:format("~w - ~w ~s ~s",[Clock1,Clock2,to_python_exec_string(constraints, #{dbc=>DBC}),ConstantString])
  end,
  {[{ConstantString,Constant}],DeltaString};
%%

%% @doc catch constraints (true)
to_python_exec_string(constraints, #{delta:=true}=_Args) -> " true ";

%% @doc catch DBC
to_python_exec_string(constraints, #{dbc:='gtr'}=_Args) -> ">";
to_python_exec_string(constraints, #{dbc:='geq'}=_Args) -> ">=";
to_python_exec_string(constraints, #{dbc:='les'}=_Args) -> "<";
to_python_exec_string(constraints, #{dbc:='leq'}=_Args) -> "<=";
to_python_exec_string(constraints, #{dbc:='eq'}=_Args) -> "=";
to_python_exec_string(constraints, #{dbc:='neq'}=_Args) -> "!=";

%% @doc catch unhandled kinds
to_python_exec_string(Unknown, Args) ->
  io:format("\n\nWarning, ~p, unrecognised mode: ~p,\n\twith args: ~p.\n",[?FUNCTION_NAME,Unknown,Args]),
  "".
%%





%% @doc type-checking rules
%% @returns tuple of bool denoting if the result of the evaluation, and a list detailing the traces of rules visited.
-spec rule(map(), map(), toast:process(), map()) -> {boolean(), [[atom()]]} | {boolean(), [atom()]}.

%% @doc helper function to make sure /timersclocks are map not list of tuples
rule(Gamma, Theta, Process, Delta) 
when is_list(Theta) -> rule(Gamma, maps:from_list(Theta), Process, Delta);
rule(Gamma, Theta, Process, {Clocks,Type}) 
when is_list(Clocks) -> rule(Gamma, Theta, Process, {maps:from_list(Clocks),Type});


% %% @doc rule [Recv]
% %% type-checking single reception
% rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
% ->
%   ok;
% %%


%% @doc helper function to wrap any lone interact type in list (now that we are past rule [Recv])
rule(Gamma, Theta, Process, {Clocks,Type}=_Delta)
when is_tuple(Type) -> rule(Gamma, Theta, Process, {Clocks,toast:interactions(Type)});
%%


%% @doc rule [Send]
%% type-checking one send from type
rule(_Gamma, _Theta, {_Role,'<-',{Label,Payload},P}=_Process, {_Clocks,Type}=_Delta)
when is_list(Type) ->
  ok;
%%


% %% @doc rule [Branch]
% %% type-checking each reception in branch 
% %% @see rule [Recv]
% rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
% ->
%   ok;
% %%


% %% @doc rule [Timeout]
% %% type-checking branches with timeouts 
% %% @see rule [Branch]
% rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
% ->
%   ok;
% %%


% %% @doc rule [IfTrue]
% %% type-checking if-true
% rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
% ->
%   ok;
% %%


% %% @doc rule [IfFalse]
% %% type-checking if-false
% rule(_Gamma, _Theta, _Process, {_Clocks,_Type}=_Delta)
% ->
%   ok;
% %%


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
rule(Gamma, Theta, {'delay',T,P}=_Process, {Clocks,Type}=_Delta)
->
  %% increment timers and clocks y T
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta1, P, {Clocks1,Type}),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Del-t'),

  %% ask z3 if Delta is not T-reading
  {Signal, NotTReading} = ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>T}),
  ?assert(Signal=:=ok),
  ?assert(is_boolean(NotTReading)),

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
