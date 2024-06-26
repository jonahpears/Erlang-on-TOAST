-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

%% how fine grained to check decimals
-define(DECIMAL_PRECISION,8).

-define(INFINITY,99999999999).


-include_lib("stdlib/include/assert.hrl").



%% @doc wrapper function for calling the default function in the python program.
-spec z3(list()) -> any().
z3(Args) -> z3(ask_z3, Args).

%% @doc python interface for calling specific functions that use z3
-spec z3(atom(), list()) -> any().
z3(Name, [H|_T]=Args)
when is_list(Args) and is_binary(H) ->
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

%% @doc constructs python code to ask z3 if constraint holds, given clocks
ask_z3(satisfied_constraints, #{clocks:=Clocks,constraints:=Constraints}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(satisfied_constraints, #{clocks=>Clocks,delta=>Constraints}),
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  % io:format("\n\nsatisfied_constraints, ExecString:\n~s\n\tResponse: ~p.",[ExecString,Z3Response]),
  %% return result
  {ok, Z3Response};
%%


%% @doc for each Clock valuation, asks z3 to check if they are t_reading
ask_z3(t_reading, #{clocks:=Clocks,type:=Type,t:=_T})
when is_map(Clocks) and is_float(_T) ->
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
      AskZ3 = lists:foldl(fun(Constraints, {IsOk, InSat}) ->
        %% get python string to execute using z3
        ExecString = to_python_exec_string(t_reading, #{clocks=>Clocks,delta=>Constraints,t=>T}),
        %% send to python program and get response
        Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
        % io:format("\n\nt_reading, ExecString:\n~s\n\tResponse: ~p.",[ExecString,Z3Response]),

        %% check IsOk
        case IsOk of
          %% check if response was okay from z3
          ok -> 
            case is_boolean(Z3Response) of 
              %% is ok
              true -> {ok, (InSat and Z3Response)};
              %% not okay, some error
              _ -> {error, [InSat,Z3Response]}
            end;

          %% not okay, continue to collect results in list, in attempt to analyse them afterwards
          _Err -> {IsOk, [InSat,Z3Response]}

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
ask_z3(not_t_reading, #{clocks:=_Clocks,type:=_Type,t:=_T}=Map) 
when is_map(_Clocks) and is_float(_T) ->
  %% inverse t_reading
  case ask_z3(t_reading, Map) of 
    {ok, Result} -> {ok, not Result};
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
-spec to_python_exec_string(atom(), map()) -> string()|{{list(),list()},string()}|{list(),list(),list()}.

%% @doc build delta string, and prepare z3 python code to query if it is sat given the clock valuations
to_python_exec_string(satisfied_constraints, #{clocks:=Clocks,delta:=Constraints})
when is_map(Clocks) ->
  %% stringify Delta
  {{VarList,ClockList}, DeltaString} = to_python_exec_string(constraints, #{delta=>Constraints}),
  %% make sure each clock in ClockList is instantiated.
  Clocks1 = instansiate_clocks(Clocks,ClockList),
  %% build code for constants
  {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
  %% build code for clocks
  {ClockNames, ClockNamesSep, ClockDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(Clocks1),datatype=>float}),
  %% denote if multiple constants
  IntString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
  RealString = case length(maps:keys(Clocks1))>1 of true -> "Reals"; _ -> "Real" end,
  %% build exec python string (for z3)
  ExecString = io_lib:format("~s = ~s('~s')\n~s = ~s('~s')\ns = Solver()\ns.add(~s, ~s)\ns.add(~s)\nresult = s.check()",[ConstantNamesSep,IntString,ConstantNames,ClockNamesSep,RealString,ClockNames,ClockDeclarations,ConstantDeclarations,DeltaString]),
  %% return string
  ExecString;
%%

%% @doc for each clock in Clocks, call z3 to see if there exists t'<T such that delta holds 
%% this is only for simple constraints (no diagonal)
%% ! make sure to initialise the variables of clocks always, including those within the delta -- later on be more clever about this and only test for clocks present in the delta
to_python_exec_string(t_reading, #{clocks:=Clocks,delta:=Delta,t:=T}=_Args)
when is_map(Clocks) and is_float(T) ->
  %% stringify Delta
  {{VarList,ClockList}, DeltaString} = to_python_exec_string(constraints, #{delta=>Delta,t_reading=>true}),
  %% make sure each clock in ClockList is instantiated.
  Clocks1 = instansiate_clocks(Clocks,ClockList),
  %% build code for constants
  {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
  %% build code for clocks
  {ClockNames, ClockNamesSep, ClockDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(Clocks1),datatype=>float}),
  %% denote if multiple constants
  IntString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
  %% build exec python string (for z3)
  ExecString = io_lib:format("~s = ~s('~s')\nt, t_, ~s = Reals('t t_ ~s')\ns = Solver()\ns.add(t==~w, ~s, ~s)\ns.add(Exists(t_,And(0<=t_,t_<t, ~s)))\nresult = s.check()",[ConstantNamesSep,IntString,ConstantNames,ClockNamesSep,ClockNames,T,ClockDeclarations,ConstantDeclarations,DeltaString]),
  %% return string
  ExecString;
%%

%% @doc catch constraints (negation)
to_python_exec_string(constraints, #{delta:={'not',Constraints}}=Args) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  {VarList, ConstraintString} = to_python_exec_string(constraints, #{delta=>Constraints,offset=>Offset,t_reading=>TReading,negated=>not Negated}),
  % {VarList, io_lib:format("Not(~s)",[ConstraintString])};
  {VarList, ConstraintString};
%%

%% @doc catch constraints (conjunction)
to_python_exec_string(constraints, #{delta:={Constraints1,'and',Constraints2}}=Args) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  {{VarList1,ClockList1}, ConstraintString1} = to_python_exec_string(constraints, #{delta=>Constraints1,offset=>Offset++"1",t_reading=>TReading,negated=>Negated}),
  {{VarList2,ClockList2}, ConstraintString2} = to_python_exec_string(constraints, #{delta=>Constraints2,offset=>Offset++"2",t_reading=>TReading,negated=>Negated}),
  %% if t_reading, no need to wrap in And
  DeltaString = case TReading of 
      true -> io_lib:format("~s, ~s",[ConstraintString1,ConstraintString2]);
      _ -> io_lib:format("And(~s, ~s)",[ConstraintString1,ConstraintString2])
  end,
  {{VarList1++VarList2,ClockList1++ClockList2}, DeltaString};
%%

%% @doc catch constraints (simple)
to_python_exec_string(constraints, #{delta:={Clock,DBC,Constant}}=Args)
when is_atom(Clock) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  ConstantString = "n"++Offset,
  DeltaString = case TReading of 
      true -> io_lib:format("(~w+t_)~s~s",[Clock,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString]);
      _ -> io_lib:format("~w~s~s",[Clock,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString])
  end,
  {{[{ConstantString,Constant}],[Clock]},DeltaString};
%%

%% @doc catch constraints (diagonal)
to_python_exec_string(constraints, #{delta:={Clock1,'-',Clock2,DBC,Constant}}=Args)
when is_atom(Clock1) and is_atom(Clock2) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  ConstantString = "n"++Offset,
  DeltaString = case TReading of 
      true -> io_lib:format("(~w+t_)-(~w+t_)~s~s",[Clock1,Clock2,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString]);
      _ -> io_lib:format("~w-~w~s~s",[Clock1,Clock2,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString])
  end,
  {{[{ConstantString,Constant}],[Clock1,Clock2]},DeltaString};
%%

%% @doc catch constraints (true)
to_python_exec_string(constraints, #{delta:=true}=_Args) -> " True ";

%% @doc catch DBC
to_python_exec_string(constraints, #{dbc:='gtr'}=_Args) -> case maps:get(negated,_Args,false) of true -> "<="; _ -> ">" end;
to_python_exec_string(constraints, #{dbc:='geq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "<"; _ -> ">=" end;
to_python_exec_string(constraints, #{dbc:='les'}=_Args) -> case maps:get(negated,_Args,false) of true -> ">="; _ -> "<" end;
to_python_exec_string(constraints, #{dbc:='leq'}=_Args) -> case maps:get(negated,_Args,false) of true -> ">"; _ -> "<=" end;
to_python_exec_string(constraints, #{dbc:='eq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "!="; _ -> "=" end;
to_python_exec_string(constraints, #{dbc:='neq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "="; _ -> "!=" end;

%% @doc build strings for vars (constants/clocks)
to_python_exec_string(vars, #{vars:=Vars,datatype:=DataType}=_Args)
when is_list(Vars) ->
  {VarNames, VarNamesSep, VarDeclarations} = lists:foldl(fun({VarName,VarValue}, {InNames,InNamesSep,InDecls}) -> 
    VarString = case is_atom(VarName) of true -> atom_to_list(VarName)++case VarName of 'global' -> "_"; _ -> "" end; _ -> VarName end,
    %% this will appear inside the "Ints(...)" call in the python with z3
    NewNames = case length(InNames)==0 of true -> VarString; _ -> InNames++" "++VarString end,
    NewNamesSep = case length(InNamesSep)==0 of true -> VarString; _ -> InNamesSep++", "++VarString end,
    %% ensure value is of the correct form
    VarTyped = case DataType of 
      int -> to_integer(VarValue);
      float -> to_float(VarValue);
      % float -> binary_to_integer(list_to_binary(VarStr));
      _ -> io:format("\n\nWarning, ~p, unrecognised datatype specified for VarValue: ~p. Leaving as is.\n",[?FUNCTION_NAME,DataType]),VarValue
    end,
    %% this will be added to the "s.add(...)" to initialise the values of the constants
    DeclString = case length(InDecls)==0 of true -> ""; _ -> InDecls++", " end,
    NewDecls = to_string(io_lib:format("~s~s==~s",[DeclString,VarString,to_string(VarTyped)])),
    % io:format("\n\nNewNames: ~p, NewNamesSep: ~p, NewDecls: ~p. (~p)\n",[NewNames,NewNamesSep,NewDecls,DataType]),
    %% return strings as tuple
    {NewNames,NewNamesSep,NewDecls}
  end, {"", "", ""}, Vars),
  %% return
  {to_string(VarNames), to_string(VarNamesSep), to_string(VarDeclarations)};
%%

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

% %% @doc helper function to convert delta tuple into map
% rule(Gamma, Theta, Process, {Role,Clocks,Type}) -> rule(Gamma, Theta, Process, #{Role=>{Clocks,Type}});

%% @doc helper function to convert delta triple (currently unused) into tuple
rule(Gamma, Theta, Process, {_Role,Clocks,Type}) -> rule(Gamma, Theta, Process, {Clocks,Type});


%% @doc rule [Recv]
%% type-checking single reception
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P}=_Process, {Clocks,[{recv,{_Label,__Payload},Constraints,Resets,S}]=Type}=Delta)
when (is_atom(E) or is_integer(E)) and is_atom(Label) and (Label=:=_Label) and is_list(Resets) ->

  %% reset clocks
  Clocks1 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks, Resets),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta, P, {Clocks1,S}),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Recv'),

  %% if Delta is only a tuple, and not a map containing other roles, then do not check E-reading
  case is_tuple(Delta) of 
    true -> NotEReading = true;
    
    %% if map, then need to check for all other in delta
    _ -> io:format("\n\nWarning, rule [Recv] does not currently support Delta maps.\n"),
      %% ask z3 if Delta is not E-reading
      Duration = case E of infinity -> ?INFINITY; _ -> E end,
      {Signal1, NotEReading} = ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>to_float(Duration)}),
      ?assert(Signal1=:=ok),
      ?assert(is_boolean(NotEReading))
  end,

  %% ask z3 if Delta if constraints satisfied
  {Signal2, SatisfiedConstraints} = ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
  ?assert(Signal2=:=ok),
  ?assert(is_boolean(SatisfiedConstraints)),

  %% construct premise and return
  Premise = Continuation and SatisfiedConstraints and NotEReading,
  io:format("\nRule [Recv] -> ~p. (~p)\n",[Premise,{Continuation,SatisfiedConstraints,NotEReading}]),
  {Premise, Trace1};
%%

%% @doc helper function to wrap any lone branch process type in list (to reapply rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta)
when is_list(Type) -> 
  rule(Gamma, Theta, {_Role,'->',E,[{{Label,_Payload},P}]}, {Clocks,Type});
%%


%% @doc helper function to wrap any lone interact type in list (now that we are past rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, Process, {Clocks,Type}=_Delta)
when is_tuple(Type) -> 
  rule(Gamma, Theta, Process, {Clocks,toast:interactions(Type)});
%%


%% @doc rule [Send]
%% type-checking one send from type
rule(Gamma, Theta, {_Role,'<-',{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta)
when is_list(Type) ->

  %% for each interaction in type, for those that are sending, check their labels match, and add to exists -- there should only be one
  MatchingSends = lists:foldl(fun({Direction_i,{Label_i,_Payload_i},_Constraints_i,_Resets_i,_S_i}=I, Ins) -> 
    case Direction_i of 
      %% check send
      send -> 
        case Label=:=Label_i of 
          %% check labels match
          true -> Ins++[I];
          _ -> Ins %% skip if not corresponding label
        end;
      _ -> Ins %% skip if not send
    end
  end, [], Type),
  %% there should only be one
  ?assert(length(MatchingSends)==1),
  Send = lists:nth(1,MatchingSends),
  
  %% unpack
  {send, {_,_}, Constraints, Resets, S} = Send,

  %% reset clocks
  Clocks1 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks, Resets),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta, P, {Clocks1,S}),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Send'),

  %% ask z3 if Delta if constraints satisfied
  {Signal, SatisfiedConstraints} = ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
  ?assert(Signal=:=ok),
  ?assert(is_boolean(SatisfiedConstraints)),

  %% construct premise and return
  Premise = Continuation and SatisfiedConstraints,
  io:format("\nRule [Send] -> ~p. (~p)\n",[Premise,{Continuation,SatisfiedConstraints}]),
  {Premise, Trace1};
%%


%% @doc rule [Branch]
%% type-checking each reception in branch 
%% @see rule [Recv]
rule(Gamma, Theta, {Role,'->',E,Branches}=_Process, {Clocks,Type}=_Delta)
when (is_atom(E) or is_integer(E)) and is_list(Branches) and is_list(Type) ->
  %% for each reception in Type that is enabled now
  Pairings = lists:foldl(fun({Direction_j,{Label_j,_Payload_j},Constraints_j,_Resets_j,_S_j}=Type_j, InPairs) -> 
    %% check recv
    case Direction_j of 
      recv ->
      %% ask z3 if Delta if constraints satisfied
      {Signal_j, SatisfiedConstraints_j} = ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints_j}),
      ?assert(Signal_j=:=ok),
      ?assert(is_boolean(SatisfiedConstraints_j)),
      %% check if enabled
      case SatisfiedConstraints_j of 
        %% search for corresponding branch in process
        true -> 
          Pair = lists:foldl(fun({{Label_i,Payload_i}, P_i}, OutPairs) ->
            case Label_i=:=Label_j of 
              true -> OutPairs++[{Type_j,{Role,'->',E,{Label_i,Payload_i},P_i}}];
              _ -> OutPairs
            end
          end, [], Branches),
          ?assert(length(Pair)==1),
          InPairs++Pair;

        %% not enabled, skip
        _ -> 
          % io:format("\n\nnot enabled, skip: ~p,\n\tclocks: ~p.\n",[Type_j,Clocks]),
          InPairs
      end;

      %% not recv, skip
      _ -> 
        % io:format("\n\nnot recv, skip: ~p,\n\tclocks: ~p.\n",[Type_j,Clocks]),
        InPairs
    end
  end, [], Type),


  %% there must be as many pairings as there are branches
  case (length(Branches)==length(Pairings)) of 
    %% find matching branch, and evaluate
    true -> 
      Continuations = lists:foldl(fun({RecvType,RecvBranch}, InEvals) -> 
        InEvals++[rule(Gamma,Theta,RecvBranch,{Clocks,RecvType})]
      end, [], Pairings),

      %% check if all continuations passed & update trace
      {Continuation, TraceList} = lists:foldl(fun({Eval,Trace}, {InEval,InTrace}) -> 
        {(Eval and InEval),[add_to_trace(Trace,'Branch')]++InTrace} 
      end, {true,[]}, Continuations);

    %% each branch is not evaluated
    _ -> 
      io:format("\n\nWarning, rule [Branch], found no enabled types to pair with branches:\nType:\t~p,\nPrc:\t~p.\n",[Type,_Process]),
      Continuation = false, TraceList = ['Branch']
  
  end,

  %% construct premise and return
  Premise = Continuation,
  io:format("\nRule [Branch] -> ~p. (~p)\n",[Premise,{Continuation}]),
  {Premise, TraceList};
%%


%% @doc rule [Timeout]
%% type-checking branches with timeouts 
%% @see rule [Branch]
rule(Gamma, Theta, {Role,'->',E,Branches,'after',Q}=_Process, {Clocks,Type}=_Delta)
when (is_atom(E) or is_integer(E)) and is_list(Branches) and is_list(Type) ->
  
  %% check via rule [Branch]
  {Continuation1, Trace1} = rule(Gamma, Theta, {Role,'->',E,Branches}, {Clocks,Type}),

  %% check Q (let E pass over Theta and Clocks)
  case E of
    %% this would be impossible to compute
    infinity -> Continuation2 = false, Trace2 = [];

    %% incrememnt clocks and timers by E, then type-check against Q
    _ -> Theta1 = increment_timers(Theta,E), Clocks1 = increment_clocks(Clocks,E),
      {Continuation2, Trace2} = rule(Gamma, Theta1, Q, {Clocks1,Type})

  end,

  %% merge traces and update
  Trace3 = add_to_trace(Trace1++Trace2,'Timeout'),

  %% construct premise and return
  Premise = Continuation1 and Continuation2,
  io:format("\nRule [Timeout] -> ~p. (~p)\n",[Premise,{Continuation1,Continuation2}]),
  {Premise, Trace3};
%%


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
  {Continuation, Trace} = rule(Gamma, Theta1, P, Delta),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Timer'),

  %% construct premise and return
  Premise = Continuation,
  io:format("\nRule [Timer] -> ~p. (~p)\n",[Premise,{Continuation}]),
  {Premise, Trace1};
%%


%% @doc rule [Del-delta]
%% type-checking non-deterministic delays
rule(Gamma, Theta, {'delay',{t, DBC, N},P}=_Process, {Clocks,Type}=_Delta)
when is_atom(DBC) and (is_atom(N) or is_integer(N)) ->

  %% determine bounds
  {Lower, Upper} = case DBC of 
    'gtr' -> {N,?INFINITY}; 'geq' -> {N,?INFINITY};
    'les' -> {0,N}; 'leq' -> {0,N};
    _ -> io:format("\n\nWarning, DBC unsupported: ~p.\n",[DBC]), {0,1}
  end,

  %% determine how many iterations (based on how precise we measure our decimals to be)
  % Iterations = math:pow(10,?DECIMAL_PRECISION),
  DecimalPrecision = lists:flatten(lists:duplicate(?DECIMAL_PRECISION,"0")),
  PerIntegerPrecision = list_to_integer("10"++DecimalPrecision),
  % Iterations = Upper*PerIntegerPrecision,

  {_,IntegerIterationIndex} = lists:foldl(fun(_,{Index,Iterations}) -> 
    %% build current iteration index
    % IterationFloat = (PerIntegerPrecision+Index)/PerIntegerPrecision,
    IterationInteger = Index+1,
    %% next iteration
    {IterationInteger,Iterations++[IterationInteger]}
  end, {0,[]}, Upper-Lower),

  %% use worker functions to calculate in parallel
  Self = self(),
  WorkerIDs = lists:foldl(fun(StartIndex, IDs) -> 
    %% spawn worker and give it 
    {ok, ID} = spawn(fun(StartInteger) ->
      %% generate range (up to precision)
      Range = [X/PerIntegerPrecision || X <- lists:seq(StartInteger,StartInteger+PerIntegerPrecision)],
      lists:foreach(fun(T) -> 
        %% evaluate for given T in range
        Continuation = rule(Gamma,Theta,{'delay',T,P},{Clocks,Type}),
        %% send back to self
        Self ! {self(), eval, Continuation, T}
      end, Range)
    end, [StartIndex]),
    %% add to IDs
    IDs++[ID]
  end, [], IntegerIterationIndex),

  %% receive all of the workers
  Continuations = lists:foldl(fun(ID, InEvals) -> 
    receive {ID, eval, Eval} -> InEvals++[Eval] end
  end, [], WorkerIDs),

  %% check if all continuations passed & update trace
  {Continuation, TraceList} = lists:foldl(fun({Eval,Trace}, {InEval,InTrace}) -> 
    {(Eval and InEval),[add_to_trace(Trace,'Branch')]++InTrace} 
  end, {true,[]}, Continuations),

  %% construct premise and return
  Premise = Continuation,
  io:format("\nRule [Branch] -> ~p. (~p)\n",[Premise,{Continuation}]),
  {Premise, TraceList};
%%


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
  {Signal, NotTReading} = ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>to_float(T)}),
  ?assert(Signal=:=ok),
  ?assert(is_boolean(NotTReading)),

  %% construct premise and return
  Premise = Continuation and NotTReading,
  io:format("\nRule [Del-t] -> ~p. (~p)\n",[Premise,{Continuation,NotTReading}]),
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
  Premise = Continuation and FreeName,
  io:format("\nRule [Rec] -> ~p. (~p)\n",[Premise,{Continuation,FreeName}]),
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
  io:format("\nRule [Var] -> ~p. (~p)\n",[Premise,{NamesMatch,IsInGamma,MsgsValid}]),
  {Premise, ['Var']};
%%


%% @doc rule [End]
%% type-checking termination type
rule(_Gamma, _Theta, 'term'=_Process, {_Clocks,Type}=_Delta)
->
  %% type must be end
  Premise = (Type=:='end'),
  io:format("\nRule [End] -> ~p. (~p)\n",[Premise,{Premise}]),
  {Premise, ['End']};
%%

%% @doc rule [Weak]
%% type-checking termination type
rule(Gamma, Theta, Process, {_Clocks,'end'=_Type}=Delta)
->
  io:format("\n\nWarning, rule [Weak] indicates that the type as terminated, but the process has not.\n\nGamma:\t~p,\nTheta:\t~p,\nPrc:\t~p,\nDelta:~p.\n\n",[Gamma, Theta, Process, Delta]),
  io:format("\nRule [Weak] -> ~p. (~p)\n",[false,{false}]),
  {false, ['Weak']};
%%

%% @doc special case, allow recursive type to be unfolded
rule(Gamma, Theta, Process, {Clocks,{'def', _Name, S}=_Type}=_Delta)
-> rule(Gamma, Theta, Process, {Clocks, S});
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
  case length(Trace)==0 of true -> [Rule]; _ -> case is_list(hd(Trace)) of
    true -> %% split into list of traces
      lists:foldl(fun(T,In) -> [[Rule]++T]++In end, [], Trace);
    _ -> %% stayed single trace
      [Rule]++Trace
  end end.
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

%% @doc makes sure each clock in List has a value in the returned Clocks.
%% if no value is found, then they are assigned value of 'global' clock
%% if no 'global' clock is found, then warning printed and 'global' added as highest value clock, which missing clocks are subsequently given the same valuation
instansiate_clocks(Clocks,List)
when is_map(Clocks) and is_list(List) ->
  %% check if 'global' in clocks
  case is_map_key('global',Clocks) of 
    %% global present, do nothing
    true -> Max = maps:get('global',Clocks), Clocks1 = Clocks;
    %% need to add global, find maximal clock
    _ -> 
      Max = lists:max(maps:fold(fun(_Clock,Val,InVals) -> InVals++[Val] end, [], Clocks)),
      Clocks1 = maps:put('global',Max,Clocks)
  end,
  %% for each in List, set value to Max if not set already
  Clocks2 = lists:foldl(fun(Clock, InMap) ->
    case is_map_key(Clock,Clocks1) of 
      true -> InMap;
      _ -> maps:put(Clock,Max,Clocks1)
    end
  end, Clocks1, List),
  %% return
  Clocks2;
%%

%% @doc helper function to convert Clocks to map and back to list.
instansiate_clocks(Clocks,List)
when is_list(Clocks) and is_list(List) ->
  Clocks1 = instansiate_clocks(maps:from_list(Clocks),List),
  %% retrurn as list
  maps:to_list(Clocks1).
%%


%% @doc rounds to DECIMAL_PRECISION decimal places
precision_rounding(Float) 
when is_float(Float) -> 
  Offset = math:pow(10.0,?DECIMAL_PRECISION),
  round(Float*Offset)/Offset.
%%



%% @doc type conversion functions
to_float(N) when is_integer(N) -> list_to_float(integer_to_list(N)++".0");
% to_float(N) when is_list(N) -> 
to_float(N) when is_binary(N) -> binary_to_float(N);
to_float(N) when is_float(N) -> N.

to_integer(N) when is_float(N) -> round(N);
% to_integer(N) when is_list(N) -> 
to_integer(N) when is_binary(N) -> binary_to_integer(N);
to_integer(N) when is_integer(N) -> N.

to_binary(N) when is_float(N) -> integer_to_binary(N);
to_binary(N) when is_list(N) -> list_to_binary(N);
to_binary(N) when is_integer(N) -> float_to_binary(N);
to_binary(N) when is_binary(N) -> N.

to_string(N) when is_float(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_integer(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_binary(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_list(N) -> lists:flatten(N).
