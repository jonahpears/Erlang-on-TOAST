-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

%% how fine grained to check decimals
-define(DECIMAL_PRECISION,1).

-define(INFINITY,99).

-define(DEFAULT_SHOW_TRACE,true).


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
ask_z3(satisfied_constraints=_Kind, #{clocks:=Clocks,constraints:=Constraints}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(satisfied_constraints, #{clocks=>Clocks,delta=>Constraints}),
  io:format("\n\n~p, ExecString:\n~s\n.",[_Kind,ExecString]),
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  io:format("\n\n~p, ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]),
  %% return result
  {ok, Z3Response};
%%

%% @doc constructs python code to ask z3 if constraint holds, given clocks and INFINITY upperbound
ask_z3(feasible_constraints=_Kind, #{clocks:=Clocks,constraints:=Constraints,e:='infinity'=E}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(feasible_constraints, #{clocks=>Clocks,delta=>Constraints,e=>E}),
  io:format("\n\n~p, (infinity) ExecString:\n~s\n.",[_Kind,ExecString]),
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  io:format("\n\n~p, (infinity) ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]),
  %% return result
  {ok, Z3Response};
%%

%% @doc constructs python code to ask z3 if constraint holds, given clocks and non-infinite upperbound
ask_z3(feasible_constraints=_Kind, #{clocks:=Clocks,constraints:=Constraints,e:=E}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(feasible_constraints, #{clocks=>Clocks,delta=>Constraints,e=>E}),
  io:format("\n\n~p, (non-infinity) ExecString:\n~s\n.",[_Kind,ExecString]),
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  io:format("\n\n~p, (non-infinity) ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]),
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
  io:format("\n\n(~p) Warning, ~p, unrecognised question: ~p,\n\twith args: ~p.\n",[?LINE,?FUNCTION_NAME,Unknown,Args]),
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


%% @doc build delta string for z3 python, for checking constraints are satisfied for duration of infinity=E
to_python_exec_string(feasible_constraints, #{clocks:=Clocks,delta:=Constraints,e:=E}) ->
  %% cosntruct E
  case E of 'infinity' ->
      StringE = "e = fpInfinity(FPSort(8,24),False)",
      StringT = "t = FP('t',FPSort(8,24))",
      StringTExpr = "texpr = fpLEQ(t,e)",
      IsInfinity = true;
    _ -> 
      {EVal, StringDBC} = case E of {'leq',_EVal} -> {_EVal," <= "}; {'les',_EVal} -> {_EVal," < "}; 0 -> {0," == "}; _ -> io:format("\n\n(~p) Warning, unexpected E: ~p.\n",[?LINE,E]), {-1,'les'} end,
      StringE = io_lib:format("e = Real('e')\ns.add(e==~w)",[EVal]),
      StringT = "t = Real('t')",
      StringTExpr = "texpr = t"++StringDBC++"e",
      IsInfinity = false
  end,
  %% solver
  Solver = "s = Solver()",
  %% extract vars and string
  {{VarList,ClockList}, DeltaString} = to_python_exec_string(constraints, #{delta=>Constraints,add_t=>true,as_z3_fp=>IsInfinity}),
  %% make sure each clock in ClockList is instantiated.
  Clocks1 = instansiate_clocks(Clocks,ClockList),

  %% if infinity, no constants
  IntString = case IsInfinity of 
    true -> "";
    _ ->
    %% otherwise, constants
    %% build code for constants
    {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
    
    % denote if multiple constants
    IntFunString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
    io_lib:format("\n~s = ~s('~s')\ns.add(~s)",[ConstantNamesSep,IntFunString,ConstantNames,ConstantDeclarations])
  end,
  
  %% build code for clocks
  {ClockNames, ClockNamesSep, ClockDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(Clocks1),datatype=>float}),

  RealFunString = case length(maps:to_list(Clocks1))>1 of true -> "Reals"; _ -> "Real" end,
  RealString = io_lib:format("~s = ~s('~s')",[ClockNamesSep,RealFunString,ClockNames]),

  VarSetupString = io_lib:format("~s\n~s\n~s\n~s",[StringE,StringT,RealString,StringTExpr]),

  % io:format("\ne: ~s, t: ~s,\ntexpr: ~s, isinfinity: ~s,\ndelta: ~s.\n\n",[StringE,StringT,StringTExpr,IsInfinity,DeltaString]),
  % timer:sleep(1000),

  LhsString = "Implies(delta,texpr)",
  RhsString = "Implies(texpr,delta)",

  ExecString = io_lib:format("~s\n~s~s\ndelta = ~s\nlhs = ~s\nrhs = ~s\ns.add(~s)\ns.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))\nresult = s.check()",[Solver,VarSetupString,IntString,DeltaString,LhsString,RhsString,ClockDeclarations]),

  % io:format("\nexec string:\n~s\n.\n",[ExecString]),

  % timer:sleep(5000),

  ExecString;
%%


%% @doc build delta string, and prepare z3 python code to query if it is sat given the clock valuations
to_python_exec_string(old_feasible_constraints, #{clocks:=Clocks,delta:=Constraints,e:=E})
when is_map(Clocks) ->
  {EVarList,EString} = to_python_exec_string(constraints, #{delta=>E,is_t=>true}),
  io:format("\n1.\n"),
  %% build code for e_timeout
  {ENames, ENamesSep, EDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(EVarList),datatype=>float}),
  io:format("\n2.\n"),
  %% stringify Delta
  {{VarList,ClockList}, DeltaString} = to_python_exec_string(constraints, #{delta=>Constraints,add_t=>true}),
  io:format("\n3.\n"),
  %% make sure each clock in ClockList is instantiated.
  Clocks1 = instansiate_clocks(Clocks,ClockList),
  io:format("\n4.\n"),
  %% build code for constants
  {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
  io:format("\n5.\n"),
  %% build code for clocks
  io:format("\n6.\n"),
  {ClockNames, ClockNamesSep, ClockDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(Clocks1),datatype=>float}),
  io:format("\n7.\n"),
  %% denote if multiple constants
  IntString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
  %% build exec python string (for z3)
  ExecString = io_lib:format("~s = ~s('~s')\nt, ~s ~s = Reals('t ~s ~s')\ns = Solver()\ns.add(~s, ~s, ~s)\ns.add(ForAll(t, t>=0, ~s, ~s))\nresult = s.check()",[ConstantNamesSep,IntString,ConstantNames,ENamesSep,ClockNamesSep,ENames,ClockNames,EDeclarations,ClockDeclarations,ConstantDeclarations,EString,DeltaString]),
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
  AddT = maps:get(add_t,Args,false),
  IsT = maps:get(is_t,Args,false),
  IsFP = maps:get(as_z3_fp,Args,false),
  {VarList, ConstraintString} = to_python_exec_string(constraints, #{delta=>Constraints,offset=>Offset,t_reading=>TReading,negated=>not Negated,add_t=>AddT,is_t=>IsT,as_z3_fp=>IsFP}),
  % {VarList, io_lib:format("Not(~s)",[ConstraintString])};
  {VarList, ConstraintString};
%%

%% @doc catch constraints (conjunction) %! <- z3 fp
to_python_exec_string(constraints, #{delta:={Constraints1,'and',Constraints2},as_z3_fp:=true=IsFP}=Args) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  AddT = maps:get(add_t,Args,false),
  %% lhs
  {{VarList1,ClockList1}, ConstraintString1} = to_python_exec_string(constraints, #{delta=>Constraints1,offset=>Offset++"1",t_reading=>TReading,negated=>Negated,add_t=>AddT,as_z3_fp=>IsFP}),
  %% rhs
  {{VarList2,ClockList2}, ConstraintString2} = to_python_exec_string(constraints, #{delta=>Constraints2,offset=>Offset++"2",t_reading=>TReading,negated=>Negated,add_t=>AddT,as_z3_fp=>IsFP}),
  %% if t_reading, no need to wrap in And
  DeltaString = case TReading of 
      true -> io_lib:format("~s, ~s",[ConstraintString1,ConstraintString2]);
      _ -> io_lib:format("And(~s, ~s)",[ConstraintString1,ConstraintString2])
  end,
  {{VarList1++VarList2,ClockList1++ClockList2}, DeltaString};
%%

%% @doc catch constraints  %! <- z3 fp
to_python_exec_string(constraints, #{delta:={Clock,DBC,Constant},as_z3_fp:=true=IsFP}=Args)
when is_atom(Clock) and is_atom(DBC) and is_integer(Constant) -> 
  % Offset = maps:get(offset,Args,""),
  Negated = maps:get(negated,Args,false),
  % ConstantString = "n"++Offset,
  %% get the z3 function for fp that corresponds to the dbc
  FpFunc = to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated,as_z3_fp=>IsFP}),
  % ConstantString = case Constant of 'infinity' -> "timeout_e"; _ -> "n"++Offset end,
  AddT = maps:get(add_t,Args,false),
  ClockString = case AddT of true -> io_lib:format("fpAdd(RNE(),~s,t)",[to_python_exec_string(clock_fp,#{clock=>Clock})]); _ -> to_python_exec_string(clock_fp,#{clock=>Clock}) end,
  DeltaString = io_lib:format("~s(~s,~w)",[FpFunc,ClockString,to_float(Constant)]),
  {{[],[Clock]},DeltaString};
%%

%% @doc catch constraints (diagonal) %! <- z3 fp
to_python_exec_string(constraints, #{delta:={Clock1,'-',Clock2,DBC,Constant},as_z3_fp:=true=IsFP}=Args)
when is_atom(Clock1) and is_atom(Clock2) and is_atom(DBC) and is_integer(Constant) -> 
  % Offset = maps:get(offset,Args,""),
  Negated = maps:get(negated,Args,false),
  % ConstantString = "n"++Offset,
  AddT = maps:get(add_t,Args,false),
  %% get the z3 function for fp that corresponds to the dbc
  FpFunc = to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated,as_z3_fp=>IsFP}),
  %% construct the clock string (because fp, will need to add wrapper functions)
  Clock1String = case AddT of true -> io_lib:format("fpAdd(RNE(),~s,t)",[to_python_exec_string(clock_fp,#{clock=>Clock1})]); _ -> to_python_exec_string(clock_fp,#{clock=>Clock1}) end,
  Clock2String = case AddT of true -> io_lib:format("fpAdd(RNE(),~s,t)",[to_python_exec_string(clock_fp,#{clock=>Clock2})]); _ -> to_python_exec_string(clock_fp,#{clock=>Clock2}) end,
  %% build string
  DeltaString = io_lib:format("~s(fpSub(~s,~s),~w)",[FpFunc,Clock1String,Clock2String,to_float(Constant)]),
  {{[],[Clock1,Clock2]},DeltaString};
%%

to_python_exec_string(clock_fp, #{clock:=Clock}) -> io_lib:format("fpRealToFP(RNE(),~w,Float32())",[Clock]);

%% @doc catch constraints (conjunction)
to_python_exec_string(constraints, #{delta:={Constraints1,'and',Constraints2}}=Args) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  AddT = maps:get(add_t,Args,false),
  IsT = maps:get(is_t,Args,false),
  {{VarList1,ClockList1}, ConstraintString1} = to_python_exec_string(constraints, #{delta=>Constraints1,offset=>Offset++"1",t_reading=>TReading,negated=>Negated,add_t=>AddT,is_t=>IsT}),
  {{VarList2,ClockList2}, ConstraintString2} = to_python_exec_string(constraints, #{delta=>Constraints2,offset=>Offset++"2",t_reading=>TReading,negated=>Negated,add_t=>AddT,is_t=>IsT}),
  %% if t_reading, no need to wrap in And
  DeltaString = case TReading of 
      true -> io_lib:format("~s, ~s",[ConstraintString1,ConstraintString2]);
      _ -> io_lib:format("And(~s, ~s)",[ConstraintString1,ConstraintString2])
  end,
  {{VarList1++VarList2,ClockList1++ClockList2}, DeltaString};
%%

%% @doc catch constraints 
to_python_exec_string(constraints, #{delta:={Clock,DBC,Constant}}=Args)
when is_atom(Clock) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  IsT = maps:get(is_t,Args,false),
  ConstantString = case IsT of true -> "timeout_e"; _ -> "n"++Offset end,
  % ConstantString = case Constant of 'infinity' -> "timeout_e"; _ -> "n"++Offset end,
  DeltaString = case TReading of 
      true -> io_lib:format("(~w+t_)~s~s",[Clock,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString]);
      _ -> 
        AddT = maps:get(add_t,Args,false),
        ClockString = io_lib:format("~w",[Clock]) ++ case AddT of true -> "+t"; _ -> "" end,
        io_lib:format("~s~s~s",[ClockString,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString])
  end,
  %% if IsT then no clock, leave as is
  case IsT of true -> {[{ConstantString,Constant}],DeltaString};
   _ -> {{[{ConstantString,Constant}],[Clock]},DeltaString} end;
%%

%% @doc catch constraints (diagonal)
to_python_exec_string(constraints, #{delta:={Clock1,'-',Clock2,DBC,Constant}}=Args)
when is_atom(Clock1) and is_atom(Clock2) and is_atom(DBC) and is_integer(Constant) -> 
  Offset = maps:get(offset,Args,""),
  TReading = maps:get(t_reading,Args,false),
  Negated = maps:get(negated,Args,false),
  ConstantString = case maps:get(is_t,Args,false) of true -> "timeout_e"; _ -> "n"++Offset end,
  % ConstantString = case Constant of 'infinity' -> "timeout_e"; _ -> "n"++Offset end,
  DeltaString = case TReading of 
      true -> io_lib:format("(~s+t_)-(~s+t_)~s~s",[Clock1,Clock2,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString]);
      _ -> 
        AddT = maps:get(add_t,Args,false),
        Clock1String = io_lib:format("~w",[Clock1]) ++ case AddT of true -> "+t"; _ -> "" end,
        Clock2String = io_lib:format("~w",[Clock2]) ++ case AddT of true -> "+t"; _ -> "" end,
        io_lib:format("~s-~s~s~s",[Clock1String,Clock2String,to_python_exec_string(constraints, #{dbc=>DBC,negated=>Negated}),ConstantString])
  end,
  {{[{ConstantString,Constant}],[Clock1,Clock2]},DeltaString};
%%

%% @doc catch constraints (true)
to_python_exec_string(constraints, #{delta:=true}=_Args) -> " True ";

%% @doc catch DBC %! <- as z3 fp
to_python_exec_string(constraints, #{dbc:='gtr',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpLEQ"; _ -> "fpGT" end;
to_python_exec_string(constraints, #{dbc:='geq',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpLT"; _ -> "fpGEQ" end;
to_python_exec_string(constraints, #{dbc:='les',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpGEQ"; _ -> "fpLT" end;
to_python_exec_string(constraints, #{dbc:='leq',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpGT"; _ -> "fpLEQ" end;
to_python_exec_string(constraints, #{dbc:='eq',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpNEQ"; _ -> "fpEQ" end;
to_python_exec_string(constraints, #{dbc:='neq',as_z3_fp:=true}=_Args) -> case maps:get(negated,_Args,false) of true -> "fpEQ"; _ -> "fpNEQ" end;

%% @doc catch DBC 
to_python_exec_string(constraints, #{dbc:='gtr'}=_Args) -> case maps:get(negated,_Args,false) of true -> "<="; _ -> ">" end;
to_python_exec_string(constraints, #{dbc:='geq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "<"; _ -> ">=" end;
to_python_exec_string(constraints, #{dbc:='les'}=_Args) -> case maps:get(negated,_Args,false) of true -> ">="; _ -> "<" end;
to_python_exec_string(constraints, #{dbc:='leq'}=_Args) -> case maps:get(negated,_Args,false) of true -> ">"; _ -> "<=" end;
to_python_exec_string(constraints, #{dbc:='eq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "!="; _ -> "==" end;
to_python_exec_string(constraints, #{dbc:='neq'}=_Args) -> case maps:get(negated,_Args,false) of true -> "=="; _ -> "!=" end;

%% @doc build strings for vars (constants/clocks)
to_python_exec_string(vars, #{vars:=Vars,datatype:=DataType}=_Args)
when is_list(Vars) ->
  {VarNames, VarNamesSep, VarDeclarations} = lists:foldl(fun({VarName,VarValue}, {InNames,InNamesSep,InDecls}) -> 
    %% appropriatly stringify var name
    VarString = case is_atom(VarName) of true -> atom_to_list(VarName)++case VarName of 'global' -> "_"; _ -> "" end; _ -> VarName end,
    %% add t if necessary (only used in decls of clocks)
    % VarString1 = case maps:get(add_t,_Args,false) of true -> VarString++"+t"; _ -> VarString end,
    %% this will appear inside the "Ints(...)" call in the python with z3
    NewNames = case length(InNames)==0 of true -> VarString; _ -> InNames++" "++VarString end,
    NewNamesSep = case length(InNamesSep)==0 of true -> VarString; _ -> InNamesSep++", "++VarString end,
    %% ensure value is of the correct form
    VarTyped = case DataType of 
      int -> to_integer(VarValue);
      float -> to_float(VarValue);
      % float -> binary_to_integer(list_to_binary(VarStr));
      _ -> io:format("\n\n(~p) Warning, ~p, unrecognised datatype specified for VarValue: ~p. Leaving as is.\n",[?LINE,?FUNCTION_NAME,DataType]),VarValue
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
  io:format("\n\n(~p) Warning, ~p, unrecognised mode: ~p,\n\twith args: ~p.\n",[?LINE,?FUNCTION_NAME,Unknown,Args]),
  "".
%%


%% @doc helper function to rewrap via eval(map)
-spec eval(map(), map(), toast:process(), map()) -> {boolean(), [[atom()]]} | {boolean(), [atom()]}.
eval(Gamma,Theta,Process,Delta) -> eval(#{gamma=>Gamma,theta=>Theta,process=>Process,delta=>Delta}).

%% @doc for starting an type-checking evaluation of a process against a delta
-spec eval(map()) -> {boolean(),list()}.
eval(Map) ->
  %% extract necessities
  Gamma = maps:get(gamma,Map,#{}),
  Theta = maps:get(theta,Map,#{}),
  _Process = maps:get(process,Map,undefined),
  _Delta = maps:get(delta,Map,undefined),

  %% warn if no process or delta provided
  Process = case _Process of undefined -> io:format("\n\n(~p) Warning, no process found. Using 'term'.",[?LINE]), 'term'; _ -> _Process end,
  Delta = case _Delta of undefined -> io:format("\n\n(~p) Warning, no delta found. Using {#{},'end'}.",[?LINE]), {#{},'end'}; _ -> _Delta end,

  %% setup ets for printouts
  ets:new(toast_checker_ets,[set,named_table,protected]),
  ets:insert(toast_checker_ets, {checker_id, self()}),

  %% check for options on printouts
  ShowTrace = maps:get(show_trace,Map,?DEFAULT_SHOW_TRACE),
  ets:insert(toast_checker_ets, {show_trace_init, ShowTrace}),
  ets:insert(toast_checker_ets, {show_trace, ShowTrace}),

  %% begin eval
  io:format("\nBeginning type-checking eval of:\nGamma: ~p,\nTheta: ~p,\nProcess: ~p,\nDelta: ~p.\n",[Gamma,Theta,Process,Delta]),

  _Eval = rule(Gamma,Theta,Process,Delta),
  {Pass,_Trace} = _Eval,
  Trace = lists:uniq(_Trace),
  % repackage
  Eval = {Pass,Trace},

  io:format("\nFinished type-checking, eval: ~p.", [Eval]),
  %% delete table
  ets:delete(toast_checker_ets),
  %% return
  Eval.
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
when is_atom(Label) and (Label=:=_Label) and is_list(Resets) ->

  io:format("\n\n/ / / /[Recv]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),

  %% reset clocks
  Clocks1 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks, Resets),

  %% obtain DBC from E
  % {Ez3,DBC,N} = case E of {leq,_N} -> {{t,leq,_N},leq,_N}; {les,_N} -> {{t,les,_N},les, _N}; 'infinity' -> {{t,les,'infinity'},les, ?INFINITY}; _ -> io:format("\n\n(~p) Warning, unrecognised E: ~p.\n",[E]), {{t,leq,0}, leq, 0} end,
  {DBC,N} = case E of {leq,_N} -> {leq,_N}; {les,_N} -> {les, _N}; 'infinity' -> {les, ?INFINITY}; 0 -> {leq,0}; _ -> io:format("\n\n(~p) Warning, unrecognised E: ~p.\n",[?LINE,E]), {leq, 0} end,

  MinimalPrecision = minimal_precision(),

  %% determine bounds
  %% counter is used to keep track of how much of the interval between integers remains to be evaluated
  %% NOTE: all lowerbounds are exclusive, since we explore the minimal case before the workers (with trace enabled)
  BoundMap = case DBC of 
    'les' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>true,
                lower=>0+MinimalPrecision,
                upper=>N-MinimalPrecision,
                is_first=>true,
                is_last=>(N==1),
                decrement=>MinimalPrecision };
    'leq' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>false,
                lower=>0+MinimalPrecision,
                upper=>N,
                is_first=>true,
                is_last=>(N==1),
                decrement=>MinimalPrecision };
    _ -> io:format("\n\n(~p) Warning, DBC unsupported: ~p.\n",[?LINE, DBC]), {-1,#{}}
  end,

  %% show initial trace now, with minimal value (which is always 0)
  %% continue type-checking (yields premise)
  {InitialContinuation, InitialTrace} = rule(Gamma, Theta, P, {Clocks1,S}),

  io:format("\nBoundMap: ~p.\n",[BoundMap]),

  %% suppress messages of eval
  ShowTrace = show_trace(),
  ets:insert(toast_checker_ets,{show_trace,false}),

  %% only use workers if not 0
  case E of
    0 -> {Continuation,OutTraces} = {true,[]};

    _ ->

    %% use worker functions to calculate in parallel
    % Self = self(),
    WorkerIDs = start_evaluation_workers(premise_recv, {Gamma,Theta,P,{#{clocks=>Clocks,resets=>Resets},S}}, BoundMap),
    % io:format("\n\n[Recv], Started (~p) workers: ~w.\n",[length(WorkerIDs),WorkerIDs]),
    
    %% receive all of the workers
    {Continuation,TraceList} = lists:foldl(fun(ID, {InEvals,InTrace}) -> 
      receive {ID, {eval, {Eval,Traces}}, {range, _Counter, _T}} -> 
        % io:format("Received from (~p, range: ~p, ~p) -- ~p.\n",[ID, _Counter,_T,Eval]),
        %% check if all continuations passed & update trace
        NewEvals = (InEvals and Eval),
        NewTrace = Traces++InTrace,
        %% return
        {NewEvals,NewTrace}
      end
    end, {true,[InitialTrace]}, WorkerIDs),
    % io:format("\nReceived from all (~p) workers.\n",[length(WorkerIDs)]),

    %% undo suprpesion of eval messages
    ets:insert(toast_checker_ets,{show_trace,ShowTrace}),

    %% add to traces
    OutTraces = add_to_trace(TraceList,'Recv'),

    %% undo suprpesion of eval messages
    ets:insert(toast_checker_ets,{show_trace,ShowTrace})
  end,

  %% if Delta is only a tuple, and not a map containing other roles, then do not check E-reading
  case is_tuple(Delta) of 
    true -> NotEReading = true;
    
    %% if map, then need to check for all other in delta
    _ -> io:format("\n\n(~p) Warning, rule [Recv] does not currently support Delta maps.\n",[?LINE]),
      %% ask z3 if Delta is not E-reading
      Duration = case E of infinity -> ?INFINITY; _ -> E end,
      {Signal1, NotEReading} = ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>to_float(Duration)}),
      ?assert(Signal1=:=ok),
      ?assert(is_boolean(NotEReading))
  end,

  %% TODO from here, and go to ask_z3/2 and to_python_exec_string (for constraints and actual code.)
  %% TODO see z3 python example file
  {Signal2, FeasibleConstraints} = ask_z3(feasible_constraints,#{clocks=>Clocks,constraints=>Constraints,e=>E}),

  % %% ask z3 if Delta if constraints feasible (satisfied bimplies E)
  % {Signal2, FeasibleConstraints} = ask_z3(feasible_constraints,#{clocks=>Clocks,constraints=>Constraints,e=>Ez3}),
  ?assert(Signal2=:=ok),
  ?assert(is_boolean(FeasibleConstraints)),

  % FeasibleConstraints = true, %% TODO temporary

  %% construct premise and return
  Premise = InitialContinuation and Continuation and FeasibleConstraints and NotEReading,
  show_trace('Recv',Premise,{InitialContinuation,Continuation,FeasibleConstraints,NotEReading}),
  {Premise, lists:uniq(OutTraces)};
%%

%% @doc helper function to wrap any lone branch process type in list (to reapply rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta)
when is_list(Type) -> rule(Gamma, Theta, {_Role,'->',E,[{{Label,_Payload},P}]}, {Clocks,Type});


%% @doc helper function to wrap any lone interact type in list (now that we are past rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, Process, {Clocks,Type}=_Delta)
when is_tuple(Type) -> rule(Gamma, Theta, Process, {Clocks,toast:interactions(Type)});



%% @doc rule [Send]
%% type-checking one send from type
rule(Gamma, Theta, {_Role,'<-',{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta)
when is_list(Type) ->

io:format("\n\n/ / / /[Send]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),


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
  show_trace('Send',Premise,{Continuation,SatisfiedConstraints}),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Branch]
%% type-checking each reception in branch 
%% @see rule [Recv]
rule(Gamma, Theta, {Role,'->',E,Branches}=_Process, {Clocks,Type}=_Delta)
when is_list(Branches) and is_list(Type) ->

  io:format("\n\n/ / / /[Branch]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  

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
          % io:format("\n\npair: ~p,\nBranches: ~p,\nType_j: ~p.\n",[Pair,Branches,Type_j]),
          % ?assert(length(Pair)==1),
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
        {(Eval and InEval),add_to_trace(Trace,'Branch')++InTrace} 
      end, {true,[]}, Continuations);

    %% each branch is not evaluated
    _ -> 
      io:format("\n\n(~p) Warning, rule [Branch], found no enabled types to pair with branches:\nType:\t~p,\nPrc:\t~p.\n",[?LINE,Type,_Process]),
      Continuation = false, TraceList = ['Branch']
  
  end,

  %% construct premise and return
  Premise = Continuation,
  show_trace('Branch',Premise,{Continuation}),
  {Premise, lists:uniq(TraceList)};
%%


%% @doc rule [Timeout]
%% type-checking branches with timeouts 
%% @see rule [Branch]
rule(Gamma, Theta, {Role,'->',E,Branches,'after',Q}=_Process, {Clocks,Type}=_Delta)
when is_list(Branches) and is_list(Type) ->
  
  io:format("\n\n/ / / /[Timeout-P]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  
  %% check via rule [Branch]
  {Continuation1, Trace1} = rule(Gamma, Theta, {Role,'->',E,Branches}, {Clocks,Type}),

  io:format("\n\n/ / / /[Timeout-Q]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),

  %% check Q (let E pass over Theta and Clocks)
  case E of
    %% this would be impossible to compute
    infinity -> Continuation2 = false, Trace2 = [];

    %% easy
    0 -> {Continuation2, Trace2} = rule(Gamma, Theta, Q, {Clocks,Type});

    %% incrememnt clocks and timers by E, then type-check against Q
    {Less,_ValE} -> 
      ValE = case Less of 'leq' -> _ValE; 'les' -> _ValE-minimal_precision(); _ -> io:format("\n\n(~p) Warning, the DBC in E is not recognised: ~p.\n",[?LINE,E]),_ValE end,
      Theta1 = increment_timers(Theta,ValE), 
      Clocks1 = increment_clocks(Clocks,ValE),
      {Continuation2, Trace2} = rule(Gamma, Theta1, Q, {Clocks1,Type})

  end,

  %% merge traces and update
  Trace3 = add_to_trace(Trace1++Trace2,'Timeout'),

  %% construct premise and return
  Premise = Continuation1 and Continuation2,
  show_trace('Timeout',Premise,{Continuation1,Continuation2}),
  {Premise, lists:uniq(Trace3)};
%%


%% @doc rule [IfTrue] or [IfFalse]
%% type-checking if-true/if-false
rule(Gamma, Theta, {'if',{Timer, DBC, Value}, 'then', P, 'else', Q}=_Process, {_Clocks,_Type}=Delta)
->

  %% determine if [IfTrue] or [IfFalse]
  HasTimer = is_map_key(Timer,Theta),

  Constraint = case HasTimer of 
    true -> 
      ActualValue = maps:get(Timer,Theta),
      ?assert(is_number(ActualValue)),
      case DBC of 
        'leq' -> (ActualValue =< Value); 
        'les' -> (ActualValue < Value); 
        'geq' -> (ActualValue >= Value); 
        'gtr' -> (ActualValue >= Value); 
        'eq' -> (ActualValue == Value); 
        'neq' -> not (ActualValue == Value); 
        _ -> io:format("\n\nWarning, unrecognised DBC: ~p. Using eq.\n",[DBC]), (ActualValue == Value)
      end;
    _ -> false
  end,

  %% if constraint true, then true
  case Constraint of 
    true ->
      io:format("\n\n/ / / /[IfTrue]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),

      %% continue with P
      {Continuation, Trace} = rule(Gamma, Theta, P, Delta),

      %% set rule name
      RuleName = 'IfTrue';

    _ -> 
      io:format("\n\n/ / / /[IfTrue]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),

      %% continue with P
      {Continuation, Trace} = rule(Gamma, Theta, Q, Delta),

      %% set rule name
      RuleName = 'IfFalse'
  end,


  %% merge traces and update
  Trace1 = add_to_trace(Trace,RuleName),

  %% construct premise and return
  Premise = Continuation and Constraint,
  show_trace(RuleName,Premise,{Continuation,Constraint}),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Timer]
%% type-checking setting timers
rule(Gamma, Theta, {'set', Timer, P}=_Process, {_Clocks,_Type}=Delta)
->
  io:format("\n\n/ / / /[Timer]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),

  %% set timer to 0 (regardless of if it existed before or not)
  Theta1 = maps:put(Timer, 0, Theta),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta1, P, Delta),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Timer'),

  %% construct premise and return
  Premise = Continuation,
  show_trace('Timer',Premise,{Continuation}),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Del-delta]
%% type-checking non-deterministic delays
rule(Gamma, Theta, {'delay',{t, DBC, N},P}=_Process, {_Clocks,_Type}=Delta)
when is_atom(DBC) and (is_atom(N) or is_integer(N)) ->
  
  io:format("\n\n/ / / /[Del-Delta]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),


  MinimalPrecision = minimal_precision(),

  %% determine bounds
  %% counter is used to keep track of how much of the interval between integers remains to be evaluated
  %% NOTE: all lowerbounds are exclusive, since we explore the minimal case before the workers (with trace enabled)
  {Init,BoundMap} = case DBC of 
    'gtr' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>false,
                lower=>N+MinimalPrecision,
                upper=>?INFINITY,
                is_first=>true,
                is_last=>false,
                decrement=>MinimalPrecision },
      {N,_Map};
    'geq' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>false,
                lower=>N+MinimalPrecision,
                upper=>?INFINITY,
                is_first=>true,
                is_last=>false,
                decrement=>MinimalPrecision },
      {N,_Map};
    'les' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>true,
                lower=>0+MinimalPrecision,
                upper=>N-MinimalPrecision,
                is_first=>true,
                is_last=>(N==1),
                decrement=>MinimalPrecision },
      {0,_Map};
    'leq' -> 
      _Map = #{ is_lower_exclusive=>true,
                is_upper_exclusive=>false,
                lower=>0+MinimalPrecision,
                upper=>N,
                is_first=>true,
                is_last=>(N==1),
                decrement=>MinimalPrecision },
      {0,_Map};
    _ -> io:format("\n\n(~p) Warning, DBC unsupported: ~p.\n",[?LINE,DBC]), {-1,#{}}
  end,

  %% show initial trace now, with minimal value 
  {InitialContinuation,InitialTrace} = rule(Gamma, Theta, {'delay',Init, P}, Delta),

  io:format("\nBoundMap: ~p.\n",[BoundMap]),

  %% suppress messages of eval
  ShowTrace = show_trace(),
  ets:insert(toast_checker_ets,{show_trace,false}),

  %% use worker functions to calculate in parallel
  % Self = self(),
  WorkerIDs = start_evaluation_workers(premise_del_delta, {Gamma,Theta,P,Delta}, BoundMap),
  % io:format("\n\n[Del-delta], Started (~p) workers: ~w.\n",[length(WorkerIDs),WorkerIDs]),
  
  %% receive all of the workers
  {Continuation,TraceList} = lists:foldl(fun(ID, {InEvals,InTrace}) -> 
    receive {ID, {eval, {Eval,Traces}}, {range, _Counter, _T}} -> 
      % io:format("Received from (~p, range: ~p, ~p) -- ~p.\n",[ID, _Counter,_T,Eval]),
      %% check if all continuations passed & update trace
      NewEvals = (InEvals and Eval),
      % NewTrace = add_to_trace(Traces,'Del-delta')++InTrace,
      NewTrace = Traces++InTrace,
      %% return
      {NewEvals,NewTrace}
    end
  end, {true,[InitialTrace]}, WorkerIDs),
  % io:format("\nReceived from all (~p) workers.\n",[length(WorkerIDs)]),

  %% add to traces
  OutTraces = add_to_trace(TraceList,'Del-delta'),

  %% undo suprpesion of eval messages
  ets:insert(toast_checker_ets,{show_trace,ShowTrace}),

  %% construct premise and return
  Premise = InitialContinuation and Continuation,
  show_trace('Del-delta',Premise,{InitialContinuation,Continuation}),
  {Premise, lists:uniq(OutTraces)};
%%


%% @doc rule [Del-t]
%% type-checking determined delays
rule(Gamma, Theta, {'delay',T,P}=_Process, {Clocks,Type}=_Delta)
->
  io:format("\n\n/ / / /[Del-t]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),

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
  show_trace('Del-t',Premise,{Continuation,NotTReading}),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Rec]
%% type-checking recursive definitions
rule(Gamma, Theta, {'def', P, 'as', {Var, Msgs}}=_Process, {_Clocks,_Type}=Delta)
->
  io:format("\n\n/ / / /[Rec]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),

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
  show_trace('Rec',Premise,{Continuation,FreeName}),
  {Premise, Trace1};
%%


%% @doc rule [Var]
%% type-checking recursive calls
rule(Gamma, _Theta, {'call', {Var, Msgs}}=_Process, {_Clocks,{'call', Name}=_Type}=_Delta)
->
  io:format("\n\n/ / / /[Var]/ / / /\n"),

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
  show_trace('Var',Premise,{NamesMatch,IsInGamma,MsgsValid}),
  {Premise, ['Var']};
%%


%% @doc rule [End]
%% type-checking termination type
rule(_Gamma, _Theta, 'term'=_Process, {_Clocks,Type}=_Delta)
->
  % io:format("\n\n/ / / /[End]/ / / /\n"),

  %% type must be end
  Premise = (Type=:='end'),
  show_trace('End',Premise,{Premise}),
  {Premise, ['End']};
%%

%% @doc rule [Weak]
%% type-checking termination type
rule(Gamma, Theta, Process, {_Clocks,'end'=_Type}=Delta)
->
  io:format("\n\n(~p) Warning, rule [Weak] indicates that the type as terminated, but the process has not.\n\nGamma:\t~p,\nTheta:\t~p,\nPrc:\t~p,\nDelta:~p.\n\n",[?LINE,Gamma, Theta, Process, Delta]),
  show_trace('Weak',false,{false}),
  {false, ['Weak']};
%%

%% @doc special case, allow recursive type to be unfolded
rule(Gamma, Theta, Process, {Clocks,{'def', _Name, S}=_Type}=_Delta)
-> rule(Gamma, Theta, Process, {Clocks, S});
%%

%% @doc unhandled case
rule(Gamma, Theta, Process, Delta) ->
  io:format("\n\n(~p) Warning, unhandled case:\nGamma:\t~p,\nTheta:\t~p,\nProcess:\t~p,\nDelta:~p.\n\n",[?LINE,Gamma, Theta, Process, Delta]),
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

%% @doc 
minimal_precision() -> 1.0/(list_to_integer(lists:flatten("1"++lists:duplicate(?DECIMAL_PRECISION,"0")))).


%% @doc rounds to DECIMAL_PRECISION decimal places
precision_rounding(Float) 
when is_float(Float) -> 
  Offset = math:pow(10.0,?DECIMAL_PRECISION),
  round(Float*Offset)/Offset.
%%


%% @doc starts evaluation workers for each integer of the given range, and collects their IDs in a list to return
% -spec start_evaluation_workers(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, integer(), float(), float(), float()) -> list().

%% @doc for starting the workers for rules (del-delta and recv)
start_evaluation_workers(Kind, {_Gamma, _Theta, _P, _Delta}=Judgement, #{is_first:=IsFirst,is_last:=IsLast,is_lower_exclusive:=IsLowerExclusive,is_upper_exclusive:=IsUpperExclusive,lower:=Lower,upper:=Upper,decrement:=Decrement}) ->
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
  WorkerID = spawn(?MODULE, evaluation_worker, [Kind, Judgement, precision_rounding(Counter1), precision_rounding(T), Decrement, PID]),
  %% create new map 
  Map1 = #{ is_upper_exclusive=>IsUpperExclusive,
            is_lower_exclusive=>IsLowerExclusive1,
            lower=>precision_rounding(Lower1),
            upper=>Upper,
            decrement=>Decrement,
            is_last=>IsNextLast,
            is_first=>false },
  %% call to start next worker 
  WorkerIDs = case IsLast of true -> []; _ -> start_evaluation_workers(Kind, Judgement, Map1) end,
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
-spec evaluation_worker(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, float(), float(), float(), pid()) -> atom().

%% @doc the first iteration of the worker sends back the result, evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_recv=Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}=Judgement, Counter, T, Decrement, PID)
when is_float(T) and is_float(Decrement) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace1} = rule(Gamma,Theta1,P,{Clocks2,Type}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement),
  %% send results to PID
  Holds = Result and NextEval,
  PID ! {self(), {eval, {Holds,[Trace1]++Trace2}}, {range, T-Counter, T}},
  %% return OK
  ok;
%%

%% @doc the first iteration of the worker sends back the result, evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_del_delta=Kind, {Gamma, Theta, P, Delta}=Judgement, Counter, T, Decrement, PID)
when is_float(T) and is_float(Decrement) ->
  %% evaluate for this current value of T
  {Result,Trace1} = rule(Gamma,Theta,{'delay',T,P},Delta),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement),
  %% send results to PID
  Holds = Result and NextEval,
  PID ! {self(), {eval, {Holds,[Trace1]++Trace2}}, {range, T-Counter, T}},
  %% return OK
  ok;
%%

%% @doc unexpected worker starter
evaluation_worker(Kind, {Gamma, Theta, P, Delta}=_Judgement, Counter, T, Decrement, PID)
when is_float(T) and is_float(Decrement) ->
  io:format("\n\n(~p) Warning, unknown evaluation_worker called: ~p.\nGamma: ~p, Theta: ~p,\nP: ~p,\nDelta: ~p,\nCounter: ~p,\nT: ~p,\nDecrement: ~p,\nPID: ~p.\n",[?LINE,Kind, Gamma, Theta, P, Delta, Counter, T, Decrement,PID]),
  ok.
%%


-spec evaluation_worker(atom(), {map(), map(), toast:process(), {map(),toast:types()}}, integer(), float(), float()) -> {boolean(),list()}.

%% @doc for catching the final evaluation
evaluation_worker(premise_recv=_Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}, Counter, T, _Decrement)
when (Counter=<0) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace} = rule(Gamma,Theta1,P,{Clocks2,Type}),
  %% return
  {Result,[Trace]};
%%

%% @doc evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_recv=Kind, {Gamma, Theta, P, {#{clocks:=Clocks,resets:=Resets},Type}}=Judgement, Counter, T, Decrement)
when is_float(T) and is_float(Decrement) ->
  %% increment clocks and timers
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),
  %% reset clocks
  Clocks2 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks1, Resets),
  %% evaluate for this current value of T
  {Result,Trace1} = rule(Gamma,Theta1,P,{Clocks2,Type}),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement),
  %% return result
  Holds = Result and NextEval,
  {Holds,[Trace1]++Trace2};
%%

%% @doc for catching the final evaluation
evaluation_worker(premise_del_delta=_Kind, {Gamma, Theta, P, Delta}, Counter, T, _Decrement)
when (Counter=<0) ->
  %% evaluate for this current value of T
  {Result,Trace} = rule(Gamma,Theta,{'delay',T,P},Delta),
  %% return
  {Result,[Trace]};
%%

%% @doc evaluates a delay of T-Decrement, and calls the next decrememnt
evaluation_worker(premise_del_delta=Kind, {Gamma, Theta, P, Delta}=Judgement, Counter, T, Decrement)
when is_float(T) and is_float(Decrement) ->
  %% evaluate for this current value of T
  {Result,Trace1} = rule(Gamma,Theta,{'delay',T,P},Delta),
  %% establish next decrement
  T1 = T - Decrement,
  Counter1 = Counter - Decrement,
  %% call next 
  {NextEval,Trace2} = evaluation_worker(Kind, Judgement, precision_rounding(Counter1), precision_rounding(T1), Decrement),
  %% return result
  Holds = Result and NextEval,
  {Holds,[Trace1]++Trace2};
%%

%% @doc unknown worker
evaluation_worker(Kind, {Gamma, Theta, P, Delta}, Counter, T, Decrement) -> 
  io:format("\n\n(~p) Warning, unknown evaluation_worker called: ~p.\nGamma: ~p, Theta: ~p,\nP: ~p,\nDelta: ~p,\nCounter: ~p,\nT: ~p,\nDecrement: ~p.\n",[?LINE,Kind, Gamma, Theta, P, Delta, Counter, T, Decrement]),
  false.
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

%% @doc for the type-checker printing its trace as it goes
show_trace(Rule,Pass,Premise) -> 
  case show_trace() of 
    true -> io:format("\nRule [~p] -> ~p. (~p)\n",[Rule,Pass,Premise]);
    _ -> ok
  end.
%%

%% @doc returns if trace should be shown
show_trace() -> ets:member(toast_checker_ets, show_trace) andalso ets:lookup(toast_checker_ets, show_trace).
