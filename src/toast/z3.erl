-module(z3).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).

-define(SHOW_V3_SNIPPET,false).

-include("utils.hrl").


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
  case ?SHOW_V3_SNIPPET of true -> io:format("\n\n~p, ExecString:\n~s\n.",[_Kind,ExecString]); _ -> ok end,
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  case ?SHOW_V3_SNIPPET of true -> 
  io:format("\n\n~p, ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]); _ -> ok end,
  %% return result
  {ok, Z3Response, ExecString};
%%

%% @doc constructs python code to ask z3 if constraint holds, given clocks and INFINITY upperbound
ask_z3(feasible_constraints=_Kind, #{clocks:=Clocks,constraints:=Constraints,e:='infinity'=E}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(feasible_constraints, #{clocks=>Clocks,delta=>Constraints,e=>E}),
  case ?SHOW_V3_SNIPPET of true -> io:format("\n\n~p, (infinity) ExecString:\n~s\n.",[_Kind,ExecString]); _ -> ok end,
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  case ?SHOW_V3_SNIPPET of true -> 
  io:format("\n\n~p, (infinity) ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]); _ -> ok end,
  %% return result
  {ok, Z3Response, ExecString};
%%

%% @doc constructs python code to ask z3 if constraint holds, given clocks and non-infinite upperbound
ask_z3(feasible_constraints=_Kind, #{clocks:=Clocks,constraints:=Constraints,e:=E}) 
when is_map(Clocks) ->
  %% build build super-constraint string and see if it holds
  ExecString = to_python_exec_string(feasible_constraints, #{clocks=>Clocks,delta=>Constraints,e=>E}),
  case ?SHOW_V3_SNIPPET of true -> io:format("\n\n~p, (non-infinity) ExecString:\n~s\n.",[_Kind,ExecString]); _ -> ok end,
  %% send to python program and get response
  Z3Response = z3(ask_z3,[list_to_binary(ExecString)]),
  case ?SHOW_V3_SNIPPET of true -> 
  io:format("\n\n~p, (non-infinity) ExecString:\n~s\n\nResponse: ~p.\n",[_Kind,ExecString,Z3Response]); _ -> ok end,
  %% return result
  {ok, Z3Response, ExecString};
%%

%% @doc for each Clock valuation, asks z3 to check if they are t_reading
ask_z3(t_reading, #{clocks:=Clocks,type:=Type,t:=_T})
when is_map(Clocks) and is_float(_T) ->
  %% round T down to accepted precision
  T = checker:precision_rounding(_T),
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
    true -> {ok, false, ""};

    %% otherise, receptions
    _ ->
      %% for each reception, go through each clock and ask z3 if there exists t'<t such that constraints are satisfied
      AskZ3 = lists:foldl(fun(Constraints, {IsOk, InSat, InExecString}) ->
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
              true -> {ok, (InSat and Z3Response), InExecString++["\n\n",ExecString]};
              %% not okay, some error
              _ -> {error, [InSat,Z3Response], InExecString++["\n\n",ExecString]}
            end;

          %% not okay, continue to collect results in list, in attempt to analyse them afterwards
          _Err -> {IsOk, [InSat,Z3Response], InExecString++["\n\n",ExecString]}

        end

      end, {ok,true,[]}, ReceptionConstraints),
      %% check response from z3
      case AskZ3 of %lists:foldl(AskZ3, {ok, true}, ReceptionConstraints)

        {ok, IsSatisfied, OutExecString} -> {ok, IsSatisfied, lists:flatten(OutExecString)};

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
    {ok, Result, _ExecString} -> {ok, not Result, _ExecString};
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
  Clocks1 = checker:instansiate_clocks(Clocks,ClockList),
  %% build code for constants
  {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
  %% build code for clocks
  {ClockNames, ClockNamesSep, ClockDeclarations} = to_python_exec_string(vars, #{vars=>maps:to_list(Clocks1),datatype=>float}),
  %% denote if multiple constants
  IntString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
  RealString = case length(maps:keys(Clocks1))>1 of true -> "Reals"; _ -> "Real" end,
  %% accomodate for no constants
  ConstantString = case length(VarList)==0 of true -> ""; _ -> io_lib:format("~s = ~s('~s')\n", [ConstantNamesSep,IntString,ConstantNames]) end,
  %% build exec python string (for z3)
  ExecString = io_lib:format("~s~s = ~s('~s')\ns = Solver()\ns.add(~s, ~s)\ns.add(~s)\nresult = s.check()",[ConstantString,ClockNamesSep,RealString,ClockNames,ClockDeclarations,ConstantDeclarations,DeltaString]),
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
  Clocks1 = checker:instansiate_clocks(Clocks,ClockList),

  %% if infinity, no constants
  IntString = case IsInfinity of 
    true -> "";
    _ ->
    %% otherwise, constants
    %% build code for constants
    {ConstantNames, ConstantNamesSep, ConstantDeclarations} = to_python_exec_string(vars, #{vars=>VarList,datatype=>int}),
    
    % denote if multiple constants
    IntFunString = case length(VarList)>1 of true -> "Ints"; _ -> "Int" end,
    %% accomodate for no constants
    case length(VarList)==0 of true -> ""; _ -> io_lib:format("\n~s = ~s('~s')\ns.add(~s)",[ConstantNamesSep,IntFunString,ConstantNames,ConstantDeclarations]) end
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
  Clocks1 = checker:instansiate_clocks(Clocks,ClockList),
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
  Clocks1 = checker:instansiate_clocks(Clocks,ClockList),
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
to_python_exec_string(constraints, #{delta:={'true'}}=_Args) -> {{[],[]},"global_>=0"};

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
      int -> checker:to_integer(VarValue);
      float -> checker:to_float(VarValue);
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
