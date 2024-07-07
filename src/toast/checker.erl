-module(checker).
-compile(export_all).
-compile(nowarn_export_all).


-include_lib("stdlib/include/assert.hrl").
-include("utils.hrl").


%% special constraint used during type-checking for cascading receptions
-type cascade () :: {toast_process:upper_bound(), toast_type:constraints()}.

-type sugar_interact_type () :: {toast_type:direction(), toast_type:msg(), cascade()|toast_type:constraints(), toast_type:resets(), sugar_toast_type()}.

-type sugar_choice_type () :: [sugar_interact_type()].

-type sugar_def_type () :: {'def', string(), sugar_toast_type()}.

-type sugar_toast_type () :: sugar_interact_type() 
                           | sugar_choice_type()
                           | sugar_def_type()
                           | toast_type:call_type()
                           | toast_type:term_type().



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
  % set_show_trace(ShowTrace),

  %% begin eval
  io:format("\nBeginning type-checking eval of:\nGamma: ~p,\nTheta: ~p,\nProcess: ~p,\nDelta: ~p.\n\n",[Gamma,Theta,Process,Delta]),

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
-spec rule(map(), map(), toast:process(), {map(),sugar_toast_type()}) -> {boolean(), [[atom()]]} | {boolean(), [atom()]}.
rule(Gamma, Theta, Process, Delta) -> rule(Gamma, Theta, Process, Delta, #{show_print=>?DEFAULT_SHOW_TRACE}).

%% @doc wrapper for adding map of print args ^^
-spec rule(map(), map(), toast:process(), {map(),sugar_toast_type()}, map()) -> {boolean(), [[atom()]]} | {boolean(), [atom()]}.

%% @doc helper function to make sure /timersclocks are map not list of tuples
rule(Gamma, Theta, Process, Delta, #{show_print:=_ShowPrint}=_Args) 
when is_list(Theta) -> rule(Gamma, maps:from_list(Theta), Process, Delta, _Args);
rule(Gamma, Theta, Process, {Clocks,Type}, #{show_print:=_ShowPrint}=_Args) 
when is_list(Clocks) -> rule(Gamma, Theta, Process, {maps:from_list(Clocks),Type}, _Args);

% %% @doc helper function to convert delta tuple into map
% rule(Gamma, Theta, Process, {Role,Clocks,Type}) -> rule(Gamma, Theta, Process, #{Role=>{Clocks,Type}});

% %% @doc helper function to convert delta triple (currently unused) into tuple
% rule(Gamma, Theta, Process, {_Role,Clocks,Type}, #{show_print:=_ShowPrint}=_Args) -> rule(Gamma, Theta, Process, {Clocks,Type}, _Args);


%% @doc rule [Recv]
%% type-checking single reception
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P}=_Process, {Clocks,[{recv,{_Label,__Payload},Constraints,Resets,S}]=Type}=Delta, #{show_print:=ShowPrint}=_Args)
when is_atom(Label) and (Label=:=_Label) and is_list(Resets) ->

  case ShowPrint of true ->
  io:format("\n\n/ / / /[Recv]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  ok; _ -> ok end,

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

  %% add received message to new gamma
  Gamma1 = maps:put(_Payload,__Payload,Gamma),

  %% show initial trace now, with minimal value (which is always 0)
  %% continue type-checking (yields premise)
  {InitialContinuation, InitialTrace} = rule(Gamma1, Theta, P, {Clocks1,S}, _Args),

  % io:format("\nBoundMap: ~p.\n",[BoundMap]),

  %% suppress messages of eval
  % ShowTrace = get_show_trace(),
  % ets:insert(toast_checker_ets,{show_trace,false}),
  % set_show_trace(false),

  %% only use workers if not 0
  case E of
    0 -> {Continuation,OutTraces} = {true,[]};

    _ ->

    %% use worker functions to calculate in parallel
    io:format("\n> > > (~p) Rule ['Recv'] -- evaluating range.\n",[self()]),
    % Self = self(),
    WorkerIDs = workers:start_evaluation_workers(premise_recv, {Gamma1,Theta,P,{#{clocks=>Clocks,resets=>Resets},S}}, maps:put(show_print,?WORKER_SHOW_TRACE,BoundMap)),
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
    % ets:insert(toast_checker_ets,{{show_trace,self()},ShowTrace}),
    % set_show_trace(ShowTrace),

    %% add to traces
    OutTraces = add_to_trace(TraceList,'Recv')

    % %% undo suprpesion of eval messages
    % ets:insert(toast_checker_ets,{{show_trace,self()},ShowTrace})
  end,

  %% if Delta is only a tuple, and not a map containing other roles, then do not check E-reading
  case is_tuple(Delta) of 
    true -> NotEReading = true;
    
    %% if map, then need to check for all other in delta
    _ -> io:format("\n\n(~p) Warning, rule [Recv] does not currently support Delta maps.\n",[?LINE]),
      %% ask z3 if Delta is not E-reading
      Duration = case E of infinity -> ?INFINITY; _ -> E end,
      {Signal1, NotEReading} = z3:ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>to_float(Duration)}),
      ?assert(Signal1=:=ok),
      ?assert(is_boolean(NotEReading))
  end,

  
  {Signal2, FeasibleConstraints} = case Constraints of 
    %% if Constraints is a cascade, then use the cascade bound
    {cascade, BoundE, _Constraints} -> z3:ask_z3(feasible_constraints,#{clocks=>Clocks,constraints=>_Constraints,e=>BoundE});

    %% otherwise, use normal constraints
    _ -> z3:ask_z3(feasible_constraints,#{clocks=>Clocks,constraints=>Constraints,e=>E})
  end,

  % %% ask z3 if Delta if constraints feasible (satisfied bimplies E)
  % {Signal2, FeasibleConstraints} = ask_z3(feasible_constraints,#{clocks=>Clocks,constraints=>Constraints,e=>Ez3}),
  ?assert(Signal2=:=ok),
  ?assert(is_boolean(FeasibleConstraints)),

  %% construct premise and return
  Premise = InitialContinuation and Continuation and FeasibleConstraints and NotEReading,


  % %% if premise fail, print
  % case Premise of true -> ok; _ -> case ShowPrint of true -> ok; _ ->
  %   io:format("\n> > > Rule [Recv], premise fail: ~p\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[{InitialContinuation,Continuation,FeasibleConstraints,NotEReading},_Process,Clocks,Type])
  % end end,

  show_trace('Recv',Premise,{InitialContinuation,Continuation,FeasibleConstraints,NotEReading},ShowPrint),
  {Premise, lists:uniq(OutTraces)};
%%


%% @doc rule [Send]
%% type-checking one send from type
rule(Gamma, Theta, {_Role,'<-',{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta, #{show_print:=ShowPrint}=_Args)
when is_list(Type) ->

  case ShowPrint of true ->
  io:format("\n\n/ / / /[Send]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  ok; _ -> ok end,


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
  % io:format("\nsend: ~p.\n",[Send]),

  %% reset clocks
  Clocks1 = lists:foldl(fun(Clock, NewClocks) -> 
    maps:put(Clock,0,NewClocks)
  end, Clocks, Resets),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta, P, {Clocks1,S}, _Args),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Send'),

  %% ask z3 if Delta if constraints satisfied
  {Signal, SatisfiedConstraints} = z3:ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
  ?assert(Signal=:=ok),
  ?assert(is_boolean(SatisfiedConstraints)),
  
  %% construct premise and return
  Premise = Continuation and SatisfiedConstraints,
  show_trace('Send',Premise,{Continuation,SatisfiedConstraints},ShowPrint),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Branch]
%% type-checking each reception in branch 
%% @see rule [Recv]
rule(Gamma, Theta, {Role,'->',E,Branches}=_Process, {Clocks,Type}=_Delta, #{show_print:=ShowPrint}=_Args)
when is_list(Branches) and is_list(Type) ->

  case ShowPrint of true ->
  io:format("\n\n/ / / /[Branch]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  ok; _ -> ok end,
  

  %% for each reception in Type that is enabled now
  Pairings = lists:foldl(fun({Direction_j,{Label_j,_Payload_j},Constraints_j,_Resets_j,_S_j}=Type_j, InPairs) -> 
    %% check recv
    case Direction_j of 
      recv ->
      %% fix constraints if cascading
      {BoundE, Constraints} = case Constraints_j of 
        {cascade,_BoundE,_Constraints_j} -> {_BoundE,_Constraints_j};
        _ -> {E,Constraints_j}
      end,

      %% ask z3 if Delta if constraints satisfied
      {Signal_j, SatisfiedConstraints_j} = z3:ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
      ?assert(Signal_j=:=ok),
      ?assert(is_boolean(SatisfiedConstraints_j)),
      %% check if enabled
      case SatisfiedConstraints_j of 
        %% search for corresponding branch in process
        true -> 
          % io:format("\n\nenabled, ~p,\n\tclocks: ~p.\n",[Type_j,Clocks]),
          Pair = lists:foldl(fun({{Label_i,Payload_i}, P_i}, OutPairs) ->
            case Label_i=:=Label_j of 
              true -> OutPairs++[{Type_j,{Role,'->',BoundE,{Label_i,Payload_i},P_i}}];
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
        InEvals++[rule(Gamma,Theta,RecvBranch,{Clocks,RecvType}, _Args)]
      end, [], Pairings),

      %% check if all continuations passed & update trace
      {Continuation, TraceList} = lists:foldl(fun({Eval,Trace}, {InEval,InTrace}) -> 
        {(Eval and InEval),add_to_trace(Trace,'Branch')++InTrace} 
      end, {true,[]}, Continuations);

    %% each branch is not evaluated
    _ -> 
      io:format("\n\n(~p) Warning, rule [Branch], found (~p) enabled types to pair (~p) with branches:\nType:\t~p,\nPrc:\t~p.\n",[?LINE,length(Pairings),length(Branches),Type,_Process]),
      Continuation = false, TraceList = ['Branch']
  
  end,

  %% construct premise and return
  Premise = Continuation,
  show_trace('Branch',Premise,{Continuation},ShowPrint),
  {Premise, lists:uniq(TraceList)};
%%


%% @doc rule [Timeout]
%% type-checking branches with timeouts 
%% @see rule [Branch]
rule(Gamma, Theta, {Role,'->',E,Branches,'after',Q}=_Process, {Clocks,Type}=_Delta, #{show_print:=ShowPrint}=_Args)
when is_list(Branches) and is_list(Type) ->

  ?assert(E=/='infinity'),
  ?assert(is_list(Type)),

  %% construct new type that compensates for cascading types
  Type1 = lists:foldl(fun({_Direction,{_Label,_Payload}=_Msg,Constraints,_Resets,_S}=Interaction, TypesIn) -> 
    %% check if constraints are cascading or not
    TypesIn++case Constraints of 
      %% if cascading, leave unmodified
      {cascade, _, _} -> [Interaction];

      %% if not cascading, check if they should be 
      _ -> [cascade(_Process,{Clocks, Interaction})]
    end
  end, [], Type),

  
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Timeout-P]/ / / /\n\nP:\t~p,\nC:\t~p,\nT1: (modified)\n\t~p,\nT: (unmodified)\n\t~p.\n",[_Process,Clocks,Type1,Type]),
  ok; _ -> ok end,

  % timer:sleep(5000),
  
  %% check via rule [Branch]
  {Continuation1, Trace1} = rule(Gamma, Theta, {Role,'->',E,Branches}, {Clocks,Type1}),

  case ShowPrint of true ->
  io:format("\n\n/ / / /[Timeout-Q]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type1]),
  ok; _ -> ok end,

  %% check Q (let E pass over Theta and Clocks)
  case E of
    %% this would be impossible to compute
    infinity -> Continuation2 = false, Trace2 = [];

    %% easy
    0 -> {Continuation2, Trace2} = rule(Gamma, Theta, Q, {Clocks,Type1}, _Args);

    %% incrememnt clocks and timers by E, then type-check against Q
    {Less,_ValE} -> 
      ValE = case Less of 'leq' -> _ValE; 'les' -> _ValE-minimal_precision(); _ -> io:format("\n\n(~p) Warning, the DBC in E is not recognised: ~p.\n",[?LINE,E]),_ValE end,
      Theta1 = increment_timers(Theta,ValE), 
      Clocks1 = increment_clocks(Clocks,ValE),
      {Continuation2, Trace2} = rule(Gamma, Theta1, Q, {Clocks1,Type1}, _Args)

  end,

  %% merge traces and update
  Trace3 = add_to_trace(Trace1++Trace2,'Timeout'),

  %% construct premise and return
  Premise = Continuation1 and Continuation2,
  show_trace('Timeout',Premise,{Continuation1,Continuation2},ShowPrint),
  {Premise, lists:uniq(Trace3)};
%%


%% @doc rule [IfTrue] or [IfFalse]
%% type-checking if-true/if-false
rule(Gamma, Theta, {'if',{Timer, DBC, Value}, 'then', P, 'else', Q}=_Process, {_Clocks,_Type}=Delta, #{show_print:=ShowPrint}=_Args)
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
      case ShowPrint of true ->
      io:format("\n\n/ / / /[IfTrue]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
      ok; _ -> ok end,

      %% continue with P
      {Continuation, Trace} = rule(Gamma, Theta, P, Delta, _Args),

      %% set rule name
      RuleName = 'IfTrue';

    _ -> 
      case ShowPrint of true ->
      io:format("\n\n/ / / /[IfFalse]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
      ok; _ -> ok end,

      %% continue with P
      {Continuation, Trace} = rule(Gamma, Theta, Q, Delta, _Args),

      %% set rule name
      RuleName = 'IfFalse'
  end,

  %% merge traces and update
  Trace1 = add_to_trace(Trace,RuleName),

  %% construct premise and return
  Premise = Continuation,
  show_trace(RuleName,Premise,{Continuation},ShowPrint),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Timer]
%% type-checking setting timers
rule(Gamma, Theta, {'set', Timer, P}=_Process, {_Clocks,_Type}=Delta, #{show_print:=ShowPrint}=_Args)
->
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Timer]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
  ok; _ -> ok end,

  %% set timer to 0 (regardless of if it existed before or not)
  Theta1 = maps:put(Timer, 0, Theta),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta1, P, Delta, _Args),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Timer'),

  %% construct premise and return
  Premise = Continuation,
  show_trace('Timer',Premise,{Continuation},ShowPrint),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Del-delta]
%% type-checking non-deterministic delays
rule(Gamma, Theta, {'delay',{t, DBC, N},P}=_Process, {_Clocks,_Type}=Delta, #{show_print:=ShowPrint}=_Args)
when is_atom(DBC) and (is_atom(N) or is_integer(N)) ->
  
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Del-delta]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
  ok; _ -> ok end,


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
  {InitialContinuation,InitialTrace} = rule(Gamma, Theta, {'delay',Init, P}, Delta, _Args),

  % io:format("\nBoundMap: ~p.\n",[BoundMap]),

  %% suppress messages of eval
  % ShowTrace = get_show_trace(),
  % % ets:insert(toast_checker_ets,{{show_trace,self()},false}),
  % set_show_trace(false),

  %% use worker functions to calculate in parallel
  io:format("\n> > > (~p) Rule ['Del-delta'] -- evaluating range.\n",[self()]),
  % Self = self(),
  WorkerIDs = workers:start_evaluation_workers(premise_del_delta, {Gamma,Theta,P,Delta}, maps:put(show_print,?WORKER_SHOW_TRACE,BoundMap)),
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
  % ets:insert(toast_checker_ets,{{show_trace,self()},ShowTrace}),
  % set_show_trace(ShowTrace),

  %% construct premise and return
  Premise = InitialContinuation and Continuation,
  show_trace('Del-delta',Premise,{InitialContinuation,Continuation},ShowPrint),
  {Premise, lists:uniq(OutTraces)};
%%


%% @doc rule [Del-t]
%% type-checking determined delays
rule(Gamma, Theta, {'delay',T,P}=_Process, {Clocks,Type}=_Delta, #{show_print:=ShowPrint}=_Args)
when is_number(T) ->
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Del-t]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,Clocks,Type]),
  ok; _ -> ok end,

  %% increment timers and clocks y T
  Theta1 = increment_timers(Theta,T),
  Clocks1 = increment_timers(Clocks,T),

  %% continue type-checking (yields premise)
  {Continuation, Trace} = rule(Gamma, Theta1, P, {Clocks1,Type}, _Args),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Del-t'),

  %% ask z3 if Delta is not T-reading
  {Signal, NotTReading} = z3:ask_z3(not_t_reading,#{clocks=>Clocks,type=>Type,t=>to_float(T)}),
  ?assert(Signal=:=ok),
  ?assert(is_boolean(NotTReading)),

  %% construct premise and return
  Premise = Continuation and NotTReading,

  % %% if premise fail, print
  % case Premise of true -> ok; _ -> case ShowPrint of true -> ok; _ ->
  %   io:format("\n> > > Rule [Del-t], premise fail: ~p\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[{Continuation,NotTReading},_Process,Clocks,Type])
  % end end,

  show_trace('Del-t',Premise,{Continuation,NotTReading},ShowPrint),
  {Premise, lists:uniq(Trace1)};
%%


%% @doc rule [Rec]
%% type-checking recursive definitions
rule(Gamma, Theta, {'def', P, 'as', {Var, Msgs}}=_Process, {_Clocks,_Type}=Delta, #{show_print:=ShowPrint}=_Args)
->
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Rec]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
  ok; _ -> ok end,

  %% check name not already used
  FreeName = not is_map_key(Var,Gamma),
  %% assign into Gamma
  Gamma1 = maps:put(Var,Msgs,Gamma),
  
  %% continue type-checking
  {Continuation, Trace} = rule(Gamma1, Theta, P, Delta, _Args),
  
  %% update Trace
  Trace1 = add_to_trace(Trace,'Var'),

  %% construct premise and return
  Premise = Continuation and FreeName,
  io:format("\nRule [Rec] -> ~p. (~p)\n",[Premise,{Continuation,FreeName}]),
  show_trace('Rec',Premise,{Continuation,FreeName},ShowPrint),
  {Premise, Trace1};
%%


%% @doc rule [Var]
%% type-checking recursive calls
rule(Gamma, _Theta, {'call', {Var, Msgs}}=_Process, {_Clocks,{'call', Name}=_Type}=_Delta, #{show_print:=ShowPrint}=_Args)
->
  case ShowPrint of true ->
  io:format("\n\n/ / / /[Var]/ / / /\n\nP:\t~p,\nC:\t~p,\nT:\t~p.\n",[_Process,_Clocks,_Type]),
  ok; _ -> ok end,

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
  show_trace('Var',Premise,{NamesMatch,IsInGamma,MsgsValid},ShowPrint),
  {Premise, ['Var']};
%%


%% @doc rule [End]
%% type-checking termination type
rule(_Gamma, _Theta, 'term'=_Process, {_Clocks,Type}=_Delta, #{show_print:=ShowPrint}=_Args)
->
  % io:format("\n\n/ / / /[End]/ / / /\n"),

  %% type must be end
  Premise = (Type=:='end'),
  show_trace('End',Premise,{Premise},ShowPrint),
  {Premise, ['End']};
%%

%% @doc rule [Weak]
%% type-checking termination type
rule(Gamma, Theta, Process, {_Clocks,'end'=_Type}=Delta, #{show_print:=ShowPrint}=_Args)
->
  io:format("\n\n(~p) Warning, rule [Weak] indicates that the type as terminated, but the process has not.\n\nGamma:\t~p,\nTheta:\t~p,\nPrc:\t~p,\nDelta:~p.\n\n",[?LINE,Gamma, Theta, Process, Delta]),
  show_trace('Weak',false,{false},ShowPrint),
  {false, ['Weak']};
%%

%% @doc special case, allow recursive type to be unfolded
rule(Gamma, Theta, Process, {Clocks,{'def', _Name, S}=_Type}=_Delta, #{show_print:=_ShowPrint}=_Args)
-> rule(Gamma, Theta, Process, {Clocks, S}, _Args);
%%


%% @doc helper function to wrap any lone branch process in list (to reapply rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P}=_Process, {Clocks,Type}=_Delta, #{show_print:=_ShowPrint}=_Args)
when is_list(Type) -> rule(Gamma, Theta, {_Role,'->',E,[{{Label,_Payload},P}]}, {Clocks,Type}, _Args);


%% @doc helper function to wrap any timeout process with a lone branch in list (to reapply rule [Recv])
%% @see rule [Recv]
rule(Gamma, Theta, {_Role,'->',E,{Label,_Payload},P,'after',Q}=_Process, {Clocks,Type}=_Delta, #{show_print:=_ShowPrint}=_Args)
when is_list(Type) -> rule(Gamma, Theta, {_Role,'->',E,[{{Label,_Payload},P}],'after',Q}, {Clocks,Type}, _Args);


%% @doc helper function to wrap any lone interact type in list (now that we are past rule [Recv])
%% NOTE: must be applied after rule [Var] to allow 'call' to be type-checked
%% @see rule [Recv]
rule(Gamma, Theta, Process, {Clocks,Type}=_Delta, #{show_print:=_ShowPrint}=_Args)
when is_tuple(Type) -> rule(Gamma, Theta, Process, {Clocks,toast:interactions(Type)}, _Args);


%% @doc unhandled case
rule(Gamma, Theta, Process, Delta, #{show_print:=_ShowPrint}=_Args) ->
  io:format("\n\n(~p, ~p) Warning, unhandled case:\nGamma:\t~p,\nTheta:\t~p,\nProcess:\t~p,\nDelta:~p,\nArgs:\t~p.\n\n",[?LINE,self(),Gamma, Theta, Process, Delta,_Args]),
  timer:sleep(5000),
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




%% @doc for the type-checker printing its trace as it goes
show_trace(Rule,Pass,Premise,ShowPrint) -> 
  case (?DEFAULT_SHOW_TRACE and ShowPrint) of 
    true -> io:format("\nRule [~p] -> ~p. (~p)\n",[Rule,Pass,Premise]);
    _ -> ok
  end.
%%




%% @doc handles joining two bounds for timeouts.
%% used when exploring cascading receptions, and in label map
-spec join_es(toast_process:upper_bound(), toast_process:upper_bound()|atom()) -> toast_process:upper_bound()|atom().

join_es(E, undefined) -> E;
join_es(_, infinity) -> infinity;
join_es(E, 0) -> E;
join_es(0, E) -> E;
join_es({_, _N}, {DBC, N}) -> {DBC, _N+N};
join_es(_1,_2) -> io:format("\n\n(~p) Warning ~p, unexpected case: ~p.\nReturning 0.\n",[?LINE,?FUNCTION_NAME,{_1,_2}]), 0.




%% @doc extracts the number of an E, given it is not infinity
-spec num(toast_process:upper_bound()) -> integer().

num(0) -> 0;
num({_,N}) -> N;
num(infinity) -> io:format("\n\n(~p) Warning ~p, does not support infinity. Returning ?INFINITY.\n",[?LINE,?FUNCTION_NAME]), ?INFINITY.





%% @doc processes a single toast type in the case it has a reception that is implemented as a cascading receive in the process.
%% if it is not receiving or cascading, then it is unchanged
%% however, if it is a cascading receive, the constraint is wrapped in a cascade type
-spec cascade(toast_type:timeout_process(), {map(), toast_type:interact_type()}) -> sugar_interact_type().
cascade({_Role,'->',E,_Branches,'after',_Q}=P, {Clocks, {Direction,{Label,_Payload}=Msg,Constraints,Resets,Next}=S})
-> 
  Ret = cascade(P, Clocks, S),
  case (length(maps:keys(Ret)))==0 of 
    true -> S;
    _ -> 
      ?assert(is_map(Ret)),
      %% since (other) cascade function starts on the same process, it must contain this label
      ?assert(is_map_key(Label,Ret)),
      %% check if the corresponding bound is the same as E (not cascading) or something else (cascading)
      Bound = maps:get(Label,Ret),
      case Bound of 
        %% if the same as E, then not cascading and return S
        E -> 
          % io:format("label (~p) is not cascading. (~p =:= ~p)\n",[Label,E,Bound]), 
          S;

        %% otherwise, is cascading, and use 
        _ -> 
          io:format("label (~p) is cascading, using: ~p.\n",[Label,Bound]),
          %% amend constraint into cascade type and return
          {Direction, Msg, {cascade, Bound, Constraints}, Resets, Next}
      end
  end.
%%


-spec cascade(toast_type:timeout_process()|toast_type:branch_process(), map(), toast_type:interact_type()) -> map().

%% wrap single recv in list
cascade({Role,'->',E,{_Label,_Payload},NextP}=_P, Clocks, S) -> cascade({Role,'->',E,[{{_Label,_Payload},NextP}]}, Clocks, S);

%% branch 
cascade({_Role,'->',E,_Branches}=_P, Clocks, {Direction,{Label,_Payload}=_Msg,Constraints,_Resets,_Next}=_S) 
-> 
  case Direction of 
    'send' -> #{};
    'recv' -> %% check if currently enabled
      %% ask z3 if Delta if constraints satisfied
      {Signal, SatisfiedConstraints} = z3:ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
      ?assert(Signal=:=ok),
      ?assert(is_boolean(SatisfiedConstraints)),

      case SatisfiedConstraints of 
        %% must be returned in map assigning label to E
        true -> 
          % io:format("\n(~p) Label (~p) returned mapped to: ~p.\n",[?LINE,Label,E]), 
          #{Label=>E};
        
        %% not enabled, just return empty
        _ -> 
          % io:format("\n(~p) Label (~p) returned empty map.\n",[?LINE,Label]), 
          #{}
      end
  end;
%%

cascade({_Role,'->',E,_Branches,'after',Q}=_P, Clocks, {Direction,{Label,_Payload}=_Msg,Constraints,_Resets,_Next}=S)
-> 
  %% if timeout, cannot be infinity
  ?assert(E=/='infinity'),
  %% 
  case Direction of
    'send' -> #{};
    'recv' -> %% check if currently enabled
      %% ask z3 if Delta if constraints satisfied
      {Signal, SatisfiedConstraints} = z3:ask_z3(satisfied_constraints,#{clocks=>Clocks,constraints=>Constraints}),
      ?assert(Signal=:=ok),
      ?assert(is_boolean(SatisfiedConstraints)),

      case SatisfiedConstraints of 
        % must check now if in Q
        true -> 
          %% increment clocks by n
          Clocks1 = increment_clocks(Clocks,num(E)),
          %% check for Q (if not cascade, uses 0 to ensure current E will be used)
          Num = maps:get(Label, cascade(Q,Clocks1,S), 0),
          %% must be returned in map assigning label to E  
          Bound = join_es(E,Num),
          % io:format("\n(~p) Label (~p) returned mapped to: ~p (joined (~p) and (~p)).\n(Process: ~p)\n\n",[?LINE,Label,Bound,E,Num,_P]), 
          #{Label=>Bound};
        %% not enabled, just return empty
        _ -> 
          % io:format("\n(~p) Label (~p) returned empty map.\n",[?LINE,Label]), 
          #{}
      end
  end;
%%

%% @doc for any other process, return nothing
cascade(_P, _Clocks, _S) -> 
  % io:format("\n(~p) Checked unsupported process.\n",[?LINE]), 
  #{}.
