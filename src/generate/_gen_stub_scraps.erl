
%% @doc 
%% 
build_state_fun(Edges, States, RecMap, StateID, ScopeID, FunMap) ->
  %% check if this is a recursive entry point
  %% if state and scopeid are the same, then we do not need to create a new scope
  case StateID=:=ScopeID of
    true -> IsRec = false;
    _ -> 
      RecStates = maps:filter(fun(_K, V) -> (V=:=StateID) end, RecMap),
      IsRec = lists:foldl(fun(Elem, AccIn) -> (AccIn and Elem) end, true, RecStates)
  end,

  case IsRec of
    true -> %% create a new function scope, add call to current scope, and return
      %% 












%% @doc
%% @returns a list of module_rep => [ {true, atom_name, [Clauses]}, ... ]
-spec state_funs(list(), map(), map()) -> list().
state_funs(Edges, States, RecMap) -> 
  %% start traversing through states from beginning (0)
  {_NextID, _Clauses, Funs, _FunNames} = state_funs(Edges, States, RecMap, 0, -1, [], []),
  ?SHOW("(should be no clauses): ~p.", [_Clauses]),
  ?SHOW("fun names: ~p.", [_FunNames]),
  ?SHOW("funs: ~p.", [Funs]),
  ?assert(length(_Clauses)==0),
  Funs.

%% @doc 
%% takes a list of clauses (of current scope) to add to
% -spec state_funs(list(), map(), map(), integer(), integer(), list(), list()) -> {list(), list(), list()}.
state_funs(Edges, States, RecMap, StateID, ScopeID, Clauses, FunNames) -> 

  Funs = [],

  %% get state
  State = maps:get(StateID, States),

  ?GAP(),
  ?SHOW("'~p': '~p'.", [StateID, State]),

  %% get outgoing edges from stateID
  IsRelevant = fun(Edge) -> Edge#edge.from =:= StateID end, 
  RelevantEdges = lists:filter(IsRelevant, Edges),

  case lists:any(fun(Elem) -> Elem=:=State end, special_funs()) of
    true -> %% if special fun, then force new function scope, and continue on child
      {{_,FunName,_}=Fun, NextID} = gen_snippets:special_state(State, StateID, RelevantEdges, States),
      case NextID==-1 of %% is finished?
        true -> {Clauses, Fun, FunNames++[FunName]};
        _ -> %% continue on child
          {NextClauses, NextFuns, NextFunNames} = state_funs(Edges, States, RecMap, NextID, NextID, Clauses, FunNames++[FunName]),
          {NextClauses, [Fun]++NextFuns, NextFunNames}
      end;
    _ -> %% not a special state, so allow to continue in same function scope
      %% check if recursive state (appears in map and clauses are empty)
      RecStates = maps:filter(fun(_K, V) -> (V=:=StateID) end, RecMap),
      IsRec = lists:foldl(fun(Elem, AccIn) -> (AccIn and Elem) end, true, RecStates),
      case (IsRec and ((length(Clauses)==0) and (StateID=:=ScopeID))) of 
        true -> %% is a recursive state
          %% start new function scope
          {RecFunName, RecEnterClauses} = gen_snippets:state_enter(State, StateID, Edges, States),
          %% reenter function body (with nonempty clauses)
          {RecClauses, RecFuns, RecFunNames} = state_funs(Edges, States, RecMap, StateID, ScopeID, RecEnterClauses, FunNames++[RecFunName]),
          %% return after child 
          {[], RecFuns++[{true,RecFunName,RecClauses}], RecFunNames};
        _ -> %% this is the body of a function
          %% first, detect what kind of pattern this is (send, recv, select, branch, send-before-after, recv-before-after)
          %% we know that a recursive state cannot be within the same mixed-choice
          %% the following function returns all of the clauses within current state (including, recursive state calls)
          {EdgeClauses, EdgeRecStates, NextID} = gen_snippets:state_clauses(State, StateID, Edges, States, RecMap),
          %% for each of the unresolved states that require their own function, generate their functions
          EdgeFuns = lists:foldl(fun({RecStateID, RecState}) -> state_funs(Edges, States, RecMap, RecStateID, ok, ok) end, [], EdgeRecStates),
          %%
          EdgeFunNames = lists:foldl(fun({_K,V}=_Elem) -> V end, [], EdgeRecStates),
          {NextClauses, NextFuns, NextFunNames} = state_funs(Edges, States, RecMap, NextID, ScopeID, Clauses++EdgeClauses, FunNanes++EdgeFunNames),
          % {Clauses++EdgeClauses, ok }=ok,
          case State of
            standard_state -> %% single send or recv
              ok;
            choice_state -> %% multiple sends or recvs
              ok;
            recv_after_state -> %% recv {...} after timeout
              ok;
            branch_after_state -> %% branch {...} after timeout
              ok;
            send_after_state -> %% send {...} after timeout 
              ok;
            select_after_state -> %% select {...} after timeout
              ok;
            unknown_state -> %% 
              ok
          end,



          %% TODO: check how to detect what kind of pattern is this state (send-before-recv, recv-before-send)
          %% go through outgoing edges
          %% for each edge, first evaluate next state


          %% call on next state

          %% return as part of scope
          ok
      end
  end.

  % %% if scope and state are the same, then no need to check for new rec state
  % case ScopeID==StateID of
  %   true -> 
  %     IsRec = false,
  %     FunNames1 = [State] ++ FunNames;
  %   _ -> %% determine if this state should be made its own (recursive) function
  %     RecStates = maps:filter(fun(_K, V) -> V=:=StateID end, RecMap),
  %     IsRec = lists:foldl(fun(Elem, AccIn) -> AccIn and Elem end, true, RecStates),
  %     FunNames1 = FunNames;
  % end,
  % case IsRec of
  %   true ->
  %     %% renter state as new scope
  %     {_, InnerClauses, [{_,RecName,_}|_T]=RecFuns} = state_funs(Edges, States, RecMap, StateID, StateID, [], FunNames1),
  %     %% add inner funs
  %     NewFuns = RecFuns ++ Funs,
  %     NewFunNames = FunNames1 ++ [RecName],
  %     %% add call in current scope
  %     RecClause = merl_commented(pre, ["% below is recursive loop"], ["'@RecName@'(CoParty, Data)"]),
  %     NewClauses = Clauses ++ RecClause;
  %   _ ->
  %     %% explore all edges in current state
  %     {EdgeClauses, InnerFuns} = 
  %     InnerFunNames = lists:foldl(fun({_,FunName,_}=_Elem, AccIn) -> AccIn ++ [FunName] end, [], InnerFuns),
  %     NewFunNames = FunNames1 ++ InnerFunNames,

  %     NewFuns = InnerFuns ++ Funs,


  %     NewClauses = Clauses ++ state_clause(StateID, State, Edges, States, RecMap)

  % end,
  % {-1, NewClauses, NewFuns, NewFunNames}.
