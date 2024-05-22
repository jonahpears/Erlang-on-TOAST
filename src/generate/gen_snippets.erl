%% @doc Erlang snippets used in stub generation
-module(gen_snippets).
-compile({nowarn_unused_function, [ {get_state_name,2}, {get_next_state_trans,2} ]}).

-export([ special_state/3,
          state/6
          % state_enter/4,
          % state_clauses/4,
          % edge_clauses/0
        ]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").
-include("stub_tools.hrl").

%% @doc 
get_state_name(State, _StateID) when is_atom(State) and is_integer(_StateID) -> 
  StateID = integer_to_list(_StateID),
  case State of 
    end_state -> end_state;
    custom_end_state -> custom_end_state;
    standard_state -> list_to_atom("state" ++ StateID ++ "_std");
    choice_state -> list_to_atom("state" ++ StateID ++ "choice_after");
    recv_after_state -> list_to_atom("state" ++ StateID ++ "_recv_after");
    branch_after_state -> list_to_atom("state" ++ StateID ++ "_branch_after");
    send_after_state -> list_to_atom("state" ++ StateID ++ "_send_after");
    select_after_state -> list_to_atom("state" ++ StateID ++ "_select_after");
    after_state -> list_to_atom("state" ++ StateID ++ "_after");
    fatal_timeout_state -> list_to_atom("state" ++ StateID ++ "_fatal_timeout");
    _Else -> list_to_atom("state" ++ StateID ++ "_unexpected_" ++ atom_to_list(State))
  end.
%%

%% @doc 
get_next_state_trans(To, NextID) when is_atom(To) and is_integer(NextID) ->
  case To of 
    end_state -> {stop, normal};
    _ ->
      NextState = get_state_name(To, NextID),
      case To of 
        custom_end_state -> {next_state, NextState};
        standard_state -> {next_state, NextState};
        choice_state -> {next_state, NextState};
        recv_after_state -> {next_state, NextState};
        branch_after_state -> {next_state, NextState};
        send_after_state -> {next_state, NextState};
        select_after_state -> {next_state, NextState};
        after_state -> {next_state, NextState};
        fatal_timeout_state -> {next_state, NextState};
        _Else -> {next_state, NextState}
      end
  end.
%%

%% TODO: process beginning and end first, so that the rest of the functions/events/actions are able to occur within main()
%% TODO: recursion: add more data too states/nodes, to signify if part of loop

%% @doc generates clause for actions/outgoing edges
edge(#edge{from=_From,to=_To,edge_data=#edge_data{event_type = _EventType,event = Event,trans_type = TransType,pattern = _Pattern,args = _Args,guard = _Guard,code = _Code,attributes = _Attributes,comments = _Comments}=_EdgeData,is_silent=_IsSilent,is_delayable_send = _IsDelayableSend,is_custom_end = _IsCustomEnd,is_internal_timeout_to_supervisor = _IsInternalTimeoutToSupervisor}=Edge) -> 

  reng_show(edge, Edge, "\nbuilding edge snippet:\n"),

  {_Act, Var} = Event,
  StrVar = atom_to_list(Var),
  case TransType of
    send ->
      PayloadClause = merl_commented(pre, ["% replace 'ok' below with some payload"], ?Q(["Payload_"++StrVar++" = ok"])),
      SendClause = merl_commented(pre, ["% "], ?Q(["CoParty ! {self(), "++StrVar++", Payload_"++StrVar++"}"])),
      Clauses = [PayloadClause,SendClause],
      {Clauses, none, ok};
    recv -> 
      RecvClause = merl_commented(pre, ["% "], ?Q(["receive {CoParty, "++StrVar++" Payload_"++StrVar++"} -> "])),
      Clauses = [RecvClause],
      PostClauses = [?Q(["end"])],
      {Clauses, post, PostClauses};
    _ ->
      {[], none, ok}
  end.

%% @doc generates snippets for a given state
%% if recursive state, 
%% @returns tuple of {list_of_state_funs, list_of_next_states}
state(standard_state=State, StateID, {ScopeID, ScopeName}=Scope, Edges, States, RecMap) ->

  ?SHOW("state: ~p.", [State]),
  ?SHOW("StateID: ~p.", [StateID]),
  ?SHOW("Scope: ~p.", [Scope]),
  ?SHOW("States: ~p.", [States]),
  ?SHOW("RecMap: ~p.", [RecMap]),

  %% if first time entering scope, add state_enter to clauses
  case StateID=:=ScopeID of
    true -> _Clauses = [state_enter(State,StateID)];
    _ -> _Clauses = []
  end,

  RelevantEdges = get_relevant_edges(StateID, Edges),
  reng_show(edges,RelevantEdges,"\nrelevant edges:\n"),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  ?SHOW("passed assertion.\n",[]),

  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  NextState = get_state_name(ToState, NextID),

  StrNextID = integer_to_list(NextID),
  StrStateID = integer_to_list(StateID),

  %% clause for fresh Data for this state
  DataClause = merl_commented(pre, ["% fresh Data for state '@StateID@': \''@State@'\'"], ?Q(["Data"++StrNextID++" = Data"++StrStateID])),

  %% get clause for action
  {EdgeClause, Signal, PostClauses} = edge(Edge),

  %% check if destination state is recursive and within different scope
  case is_state_recursive(StateID, RecMap) and (StateID=/=ScopeID) of
    true -> %% add to list of next states and add call to function to clauses
      LoopName = get_loop_name(NextState),
      %% do not add {NextID,ScopeName} since these will be built within scope next time
      NextStateFuns = [{StateID,LoopName}],
      StateFunsTail = [],
      %% add function call to clauses
      ScopeClause = merl_commented(pre, ["% enter recursive state with fresh data"], ?Q(["'@LoopName@'(CoParty, Data'@NextID@')"])),
      Clauses = [DataClause,EdgeClause,ScopeClause];
    _ -> %% not a recursive state, continue adding clauses to current state
      %% next state funs is empty since we have no found a reason to start a new scope
      % NextStateFuns = [], %% {NextID,ScopeName}
      %% continue to next state
      {_StateFuns, NextStateFuns} = state(NextState, NextID, Scope, Edges,States,RecMap),
      %% check if any further statefuns were returned
      case length(_StateFuns)==0 of
        true -> %% no other statefuns returned
          NextClauses = [],
          StateFunsTail = [];
        _ -> %% statefuns returned
          %% if within same scope (head) then extract clauses and add rest to tail
          [{true,_ScopeName,_NextClauses}|_StateFunsTail] = _StateFuns,
          case _ScopeName=:=ScopeName of 
            true -> %% head is from same scope, add all clauses, add tail to tail
              NextClauses = _NextClauses,
              StateFunsTail = _StateFunsTail;
            _ -> %% nothing from new scope, add whole _StateFuns to tail
              NextClauses = [],
              StateFunsTail = _StateFuns
          end
      end,
      %% any next states should be within the same scope
      % ?SHOW("ScopeName: ~p.", [ScopeName]),
      % ?SHOW("_ScopeName: ~p.", [_ScopeName]),
      % ?assert(ScopeName=:=_ScopeName),
      %% add nextclauses to end of current clauses
      Clauses = [DataClause,EdgeClause]++NextClauses
  end,

  %% check for any post clauses
  case Signal of
    post -> StateFuns = [{true,ScopeName,_Clauses++Clauses++PostClauses}];
    _ -> StateFuns = [{true,ScopeName,_Clauses++Clauses}]
  end,
  {StateFuns++StateFunsTail, NextStateFuns};
%% 

%% @doc 
state(custom_end_state=State, StateID, _ScopeID, _Edges, __States, _RecMap) ->
  % {Signal, Fun, _NextState} = special_state(State, StateID, Edges),
  % ?assert(Signal=:=none),
  % {[Fun], []};
  {[],[{StateID,State}]};
%% 

%% @doc 
state(State, _StateID, _ScopeID, _Edges, _States, _RecMap) ->
  StateFuns = [],
  NextStateFuns = [],

  ?SHOW("unhandled: ~p. (return empty)", [State]),


  {StateFuns, NextStateFuns}.
%% 

%% @doc 
%% @returns tuple 
state_enter(State, StateID) ->
  %% check if current state is recursive

  %% get next state after the transition
  % To = maps:get(Edge#edge.to, States),
  % NextID = integer_to_list(Edge#edge.to),
  % NextState = get_state_name(To, NextID),

  Name = get_state_name(State, StateID),

  % Clause = ?Q(["(CoParty) -> ","'@Name@'(CoParty, [])"])
  Clause = ?Q(["(CoParty, Data) -> ok"]),

  Clauses = [Clause],

  {Name, Clauses}.
%%

%% @doc 
%% @returns tuple containing merl function clauses, and nextstate ID matched with the corresponding function name of its scope.
special_state(init_state=_State, StateID, Edges) ->
  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?assert(length(RelevantEdges)==1),
  InitEdge = lists:nth(1, RelevantEdges),
  NextStateID = InitEdge#edge.to,

  ?SHOW("init_state, NextStateID: ~p.",[NextStateID]),

  Name = run,
  Main = main,


  Clause1 = merl_commented(pre, [
      "% @doc Adds default empty list for Data.",
      "% @see '@Name@'/2."
    ],?Q([
      "(CoParty) -> ",
      "'@Name@'(CoParty, [])"
    ])),

  Clause2 = merl_commented(pre, [
      "% @doc Called immediately after a successful initialisation.",
      "% Add any setup functionality here, such as for the contents of Data.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a list to store data inside to be used throughout the program."
    ],?Q([
      "(CoParty, Data) -> %% add any init/start preperations below, before entering '@Main@'",
      "",
      "'@Main@'(CoParty, Data)"
    ])),

  Clauses = [Clause1, Clause2],

  {next_state, {true, Name, Clauses}, {NextStateID, Main}};
%%

%% @doc 
%% @see state(custom_end_state, ...)
special_state(end_state=_State, StateID, Edges) -> special_state(custom_end_state, StateID, Edges);

%% @doc 
special_state(custom_end_state=_State, _StateID, _Edges) ->
  Name = stopping,

  %%
  ClauseDefault = merl_commented(pre, [
      "% @doc Adds default reason 'normal' for stopping.",
      "% @see '@Name@'/3."
    ],?Q([
      "(CoParty, Data) -> ",
      "'@Name@'(normal, CoParty, Data)"
    ])),

  %%
  ClauseNormal = merl_commented(pre, [
      "% @doc Adds default reason 'normal' for stopping.",
      "% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}."
    ],?Q([
      "(normal=Reason, _CoParty, _Data) -> ",
      "exit(normal)"
    ])),

  %%
  ClauseError = merl_commented(pre, [
      "% @doc stopping with error.",
      "% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a list to store data inside to be used throughout the program."
    ],?Q([
      "({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> ",
        "erlang:error(Reason, Details)"
    ])),

  %%
  ClausePartialError = merl_commented(pre, [
      "% @doc Adds default Details to error."
    ],?Q([
      "({error, Reason}, CoParty, Data) when is_atom(Reason) -> ",
      "'@Name@'({error, Reason, []}, CoParty, Data)"
    ])),

  %%
  ClauseUnknown = merl_commented(pre, [
      "% @doc stopping with Unexpected Reason."
    ],?Q([
      "(Reason, _CoParty, _Data) when is_atom(Reason) -> ",
      "exit(Reason)"
    ])),

  Clauses = [ClauseDefault, ClauseNormal, ClauseError, ClausePartialError, ClauseUnknown],

  {none, {true, Name, Clauses}, ok};
%%

special_state(Kind, StateID, Edges) when is_atom(Kind) ->
  ?SHOW("unexpected kind: ~p.", [Kind]),
  ?SHOW("stateid: ~p.", [StateID]),
  ?SHOW("edges: ~p.", [Edges]),
  {unknown, {true, Kind, []}, -1}.
%%

%% @doc 
get_relevant_edges(StateID, Edges) -> 
  %% get outgoing edges from stateID
  IsRelevant = fun(Edge) -> Edge#edge.from =:= StateID end, 
  RelevantEdges = lists:filter(IsRelevant, Edges),
  RelevantEdges.
%%

%% @doc retrieves all recursive variables bound to StateID.
%% @returns list of recursive variable names bound to StateID
get_recursive_vars(StateID, RecMap) ->
  RecStates = maps:filter(fun(_K, V) -> (V=:=StateID) end, RecMap),
  maps:keys(RecStates).
%%

%% @doc for use in boolean conditions 
%% @returns true if StateID appears in RecMap
is_state_recursive(StateID, RecMap) -> length(get_recursive_vars(StateID, RecMap)) > 0.

%% @doc returns atom for naming recursive loop functions
get_loop_name(LoopName) -> list_to_atom("loop_"++LoopName).

