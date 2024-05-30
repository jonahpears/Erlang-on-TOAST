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
edge(#edge{from=_From,to=_To,edge_data=#edge_data{event_type = _EventType,event = Event,trans_type = TransType,pattern = _Pattern,args = _Args,guard = _Guard,code = _Code,attributes = _Attributes,comments = _Comments}=_EdgeData,is_silent=_IsSilent,is_delay = _IsDelay,is_custom_end = _IsCustomEnd,is_internal_timeout_to_supervisor = _IsInternalTimeoutToSupervisor}=_Edge) -> 

  % reng_show(edge, Edge, "\nbuilding edge snippet:\n"),

  {Act, Var} = Event,
  % StrAct = atom_to_list(Act),
  StrVar = atom_to_list(Var),
  case TransType of
    send ->
      Label = string:prefix(atom_to_list(Act), "send_"),
      % PayloadClause = merl_commented(pre, ["% replace 'ok' below with some payload"], ?Q(["Payload_"++StrVar++" = ok"])),
      % SendClause = merl_commented(pre, ["% "], ?Q(["CoParty ! {self(), "++StrVar++", Payload_"++StrVar++"}"])),
      PayloadClause = [%"%% replace 'ok' below with some payload \n",
      "Payload_"++StrVar++" = ok,"],
      SendClause = [%"%% send \n",
      "CoParty ! {self(), "++Label++", Payload_"++StrVar++"},"],
      % Clauses = [PayloadClause,SendClause],
      Clauses = PayloadClause++SendClause,
      {Clauses, []};
    recv -> 
      Label = string:prefix(atom_to_list(Act), "receive_"),
      % RecvClause = merl_commented(pre, ["% "], ?Q(["receive {CoParty, "++StrVar++" Payload_"++StrVar++"} -> "])),
      RecvClause = [%"%% recv \n",
      "receive {CoParty, "++Label++", Payload_"++StrVar++"} -> "],
      % Clauses = [RecvClause],
      Clauses = RecvClause,
      % PostClauses = [?Q(["end"])],
      PostClauses = ["end"],
      {Clauses, PostClauses};
    _ ->
      {[], []}
  end.

%% @doc generates snippets for a given state
%% if recursive state, 
%% @returns tuple of {list_of_state_funs, list_of_next_states}
state(standard_state=State, StateID, {ScopeID, ScopeName, ScopeData}=Scope, Edges, States, RecMap) ->

  ?GAP(),
  ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID},State]),
  ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID},StateID]),
  ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID},Scope]),
  ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID},States]),
  ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID},RecMap]),

  %% check if in main and needing to loop, so then will create new scope
  case ScopeID==-1 of
    true -> 
      {_StateName, EnterClause} = {ScopeName, ["(CoParty, Data) -> "]},
      % {_StateName, EnterClause} = {ScopeName, ["(CoParty, Data) -> %% (enter recursive loop from function.) \n"]},
      StateData = "Data"++integer_to_list(StateID),
      DataClause = [%" %% fresh Data for "++atom_to_list(_StateName)++": "++atom_to_list(State)++" \n ",
                    StateData++" = "++ScopeData++","];
    _ -> %% if first time entering scope, add state_enter to clauses
    StateData = ScopeData++"_"++integer_to_list(StateID),
    case StateID=:=ScopeID of
      true -> %% state at the top of scope
      {_StateName,EnterClause} = state_enter(State,StateID),
      DataClause = [%"%% fresh Data for "++atom_to_list(_StateName)++": "++atom_to_list(State)++" \n",
                    StateData++" = "++ScopeData++","];
      _ -> %% some state within scope
        {_StateName, EnterClause} = {ScopeName, []},%{ScopeName, ["(CoParty, Data) -> %% (note for developer: this is an odd path) \n"]},
      DataClause = [%"%% fresh Data for "++atom_to_list(_StateName)++": "++atom_to_list(State)++" \n",
                    StateData++" = "++ScopeData++", %% (fresh Data for entering "++atom_to_list(get_state_name(State,StateID))++".) \n"]
      end
  end,

  RelevantEdges = get_relevant_edges(StateID, Edges),
  % reng_show(edges,RelevantEdges,"\nrelevant edges:\n"),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  % ?SHOW("~p, ~p, passed assertion.\n",[Scope]),

  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  NextState = get_state_name(ToState, NextID),

  % StrNextID = integer_to_list(NextID),
  % StrStateID = integer_to_list(StateID),

  %% clause for fresh Data for this state
  % DataClause = merl_commented(pre, ["% fresh Data for state '@_StateName@': \''@State@'\'"], ?Q(["Data"++StrNextID++" = Data"++StrStateID])),
  %% only if current and next states are different
  % case NextID=/=StateID of
  %   true ->
      % DataClause = [%"%% fresh Data for state '@_StateName@': \''@State@'\' \n",
      %               "Data"++StrNextID++" = Data"++StrStateID++","],
  %       _ -> DataClause = []
  % end,

  %% get clause for action
  {EdgeClause, PostClauses} = edge(Edge),

  %% check if destination state is recursive and within different scope
  case is_state_recursive(StateID, RecMap) and (StateID=/=ScopeID) of
    true -> %% add to list of next states and add call to function to clauses
      LoopName = get_loop_name(atom_to_list(NextState)),
      ?SHOW("~p, ~p, new loop: ~p.",[Scope,{StateID},{StateID,LoopName}]),
      %% do not add {NextID,ScopeName} since these will be built within scope next time
      % NextStateFuns = [{StateID,LoopName}],
      % StateFunsTail = [],
      %% add function call to clauses
      % ScopeClause = merl_commented(pre, ["% enter recursive state with fresh data"], ?Q(["'@LoopName@'(CoParty, Data'@NextID@')"])),
      PostActionClause = [%"%% enter recursive state with fresh data \n",
      atom_to_list(LoopName)++"(CoParty, "++StateData++")"],
      % StateClauses = DataClause++EdgeClause++ScopeClause,
      %% build new scope
      _NextStateFuns = state(State, StateID, {StateID, LoopName, StateData}, Edges,States,RecMap);
    _ -> %% not a *new* recursive state, continue adding clauses to current state
      PostActionClause = [],
      %% next state funs is empty since we have no found a reason to start a new scope
      % NextStateFuns = [], %% {NextID,ScopeName}
      %% continue to next state
      ?SHOW("~p, ~p, continuing to next state: ~p.",[Scope,{StateID},{NextID,NextState,ToState}]),
      % {_StateFuns, _NextStateFuns} = state(NextState, NextID, Scope, Edges,States,RecMap),
      %% check if next state is recursive
      case is_state_recursive(NextID, RecMap) and (StateID=:=ScopeID) of
        true -> %% reconstruct function to go back
          _NextStateFuns = [{true,ScopeName,[%"%% recursive loop \n",
      atom_to_list(get_loop_name(NextState))++"(CoParty, "++StateData++")"]}];
        _ -> %% some other state to explore
          _NextStateFuns = state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
      end

      % %% check if any further statefuns were returned
      % case length(_NextStateFuns)==1 of
      %   true -> %% no other statefuns returned
      %     ?SHOW("~p, ~p, no further same-state clauses.",[Scope]),
      %     ?SHOW("~p, ~p, total (~p) next-state funs:\n\t~p.",[Scope,length(_NextStateFuns),_NextStateFuns]),
      %     NextClauses = [],
      %     StateFunsTail = [],
      %     %% if next-state funs, expand these now
      %     case length(_NextStateFuns)>0 of
      %       true -> %% expand these now
      %         NextStateFuns = _NextStateFuns;
      %       _ -> %% no next state funs
      %         NextStateFuns = []
      %     end;
      %   _ -> %% statefuns returned
      %     %% if within same scope (head) then extract clauses and add rest to tail
      %     [{true,_ScopeName,_NextClauses}|_StateFunsTail] = _StateFuns,
      %     ?SHOW("~p, ~p, total (~p) same-state clauses found:\n\t~p.",[Scope,length(_StateFuns),_StateFuns]),
      %     case _ScopeName=:=ScopeName of 
      %       true -> %% head is from same scope, add all clauses, add tail to tail
      %         NextClauses = _NextClauses,
      %         StateFunsTail = _StateFunsTail;
      %       _ -> %% nothing from new scope, add whole _StateFuns to tail
      %         NextClauses = [],
      %         StateFunsTail = _StateFuns
      %     end
      % end,




      %% any next states should be within the same scope
      % ?SHOW("ScopeName: ~p.", [ScopeName]),
      % ?SHOW("_ScopeName: ~p.", [_ScopeName]),
      % ?assert(ScopeName=:=_ScopeName),
      %% add nextclauses to end of current clauses
      % StateClauses = DataClause++EdgeClause++SameStateClauses
  end,

  ?SHOW("~p, ~p, _NextStateFuns:\n\t~p.", [Scope, {StateID},_NextStateFuns]),
  %% it must be at least more than 1, since we are not at end
  % case length(_NextStateFuns)>0 of
  %   true -> %% not the end of a recursive fold?
      %% check if head of next state funs is the same scope, and add clauses to 
      {_, HeadStateFunName, HeadStateFunClauses} = lists:nth(1,_NextStateFuns),
      ?SHOW("~p, ~p, ScopeName: ~p.",[Scope,{StateID},ScopeName]),
      ?SHOW("~p, ~p, HeadStateFunName: ~p.",[Scope,{StateID},HeadStateFunName]),
      ?GAP(),?SHOW("~p, ~p, HeadStateFunClauses:\n\t~p.",[Scope, {StateID},HeadStateFunClauses]),?GAP(),
      case HeadStateFunName=:=ScopeName of
        true -> %% same scope, should only be one clause, add clauses to our own (except first! which is another enter -- only if greater than 1 in length)
      % ?SHOW("~p, ~p, ~p, HeadStateFunClauses:\n\t~p.",[{StateID},Scope,HeadStateFunClauses]),
          % ?assert(is_list(HeadStateFunClauses)),
          ?assert(length(HeadStateFunClauses)==1),
          case is_list(lists:nth(1,lists:nth(1,HeadStateFunClauses))) of
            true ->
          % case length(HeadStateFunClauses)>1 of
            % true -> 
              _HeadStateFunClauses = lists:nthtail(1,lists:nth(1,HeadStateFunClauses));
            _ -> _HeadStateFunClauses = HeadStateFunClauses%lists:nthtail(1,HeadStateFunClauses)
          end,
          ?SHOW("~p, ~p, _HeadStateFunClauses:\n\t~p.",[Scope,{StateID},_HeadStateFunClauses]),
          StateClause = EnterClause++DataClause++EdgeClause++PostActionClause++_HeadStateFunClauses++PostClauses,
          NextStateFuns = lists:nthtail(1,_NextStateFuns);
        _ -> %% different scope, add all to state funs
          StateClause = EnterClause++DataClause++EdgeClause++PostActionClause++PostClauses,
          NextStateFuns = _NextStateFuns
      end,
    % _ -> %% the bottom of a recursive fold
    %   %% 



  % ?SHOW("~p, ~p, StateFunsTail: ~p.",[Scope,StateFunsTail]),
  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID},NextStateFuns]),

  StateClauses = [StateClause],
      ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),
  % ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,StateClauses]),
  % ?SHOW("~p, ~p, PostClauses:\n\t~p.",[Scope,PostClauses]),

  % ListClauses = []++_Clauses++StateClauses++PostClauses,
  % ?SHOW("~p, ~p, List Clauses:\n\t~p.",[Scope,ListClauses]),
  % ?SHOW("~p, ~p, ?Q Clauses:\n\t~p.",[Scope,?Q(ListClauses)]),

  %% check for any post clauses
  % case Signal of
    % post -> 
  % Clauses = merl_commented(pre, ["% state: "++StrStateID++" ."], ?Q(StateClauses)),
    % _ -> Clauses = merl_commented(pre, ["% state: "++StrStateID++" ."], ?Q(_Clauses++StateClauses))
  % end,
  % ?SHOW("~p, ~p, Clauses: ~p.",[Scope,Clauses]),
  StateFun = {true,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
  % {StateFuns++StateFunsTail, NextStateFuns};
%% 

%% @doc 
state(custom_end_state=State, StateID, {_ScopeID, ScopeName, ScopeData}=_Scope, Edges, __States, _RecMap) ->
  % {Signal, Fun, _NextState} = special_state(State, StateID, Edges),
  % ?assert(Signal=:=none),
  % {[Fun], []};
  % {[],[{StateID,State}]};
  {_, {_,FunName,_}=Fun, _} = special_state(State,StateID, Edges),
  [{true,ScopeName,[atom_to_list(FunName)++"(CoParty, "++ScopeData++")"]},Fun];
%% 

%% @doc 
state(State, _StateID, {_ScopeID, _ScopeName, _ScopeData}=Scope, _Edges, _States, _RecMap) ->
  % StateFuns = [],
  % NextStateFuns = [],

  ?SHOW("~p, unhandled: ~p. (return empty)", [Scope,State]),


  [].
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
  StrStateID = integer_to_list(StateID),

  % Clause = ?Q(["(CoParty) -> ","'@Name@'(CoParty, [])"])
  % Clause = ?Q(["(CoParty, Data) -> ok"]),
  Clause = "(CoParty, Data"++StrStateID++") -> ",

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
      "% @see "++atom_to_list(Name)++"/2."
    ],?Q([
      "(CoParty) -> ",
      atom_to_list(Name)++"(CoParty, [])"
    ])),

  Clause2 = merl_commented(pre, [
      "% @doc Called immediately after a successful initialisation.",
      "% Add any setup functionality here, such as for the contents of Data.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a list to store data inside to be used throughout the program."
    ],?Q([
      "(CoParty, Data) -> %% add any init/start preperations below, before entering "++atom_to_list(Main)++" \n",
      atom_to_list(Main)++"(CoParty, Data)"
    ])),

  ?SHOW("~p, Clause1:\n\t~p.",[_State,Clause1]),
  ?SHOW("~p, Clause2:\n\t~p.",[_State,Clause2]),

  % Clauses = [Clause1, Clause2],

  {next_state, [{true, Name, [Clause1]},{true,Name,[Clause2]}], {NextStateID, Main}};
%%

%% @doc 
%% @see state(custom_end_state, ...)
special_state(end_state=_State, StateID, Edges) -> special_state(custom_end_state, StateID, Edges);

%% @doc 
special_state(custom_end_state=_State, _StateID, _Edges) ->
  Name = stopping,

  %%
  % ClauseDefault = merl_commented(pre, [
  %     "% @doc Adds default reason 'normal' for stopping.",
  %     "% @see '@Name@'/3."
  %   ],?Q([
  %     "(CoParty, Data) -> ",
  %     "'@Name@'(normal, CoParty, Data)"
  %   ])),
  ClauseDefault = [
      "%% @doc Adds default reason 'normal' for stopping. \n",
      "%% @see "++atom_to_list(Name)++"/3. \n",
      "(CoParty, Data) -> ",
      atom_to_list(Name)++"(normal, CoParty, Data)"
    ],

  %%
  % ClauseNormal = merl_commented(pre, [
  %     "% @doc Adds default reason 'normal' for stopping.",
  %     "% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}."
  %   ],?Q([
  %     "(normal=Reason, _CoParty, _Data) -> ",
  %     "exit(normal)"
  %   ])),
  ClauseNormal = [
      "%% @doc Adds default reason 'normal' for stopping. \n",
      "%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}. \n",
      "(normal=Reason, _CoParty, _Data) -> ",
      "exit(normal)"
    ],

  %%
  % ClauseError = merl_commented(pre, [
  %     "% @doc stopping with error.",
  %     "% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.",
  %     "% @param CoParty is the process ID of the other party in this binary session.",
  %     "% @param Data is a list to store data inside to be used throughout the program."
  %   ],?Q([
  %     "({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> ",
  %       "erlang:error(Reason, Details)"
  %   ])),
  ClauseError = [
      "%% @doc stopping with error.\n",
      "%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.\n",
      "%% @param CoParty is the process ID of the other party in this binary session.\n",
      "%% @param Data is a list to store data inside to be used throughout the program.\n",
      "({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> ",
        "erlang:error(Reason, Details)"
    ],

  %%
  % ClausePartialError = merl_commented(pre, [
  %     "% @doc Adds default Details to error."
  %   ],?Q([
  %     "({error, Reason}, CoParty, Data) when is_atom(Reason) -> ",
  %     "'@Name@'({error, Reason, []}, CoParty, Data)"
  %   ])),
  ClausePartialError = [
      "%% @doc Adds default Details to error.\n",
      "({error, Reason}, CoParty, Data) when is_atom(Reason) -> ",
      atom_to_list(Name)++"({error, Reason, []}, CoParty, Data)"
    ],

  %%
  % ClauseUnknown = merl_commented(pre, [
  %     "% @doc stopping with Unexpected Reason."
  %   ],?Q([
  %     "(Reason, _CoParty, _Data) when is_atom(Reason) -> ",
  %     "exit(Reason)"
  %   ])),
  ClauseUnknown = [
      "%% @doc stopping with Unexpected Reason.\n",
      "(Reason, _CoParty, _Data) when is_atom(Reason) -> ",
      "exit(Reason)"
    ],

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
get_loop_name(LoopName) when is_atom(LoopName) -> list_to_atom("loop_"++atom_to_list(LoopName));
get_loop_name(LoopName) when is_list(LoopName) -> list_to_atom("loop_"++LoopName).

