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

%% @doc checks if _NextStateFuns head corresponds to current scope, and returns current state funs adn next state funs accordingly
resolve_clauses({_ScopeID, ScopeName, _ScopeData}=Scope,StateID,_NextStateFuns,EnterClause,DataClause,EdgeClause,PostActionClause,PostClauses) ->
  %% check if head of next state funs is the same scope, and add clauses to 
  {_, HeadStateFunName, HeadStateFunClauses} = lists:nth(1,_NextStateFuns),
  ?SHOW("~p, ~p, _NextStateFuns:\n\t~p.",[Scope,{StateID},_NextStateFuns]),
  ?SHOW("~p, ~p, HeadStateFunName: ~p.",[Scope,{StateID},HeadStateFunName]),
  ?GAP(),?SHOW("~p, ~p, HeadStateFunClauses:\n\t~p.",[Scope, {StateID},HeadStateFunClauses]),?GAP(),
  case HeadStateFunName=:=ScopeName of
    true -> %% same scope, should only be one clause, add clauses to our own (except first! which is another enter -- only if greater than 1 in length)
  % ?SHOW("~p, ~p, ~p, HeadStateFunClauses:\n\t~p.",[{StateID},Scope,HeadStateFunClauses]),
      % ?assert(is_list(HeadStateFunClauses)),
      ?assert(length(HeadStateFunClauses)==1),
      case is_list(lists:nth(1,lists:nth(1,HeadStateFunClauses))) of
        true -> 
          _HeadStateFunClauses = lists:nthtail(1,lists:nth(1,HeadStateFunClauses));
        _ -> 
          _HeadStateFunClauses = HeadStateFunClauses
      end,
      ?SHOW("~p, ~p, _HeadStateFunClauses:\n\t~p.",[Scope,{StateID},_HeadStateFunClauses]),
      StateClause = EnterClause++DataClause++EdgeClause++PostActionClause++_HeadStateFunClauses++PostClauses,
      NextStateFuns = lists:nthtail(1,_NextStateFuns);
    _ -> %% different scope, add all to state funs
      StateClause = EnterClause++DataClause++EdgeClause++PostActionClause++PostClauses,
      NextStateFuns = _NextStateFuns
  end,
  {StateClause,NextStateFuns}.

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

%% @doc returns timer name
timer_name(Name) when is_list(Name) -> "timer_"++Name;
timer_name(Name) when is_atom(Name) -> "timer_"++atom_to_list(Name).

%% @doc generates clause for actions/outgoing edges

%% @doc for defining new timers and adding them to data
edge(#edge{edge_data=#edge_data{timer=#{duration:=Duration,name:=Name}},is_timer=true,is_delay=false}, {StateData,NextStateData}) ->
  EdgeClause = NextStateData++" = set_timer("++timer_name(Name)++", "++integer_to_list(Duration)++", "++StateData++"), ",
  {[EdgeClause], []};

%% @doc for delay (via timer)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Timer}},is_delay=true,is_timer=false}, {StateData,_NextStateData}) when is_list(Timer) ->
  ?SHOW("timer ref: ~p.",[Timer]),
  EdgeClause = "% (delay until "++timer_name(Timer)++" completes, error if does not exist) \n case get_timer("++timer_name(Timer)++", "++StateData++") of {ok, TID_"++Timer++"} -> receive {timeout, TID_"++Timer++", {timer, "++Timer++"}} -> ok end; {ko, no_such_timer} -> error(no_such_timer) end,",
  {[EdgeClause], []};

%% @doc for delay (via delay)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Delay}},is_delay=true,is_timer=false}, {_StateData,_NextStateData}) when is_number(Delay) ->
  EdgeClause = "timer:sleep("++integer_to_list(floor(Delay))++"),",
  {[EdgeClause], []};

edge(#edge{from=_From,to=_To,edge_data=#edge_data{event_type = _EventType,event = Event,trans_type = TransType,pattern = _Pattern,args = _Args,guard = _Guard,code = _Code,attributes = _Attributes,comments = _Comments}=_EdgeData,is_silent=_IsSilent,is_delay = _IsDelay,is_timer = _IsTimer, is_custom_end = _IsCustomEnd,is_internal_timeout_to_supervisor = _IsInternalTimeoutToSupervisor}=_Edge, {StateData,NextStateData}) -> 

  % reng_show(edge, Edge, "\nbuilding edge snippet:\n"),

  {Act, Var} = Event,
  % StrAct = atom_to_list(Act),
  StrVar = atom_to_list(Var),
  StrPayload = "Payload_"++StrVar,
  case TransType of
    send ->
      Label = string:prefix(atom_to_list(Act), "send_"),
      PayloadClause = [%"%% replace 'ok' below with some payload \n",
      StrPayload++" = \'payload\',"],
      SendClause = [%"%% send \n",
      "CoParty ! {self(), "++Label++", Payload_"++StrVar++"},"],
      Clauses = PayloadClause++SendClause,
      {Clauses, []};
    recv -> 
      Label = string:prefix(atom_to_list(Act), "receive_"),
      RecvClause = [%"%% recv \n",
      "receive {CoParty, "++Label++", "++StrPayload++"} -> "],
      DataClause = [ NextStateData++" = save_msg("++Label++", "++StrPayload++", "++StateData++"), " ],
      Clauses = RecvClause++DataClause,
      PostClauses = ["end"],
      {Clauses, PostClauses};
    _ ->
      {[], []}
  end.
%%


%% @docs enter clauses
enter_clause(_State, _StateID, {ScopeID, _ScopeName, _ScopeData}=_Scope) 
  when ScopeID==-1 -> ["(CoParty, Data) ->"];
enter_clause(State, StateID, {ScopeID, _ScopeName, _ScopeData}=_Scope) 
  when StateID=:=ScopeID -> 
    {_StateName, EnterClause} = state_enter(State,StateID),
    EnterClause;
enter_clause(_State, _StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope) -> [].

%% @docs data clauses
data_clause(_State, StateID, {ScopeID, _ScopeName, ScopeData}=Scope)
  when ScopeID==-1 -> 
    {StateData, _} = next_state_data(StateID, Scope),
    DataClause = [ StateData++" = "++ScopeData++","% %% (fresh Data for entering "++atom_to_list(get_state_name(State,StateID))++": "++atom_to_list(State)++") \n"
    ],
    {StateData, DataClause};
data_clause(_State, StateID, {ScopeID, _ScopeName, ScopeData}=Scope) 
  when StateID=:=ScopeID -> 
    {StateData, _} = next_state_data(StateID, Scope),
    DataClause = [ ScopeData++"_"++integer_to_list(StateID)++" = "++ScopeData++","% %% (fresh Data for entering "++atom_to_list(get_state_name(State,StateID))++": "++atom_to_list(State)++") \n"
    ],
    {StateData, DataClause};
data_clause(State, StateID, {_ScopeID, _ScopeName, ScopeData}=Scope) -> 
  {StateData, _} = next_state_data(StateID, Scope),
  DataClause = [ ScopeData++"_"++integer_to_list(StateID)++" = "++ScopeData++", %% (fresh Data for entering "++atom_to_list(get_state_name(State,StateID))++".) \n"],
  {StateData, DataClause}.

%% @doc returns state data incrememnted
%% second elem of tuple indicates the integer suffix at the end denoting the state within the scope.
%% if second elem is -1, then this is a fresh scope
next_state_data(StateID, {ScopeID, _ScopeName, _ScopeData}=_Scope) 
  when ScopeID==-1 -> {"Data"++integer_to_list(StateID), -1};
next_state_data(StateID, {_ScopeID, _ScopeName, ScopeData}=_Scope) -> 
  {ScopeData++"_"++integer_to_list(StateID), StateID}.


state(delay_state=State, StateID, {ScopeID, ScopeName, ScopeData}=Scope, Edges, States, RecMap) ->
  
  ?GAP(),
  ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID},State]),
  ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID},StateID]),
  ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID},Scope]),
  ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID},States]),
  ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID},RecMap]),

  %% 
  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  NextState = get_state_name(ToState, NextID),

  EnterClause = enter_clause(State, StateID, Scope),

  %% since starting new timer, no need for new statedata
  StateData=ScopeData,

  %% get clause for action
  {EdgeClause, _} = edge(Edge, {ScopeData,StateData}),

  %% TODO FROM HERE

  %% check if next state is recursive
  case is_state_recursive(NextID, RecMap) and (StateID=:=ScopeID) of
    true -> %% reconstruct function to go back
      _NextStateFuns = [{true,ScopeName,[%"%% recursive loop \n",
  atom_to_list(get_loop_name(NextState))++"(CoParty, "++StateData++")"]}];
    _ -> %% some other state to explore
      _NextStateFuns = state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
  end,
  
  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,_NextStateFuns,EnterClause,[],EdgeClause,[],[]),

  StateClauses = [StateClause],

  ?SHOW("~p, ~p, EnterClause:\n\t~p.",[Scope,{StateID},EnterClause]),
  ?SHOW("~p, ~p, EdgeClause:\n\t~p.",[Scope,{StateID},EdgeClause]),
  ?SHOW("~p, ~p, StateClause:\n\t~p.",[Scope,{StateID},StateClause]),
  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID},NextStateFuns]),


  %% prepare to return state funs
  StateFun = {true,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;

%%




%% @doc generates snippet for delay state
%% timers must be stored in the map Data 
state(timer_start_state=State, StateID, {ScopeID, ScopeName, ScopeData}=Scope, Edges, States, RecMap) ->
  
  ?GAP(),
  ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID},State]),
  ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID},StateID]),
  ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID},Scope]),
  ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID},States]),
  ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID},RecMap]),

  %% 
  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  NextState = get_state_name(ToState, NextID),

  EnterClause = enter_clause(State, StateID, Scope),

  %% since starting new timer, use custom StateData (no data clause)
  {StateData, _} = next_state_data(StateID, Scope),

  %% get clause for action
  {EdgeClause, _} = edge(Edge, {ScopeData,StateData}),

  %% check if next state is recursive
  case is_state_recursive(NextID, RecMap) and (StateID=:=ScopeID) of
    true -> %% reconstruct function to go back
      _NextStateFuns = [{true,ScopeName,[%"%% recursive loop \n",
  atom_to_list(get_loop_name(NextState))++"(CoParty, "++StateData++")"]}];
    _ -> %% some other state to explore
      _NextStateFuns = state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
  end,
  
  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,_NextStateFuns,EnterClause,[],EdgeClause,[],[]),

  StateClauses = [StateClause],

  ?SHOW("~p, ~p, EnterClause:\n\t~p.",[Scope,{StateID},EnterClause]),
  ?SHOW("~p, ~p, EdgeClause:\n\t~p.",[Scope,{StateID},EdgeClause]),
  ?SHOW("~p, ~p, StateClause:\n\t~p.",[Scope,{StateID},StateClause]),
  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID},NextStateFuns]),


  %% prepare to return state funs
  StateFun = {true,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;

%%


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


  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),

  %% if send, still need data clause
  case Edge#edge.edge_data#edge_data.trans_type=:=send of
    true -> 
      % StateData=ScopeData,DataClause=[];
      {StateData, DataClause} = data_clause(State, StateID, Scope);
    _ ->
      % {_StateData, DataClause} = data_clause(State, StateID, Scope),
      {StateData, _} = next_state_data(StateID, Scope),
      DataClause = []
  end,


  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  NextState = get_state_name(ToState, NextID),

  %% get clause for action
  {EdgeClause, PostClauses} = edge(Edge, {ScopeData, StateData}),

  %% check if destination state is recursive and within different scope
  case is_state_recursive(StateID, RecMap) and (StateID=/=ScopeID) of
    true -> %% add to list of next states and add call to function to clauses
      LoopName = get_loop_name(atom_to_list(NextState)),
      ?SHOW("~p, ~p, new loop: ~p.",[Scope,{StateID},{StateID,LoopName}]),
      %% do not add {NextID,ScopeName} since these will be built within scope next time
      LoopCall = "(CoParty, "++StateData++")",
      % LoopDef = "(CoParty, "++StateData++")",
      %% add function call to clauses
      PostActionClause = [%"%% enter recursive state with fresh data \n",
      atom_to_list(LoopName)++LoopCall],
      %% build new scope
      % _NextStateFuns = [{true,LoopName,[LoopCall]}];
      _NextStateFuns=state(State, StateID, {StateID, LoopName, StateData}, Edges,States,RecMap);
    _ -> %% not a *new* recursive state, continue adding clauses to current state
      PostActionClause = [],
      %% continue to next state
      ?SHOW("~p, ~p, continuing to next state: ~p.",[Scope,{StateID},{NextID,NextState,ToState}]),
      %% check if next state is recursive
      case is_state_recursive(NextID, RecMap) and (StateID=:=ScopeID) of
        true -> %% reconstruct function to go back
          _NextStateFuns = [{true,ScopeName,[%"%% recursive loop \n",
      atom_to_list(get_loop_name(NextState))++"(CoParty, "++StateData++")"]}];
        _ -> %% some other state to explore
          _NextStateFuns = state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
      end

  end,

  ?SHOW("~p, ~p, _NextStateFuns:\n\t~p.", [Scope, {StateID},_NextStateFuns]),

  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,_NextStateFuns,EnterClause,DataClause,EdgeClause,PostActionClause,PostClauses),


  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID},NextStateFuns]),

  StateClauses = [StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  StateFun = {true,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
  % {StateFuns++StateFunsTail, NextStateFuns};
%% 

%% @doc 
state(custom_end_state=State, StateID, {_ScopeID, ScopeName, ScopeData}=_Scope, Edges, __States, _RecMap) ->
  % {_, {_,FunName,_}=Fun, _} = special_state(State,StateID, Edges),
  % [{true,ScopeName,[atom_to_list(FunName)++"(CoParty, "++ScopeData++")"]},Fun];
  {_, Funs, _} = special_state(State,StateID, Edges),
  Fun=lists:nth(1,Funs),
  {_,FunName,_}=Fun,
  [{true,ScopeName,[atom_to_list(FunName)++"(CoParty, "++ScopeData++")"]}]++Funs;
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
      "(normal=_Reason, _CoParty, _Data) -> ",
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

  %% clausedefault uses different multi-despatch
  Clauses = [ClauseNormal, ClauseError, ClausePartialError, ClauseUnknown],

  {none, [{true, Name, [ClauseDefault]},{true,Name,Clauses}], ok};
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

