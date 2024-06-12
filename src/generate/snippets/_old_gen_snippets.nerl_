%% @doc Erlang snippets used in stub generation
-module(gen_snippets).
-compile({nowarn_unused_function, [ {state_name,2} ]}).

-define(EXPORT_DEFAULT, false).

-export([ special_state/3,
          state/7
          % clauses_enter/4,
          % state_clauses/4,
          % edge_clauses/0
        ]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").
-include("stub_tools.hrl").
-include("fsm_tools.hrl").

%% specific functionality
-include("edge_snippets.hrl").
-include("get_if_edges.hrl").


%%%%%%%
%%%%%%% helper functions
%%%%%%%


%% @doc takes a list of edges and returns a tuple of two distinct lists containing action edges and silent edges.
separate_edges(Edges) ->
  lists:foldl(fun(Edge, {Action, Silent}=AccIn) -> 
    case is_edge_silent(Edge) of
      true -> {Action, Silent++[Edge]};
      _ -> {Action++[Edge], Silent}
    end end, {[],[]}, Edges).
%%

%% @doc extracts timer ref from if_timer edge_data to list form
get_if_timer(#{is_timer:=IsTimer,ref:=Ref,to:=_To}) ->
  case IsTimer of
    true -> Ref;
    _ -> atom_to_list(Ref)
  end.
%%

%% @doc function tuple for some function to return payload
get_state_payload_fun(_NonSilentEdges, _State, StateID, _Scope, _Edges, _States, _RecMap) -> 
  FunName = "get_state"++integer_to_list(StateID)++"_payload",
  PayloadArgs = [],
  {{false,list_to_atom(FunName),[["() -> ok"]]}, PayloadArgs}.
%%


%% @doc function tuple for some function to determine selection
get_state_selection_fun([H|_T]=_NonSilentEdges, _State, StateID, _Scope, _Edges, _States, _RecMap) -> 
  FunName = "select_state"++integer_to_list(StateID),
  SelectionArgs = [],
  {{false,list_to_atom(FunName),[["([]) -> "++get_msg_label(H)],["(_Selection) -> expand_this_stub"]]}, SelectionArgs}.
%%

%% @doc 
get_state_selection_fun_name(StateID) -> 
  FunName = "select_state"++integer_to_list(StateID),
  list_to_atom(FunName).
%%

%% @doc 
get_state_payload_fun_name(StateID) -> 
  FunName = "get_state_"++integer_to_list(StateID)++"_payload",
  list_to_atom(FunName).
%%


%% @doc adds any nextstatefun tuples from ToMerge to Primary that are not already in Primary
%% @returns list of statefun triples merged from Primary and ToMerge
merge_next_state_funs(Primary, ToMerge) -> 
  %% get names of next state funs from r
  NextStateFunNames = lists:foldl(fun({_,Name,_}=_Elem,AccIn) when is_atom(Name) -> 
    case lists:member(Name,AccIn) of 
      true -> AccIn; 
      _ -> AccIn++[Name] 
    end 
  end, [], Primary),

  %% merge nextstatefuns
  NextStateFuns = lists:foldl(fun({_,T_Name,_}=Elem,AccIn) -> 
      case lists:member(T_Name,NextStateFunNames) of
        true -> AccIn;
        _ -> AccIn++[Elem]
      end
    end, Primary, ToMerge),

  NextStateFuns.
%%

%% @doc concats fields to Primary from ToMerge 
merge_fun_maps(Primary, ToMerge) ->
  Result = maps:fold(fun(K,V,AccIn) -> 
    PrimaryValue = maps:get(K,Primary,[]),
    maps:put(K,PrimaryValue++V,AccIn)
  end,Primary,ToMerge),
  Result.
%%

%% @doc adds fields to Primary from ToMerge that do not already exist there
update_fun_maps(Primary, ToMerge) ->
  % ?SHOW("\nPrimary:\n\t~p,\nToMerge:\n\t~p.\n",[Primary,ToMerge]),
  Result = maps:fold(fun(K,V,AccIn) -> 
    PrimaryValue = maps:get(K,Primary,[]),
    case PrimaryValue of
      [] -> maps:put(K,V,AccIn);
      _ -> AccIn
    end
  end,Primary,ToMerge),
  Result.
%%

%%%%%%%
%%%%%%% wrapper naming functions
%%%%%%%


%% @doc returns atom for naming recursive loop functions
loop_name(LoopName) when is_atom(LoopName) -> loop_name(atom_to_list(LoopName));
%%
loop_name(LoopName) when is_list(LoopName) -> list_to_atom("loop_"++LoopName).
%%


%% @doc returns timer name
timer_name(Name) when is_list(Name) -> "timer_"++Name;
%%
timer_name(Name) when is_atom(Name) -> "timer_"++atom_to_list(Name).
%%


%% @doc returns state data incrememnted
%% second elem of tuple indicates the integer suffix at the end denoting the state within the scope.
%% if second elem is -1, thenresolve_edge_clause this is a fresh scope
next_state_data(StateID, ScopeID, _ScopeData) 
when ScopeID==-1 -> 
  "Data"++integer_to_list(StateID);
%%
next_state_data(StateID, _ScopeID, ScopeData) -> 
  ScopeData++"_"++integer_to_list(StateID).
%%



%% @doc returns the 
% get_next_state_trans(To, NextID) when is_atom(To) and is_integer(NextID) ->
%   case To of 
%     end_state -> {stop, normal};
%     _ ->
%       NextState = state_name(To, NextID),
%       case To of 
%         custom_end_state -> {next_state, NextState};
%         standard_state -> {next_state, NextState};
%         choice_state -> {next_state, NextState};
%         recv_after_state -> {next_state, NextState};
%         branch_after_state -> {next_state, NextState};
%         send_after_state -> {next_state, NextState};
%         select_after_state -> {next_state, NextState};
%         after_state -> {next_state, NextState};
%         fatal_timeout_state -> {next_state, NextState};
%         _Else -> {next_state, NextState}
%       end
%   end.
% %%


%%%%%%%
%%%%%%% boolean conditions
%%%%%%%


%%%%%%%
%%%%%%% clauses
%%%%%%%

%% @returns name of Data variable depending on stateID and ScopeID
%% if scopeID==-1, then this is main, and just use default Data
state_data(_StateID, _ScopeID) 
when _ScopeID==-1 -> "Data";
%% if stateID==ScopeID then we are in fresh scope, so use stateID
state_data(StateID, _ScopeID) 
when StateID==_ScopeID -> "Data"++integer_to_list(StateID);
%% if stateID=/=ScopeID then use scopeID and stateid suffix
state_data(StateID, ScopeID) -> 
  "Data"++integer_to_list(ScopeID)++"_"++integer_to_list(StateID).
%%

%% @returns clause for entering new scope
clauses_enter(StateID, ScopeID) -> 
  ["(CoParty, "++state_data(StateID, ScopeID)++") ->"].
%%

%% @returns tuple containing clause of creating fresh data for new state and name of next data
data_clause(StateID, ScopeID, ScopeData) ->
  NextData = next_state_data(StateID,ScopeID, ScopeData),
  DataClause = NextData++" = "++ScopeData++",",
  {[DataClause],NextData}.
%%


%% TODO:: look into re-doing the below, incorporating it into whatever functionality goes to check out the next state

%% @doc for adding function calls (recursive loops) to the end of clauses (after action)
%% only communicating, recursive state are likely to return anything non-empty.
clauses_post_action(true=_IsRecursive, standard_state=_State, NextState, StateData) -> 
  [atom_to_list(loop_name(atom_to_list(NextState)))++"(CoParty, "++StateData++")"];
clauses_post_action(false=_IsRecursive, standard_state=_State, _NextState, _StateData) -> [];
clauses_post_action(_IsRecursive, _State, _NextState, _StateData) -> [].
%%


%% @doc determines how the stub is built.
%% if next current state is recursive, starts a new scope and returns it as list
next_state_funs(true=_IsRecursive, standard_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {ToState, NextID, StateData}=_NextData) ->
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(), 
  state(State, StateID, {StateID, loop_name(atom_to_list(state_name(ToState, NextID))), StateData}, Edges,States,RecMap);
%%

%% @doc
next_state_funs(true=_IsRecursive, if_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {_ToState, _NextID, StateData}=_NextData) -> 
  % timer:sleep(500),
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(),
  S = state(State, StateID, {StateID, loop_name(atom_to_list(state_name(State, StateID))), StateData}, Edges,States,RecMap),
  ?GAP(),?SHOW("~p,\n\t~p, ~p,\n\tS: ~p.",[_Scope,{StateID,State},_NextData,S]),
  ?GAP(),
  S;


%% @doc
next_state_funs(true=_IsRecursive, if_then_else_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {_ToState, _NextID, StateData}=_NextData) -> 
  % timer:sleep(500),
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(),
  S = state(State, StateID, {StateID, loop_name(atom_to_list(state_name(State, StateID))), StateData}, Edges,States,RecMap),
  ?GAP(),?SHOW("~p,\n\t~p, ~p,\n\tS: ~p.",[_Scope,{StateID,State},_NextData,S]),
  ?GAP(),
  S;
%%

%% @doc 
next_state_funs(false=_IsRecursive, _State, _StateID, {ScopeID, ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {ToState, NextID, StateData}=_NextData) -> 
  ?GAP(),?SHOW("~p, not recursive\n\t~p, ~p.",[_Scope,{_StateID,_State},_NextData]),?GAP(),
  %% check if next state is recursive 
  case is_state_recursive(NextID, RecMap) and (NextID=:=ScopeID) of %% and(StateID=:=ScopeID) of
    true -> %% reconstruct loop name to go back
      Return=[{?EXPORT_DEFAULT,ScopeName,[atom_to_list(loop_name(state_name(ToState, NextID)))++"(CoParty,"++StateData++")"]}],
      ?SHOW("~p, ~p, not recursive return:\n\t~p.",[_Scope,{_StateID,_State},Return]),
      Return;
    _ -> %% some other state to explore
      state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
  end.
%%




%%%%%%%%%% todo
%% todo
%% todo :: fix issue with recursive if_state/if_then_else_state
%% todo :: test the above with selection/branch/after etc
%% todo
%% todo :: look into re-doing the clauses_post_action, incorporating it into whatever functionality goes to check out the next state
%% todo
%% todo
%% todo
%% todo


%%%%% todo::
%% todo :: resolve the following temporary wrapper funs
clauses_enter(_State, StateID, {ScopeID, _ScopeName, _ScopeData}) -> clauses_enter(StateID, ScopeID).








%% @doc checks if _NextStateFuns head corresponds to current scope, and returns current state funs adn next state funs accordingly
resolve_clauses({_ScopeID, ScopeName, _ScopeData}=Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause,PostClauses) ->
  %% check if head of next state funs is the same scope, and add clauses to 
  case length(LaterStateFuns)==0 of
    true -> StateClause = EnterClause++DataClause++EdgeClause++PostClauses,
    NextStateFuns = LaterStateFuns;
  _ ->
  ?SHOW("~p, ~p, LaterStateFuns:\n\t~p.",[Scope,{StateID},LaterStateFuns]),
  {_, HeadStateFunName, HeadStateFunClauses} = lists:nth(1,LaterStateFuns),
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
      StateClause = EnterClause++DataClause++EdgeClause++_HeadStateFunClauses++PostClauses,
      NextStateFuns = lists:nthtail(1,LaterStateFuns);
    _ -> %% different scope, add all to state funs
      StateClause = EnterClause++DataClause++EdgeClause++PostClauses,
      NextStateFuns = LaterStateFuns
  end
end,
  {StateClause,NextStateFuns}.

%% @doc combo of edge andresolve clause functions
resolve_edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap) ->
  %% get edge clause
  {_DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, _PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),
  %% get resolved
  {ElemStateClause,ElemNextStateFuns} = resolve_clauses(Scope,StateID,[],LaterStateFuns,[],EdgeClause,PostEdgeClause),
  {ElemStateClause,ElemNextStateFuns}.

%% @doc if clauses
if_clauses([_H|_T]=Es, State, StateID, Scope, Edges, States, RecMap) when is_list(Es) ->

  %% separate into relating to timer reaching 0 or not
  {_First,_Second} = get_if_edges(Es),
  {F,First} = _First,
  {S,Second} = _Second,
  %% make sure both not undefined
  ?assert(First=/=Second),

  %% 
  case First of
    undefined -> %% get timer from second
      AlreadyUnfolded=false,
      Timer = get_if_timer(Second),
      FirstNextStateFuns = [],
      FirstClause = ["receive {timeout, "++timer_name(Timer)++", {timer, "++timer_name(Timer)++"}} -> error(upper_bound_violation) "];
    _ -> %% 
      Timer = get_if_timer(First),
      ClauseStart = ["receive {timeout, "++timer_name(Timer)++", {timer, "++timer_name(Timer)++"}} -> "],
      {FStateClause,FirstNextStateFuns} = resolve_edge_clause(F, State, StateID, Scope, Edges, States, RecMap),

      %% check if first or second loop
      case length(FStateClause)==0 of
        true -> %% just take this one
          AlreadyUnfolded=true,
          {_,_FLoopScope,FClauses} = lists:nth(1,FirstNextStateFuns),
          FirstClause = lists:nthtail(1,lists:nth(1,FClauses));
        _ -> 
          AlreadyUnfolded=false,
          FirstClause = ClauseStart ++ FStateClause
      end
    
      % ?GAP(),?SHOW("~p, ~p,\n\tf stateclause:\n\t\t~p.",[Scope,{StateID,State},FStateClause]),?GAP(),
      % ?GAP(),?SHOW("~p, ~p,\n\tf next funs:\n\t\t~p.",[Scope,{StateID,State},FirstNextStateFuns]),?GAP(),

      
  end,


  %% 
  case Second of
    undefined -> %% get timer from second
      SecondNextStateFuns=[],
      case AlreadyUnfolded of
        true -> SecondClause=[];
        _ -> SecondClause = ["after 0 -> error(urgent_lower_bound_violation) end "]
      end;
    _ -> %% 
      {SStateClause,SecondNextStateFuns} = resolve_edge_clause(S, State, StateID, Scope, Edges, States, RecMap),

      case not AlreadyUnfolded of
        true -> ?SHOW("1\n\n\n",[]),
          %% check if first or second loop
          case length(SStateClause)==0 of
            true -> %% just take this one
              ?SHOW("2\n\n\n",[]),
              {_,_SLoopScope,SClauses} = lists:nth(1,SecondNextStateFuns),
              SecondClause = lists:nthtail(1,lists:nth(1,SClauses));
            _ -> 
              ?SHOW("3\n\n\n",[]),
              SecondClause = ["after 0 -> "] ++ SStateClause ++["end"]
          end;
        _ -> ?SHOW("4\n\n\n",[]),
          SecondClause=[]
      end

      % ?GAP(),?SHOW("~p, ~p,\n\tS stateclause:\n\t\t~p.",[Scope,{StateID,State},SStateClause]),?GAP(),
      % ?GAP(),?SHOW("~p, ~p,\n\tS next funs:\n\t\t~p.",[Scope,{StateID,State},SecondNextStateFuns]),?GAP(),


      
  end,

  NextStateFuns = merge_next_state_funs(FirstNextStateFuns, SecondNextStateFuns),

  StateClauses=FirstClause++SecondClause,


  {StateClauses,NextStateFuns}.


%% @doc returns resolved clauses for all Es
select_clauses([_H|_T]=_Es, State, StateID, Scope, Edges, States, RecMap) when is_list(_Es) ->

  %% separate to order (should be ordered anyway, but just to make sure)
  SilentEdges = lists:filter(fun is_edge_silent/1, _Es),
  NonSilentEdges = lists:filter(fun is_edge_not_silent/1, _Es),
  % Es = NonSilentEdges ++ SilentEdges,

  ?assert(length(SilentEdges)=<1),

  %% get labels of each msg selection
  % NonSilentLabels = list:foldl(fun(E, AccIn) -> AccIn++[get_msg_label(E)] end, [], NonSilentEdges),


  ResolveClauses = fun(Edge, {_StateClause,_NextStateFuns}=_AccIn) ->
    {ElemStateClause,ElemNextStateFuns} = resolve_edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),
    ?GAP(),
    ?SHOW("~p, ~p, _StateClause:\n\t~p.",[Scope,{StateID},_StateClause]),
    ?GAP(),
    ?SHOW("~p, ~p, ElemStateClause:\n\t~p.",[Scope,{StateID},ElemStateClause]),
    ?GAP(),
    ?SHOW("~p, ~p, ElemNextStateFuns:\n\t~p.",[Scope,{StateID},ElemNextStateFuns]),
    ?GAP(),
    %% add resolved to StateClause 
    % case is_edge_not_silent(Edge) and (length(_StateClause)>0) of
    %   true -> StateClause = _StateClause ++ [";"] ++ ElemStateClause; %% silent edges take this into account by default
    %   false -> StateClause = _StateClause ++ElemStateClause
    % end,
    %% add resolved to map against msg label
    StateClauseMap = maps:put(get_msg_label(Edge), ElemStateClause, _StateClause),
    %% merge nextstatefuns
    NextStateFuns = merge_next_state_funs(_NextStateFuns,ElemNextStateFuns),
    %% return 
    {StateClauseMap,NextStateFuns}
  end,

  %% resolve each in NonSilentEdges only (handle Silent Edges differently)
  {EsStateClauseMap,EsNextStateFuns} = lists:foldl(ResolveClauses, {#{},[]}, NonSilentEdges),
  

  %% get each of the cases for selection (post-AwaitSelection returning ok)
  MsgSelectionClause = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of
    true -> [K++" -> "]++V;
  _ -> AccIn++["; "++K++" -> "]++V end end, [], EsStateClauseMap),

  _SelectionFun = get_state_selection_fun(NonSilentEdges, State, StateID, Scope, Edges, States, RecMap),
  {SelectionFun,SelectionArgs} = _SelectionFun,
  {_, SelectionFunName, _} = SelectionFun,

  _PayloadFun = get_state_payload_fun(NonSilentEdges, State, StateID, Scope, Edges, States, RecMap),
  {PayloadFun,PayloadArgs} = _PayloadFun,
  {_, PayloadFunName, _} = PayloadFun,



  case length(SilentEdges)==1 of
    true -> 
      Timeout = lists:nth(1,SilentEdges),
      _TimeoutRef = maps:get(ref,Timeout#edge.edge_data#edge_data.timeout),
      case is_number(_TimeoutRef) of
        true -> TimeoutRef = integer_to_list(floor(_TimeoutRef));
        _ -> TimeoutRef = _TimeoutRef
      end,
      {_SilentClause,SilentEsNextStateFuns} = resolve_edge_clause(Timeout, State, StateID, Scope, Edges, States, RecMap),
      %% remove front from _silentclause (which will either be 'after Timeout -> ' or '{timeout,...}')
      SilentClause = lists:nthtail(1,_SilentClause),
      
      ?GAP(),
      ?SHOW("~p, ~p, SilentClause:\n\t~p.",[Scope,{StateID},SilentClause]),
      
      NextStateFuns = merge_next_state_funs(EsNextStateFuns, SilentEsNextStateFuns),
      
      StateClause = ["AwaitSelection = nonblocking_selection(fun "++atom_to_list(SelectionFunName)++"/1, ["++SelectionArgs++"], self(), "++TimeoutRef++"), \n receive {AwaitSelection, ok, {Label, Payload}} -> case Label of"]++MsgSelectionClause++["; _ -> error(unexpected_label_selected) end; {AwaitPayload, ko} -> "]++SilentClause++["end"]  ;

    _ -> NextStateFuns = EsNextStateFuns,
      StateClause = ["case {Label,Payload}="++atom_to_list(PayloadFunName)++"(["++PayloadArgs++"]) of"]++MsgSelectionClause++["; _ -> error(unexpected_label_selected)"]++["end"] 
  end,

  ?GAP(),
  ?SHOW("~p, ~p, EsStateClauseMap:\n\t~p.",[Scope,{StateID},EsStateClauseMap]),
  ?GAP(),
  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID},NextStateFuns]),
  ?GAP(),

  {StateClause,NextStateFuns++[SelectionFun,PayloadFun]}.
%%

%% @doc branch_clauses edge clauses (list of edges)
%% @returns resovled_clauses for all _Es
branch_clauses([_H|_T]=_Es, State, StateID, Scope, Edges, States, RecMap) when is_list(_Es) ->

  %% separate to order (should be ordered anyway, but just to make sure)
  SilentEdges = lists:filter(fun is_edge_silent/1, _Es),
  NonSilentEdges = lists:filter(fun is_edge_not_silent/1, _Es),
  Es = NonSilentEdges ++ SilentEdges,

  ?SHOW("~p, ~p, silent edges:\n\t~p.",[Scope,{StateID,State},SilentEdges]),
  ?SHOW("~p, ~p, non silent edges:\n\t~p.",[Scope,{StateID,State},NonSilentEdges]),

  % timer:sleep(1500),

  ResolveClauses = fun(Edge, {_StateClause,_NextStateFuns}=_AccIn) ->
    {ElemStateClause,ElemNextStateFuns} = resolve_edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),
    ?GAP(),
    ?SHOW("~p, ~p, _StateClause:\n\t~p.",[Scope,{StateID},_StateClause]),
    ?GAP(),
    ?SHOW("~p, ~p, ElemStateClause:\n\t~p.",[Scope,{StateID},ElemStateClause]),
    ?GAP(),
    ?SHOW("~p, ~p, ElemNextStateFuns:\n\t~p.",[Scope,{StateID},ElemNextStateFuns]),
    ?GAP(),
    %% add resolved to StateClause 
    case is_edge_not_silent(Edge) and (length(_StateClause)>0) of
      true -> StateClause = _StateClause ++ [";"] ++ ElemStateClause; %% silent edges take this into account by default
      false -> StateClause = _StateClause ++ElemStateClause
    end,
    %% merge nextstatefuns
    NextStateFuns = merge_next_state_funs(_NextStateFuns,ElemNextStateFuns),
    %% return 
    {StateClause,NextStateFuns}
  end,

  %% resolve each in Es
  {EsStateClause,EsNextStateFuns} = lists:foldl(ResolveClauses, {[],[]}, Es),

  ?GAP(),
  ?SHOW("~p, ~p, EsStateClause:\n\t~p.",[Scope,{StateID},EsStateClause]),
  ?GAP(),
  ?SHOW("~p, ~p, EsNextStateFuns:\n\t~p.",[Scope,{StateID},EsNextStateFuns]),
  ?GAP(),

  StateClause = ["receive"]++EsStateClause++["end"],

  % timer:sleep(5000),

  {StateClause,EsNextStateFuns}.
%%

%% @doc return snippet for single edge
clauses_edge(Edge, State, StateID, {ScopeName, ScopeID}, Edges, States, RecMap, FunMap) ->

  %% get next state/id
  NextStateID = Edge#edge.to,
  NextState = maps:get(NextStateID, States),
  ?SHOW("\nfrom (~p) to (~p).",[{StateID,State},{NextStateID,NextState}]),

  %% get datas
  ScopeData = state_data(ScopeID, ScopeID),
  StateData = state_data(StateID, ScopeID),
  ?SHOW("\nget data...\n\t~p: ~p,\n\t~p: ~p,\nEdge: ~p.\n",[{ScopeID,ScopeName},ScopeData,{StateID,State},StateData,to_map(Edge)]),

  % %% get edge clause
  % {EdgeClause, PostClause} = edge(Edge, {ScopeData, StateData})
  % ?SHOW("(~p,~p), edge...\nEdgeClause:\n\t~p,\nPostClause:\n\t~p.",[{ScopeID,ScopeName},{StateID,State},EdgeClause,PostClause]),

  %% get edge clause
  case Edge#edge.is_silent of
    true ->
      %% if silent and state uses stub.hrl for handling payloads/selection, no edge here
      case State of
        select_after_state -> 
          EdgeClause = [];
        select_after_timer_state -> 
          EdgeClause = [];
        send_after_state -> 
          EdgeClause = [];
        send_after_timer_state -> 
          EdgeClause = [];
        _ -> 
          EdgeClause = edge(Edge, {ScopeData, StateData})
      end;
    _ -> 
      EdgeClause = edge(Edge, {ScopeData, StateData})
  end,
  
?SHOW("\n(~p,~p)\nEdgeClause:\n\t~p,\nFunMap:\n\t~p,\nStates:\n\t~p.",[{ScopeID,ScopeName},{StateID,State},EdgeClause,FunMap,States]),

  %% check if next state is recursive AND already mapped out
  case is_state_recursive(NextStateID, RecMap) and is_map_key(NextStateID, FunMap) of
    true -> 
      %% do not explore next state, add call to next state after current edge
      NextFunCall = atom_to_list(maps:get(ScopeID,FunMap)),
      ?SHOW("\n(~p,~p)\nnext state is recursive, adding call:\n\t~p.",[{ScopeID,ScopeName},{StateID,State},NextFunCall]),
      NextFunMap = #{ScopeID=>
        [NextFunCall++"(CoParty, "++StateData++")"]},
        UpdatedFunMap=FunMap;
    _ ->
      %% explore next state
      {NextFunMap, UpdatedFunMap} = state(NextState, NextStateID, {ScopeName, ScopeID}, Edges, States, RecMap, FunMap)
  end,

  %% add edge clause to current scope
  StateFunMap = maps:update_with(ScopeID, fun(NextStateClauses) -> 
    %% check if single action (and no after)
    case State of 
      standard_state -> 
        %% if recv in standard state, wrap in receive end here
        case Edge#edge.edge_data#edge_data.trans_type of
          send -> EdgeClause++NextStateClauses;
          recv -> [" receive "]++EdgeClause++NextStateClauses++[" end "]
        end;
      _ -> EdgeClause++NextStateClauses 
    end
  end, [], NextFunMap),

  {StateFunMap,UpdatedFunMap}.
%%

%% @doc checks the correct number of edges for the state
assert_edges(RelevantEdges, State) ->
  case State of
    init_state -> 
      %% init state must have only 1 action
      ?assert(length(RelevantEdges)==1);

    standard_state ->
      %% standard state must have only 1 action
      ?assert(length(RelevantEdges)==1);

    recv_after_state ->
      %% recv-after state must have only 2 actions
      ?assert(length(RelevantEdges)==2);

    send_after_state ->
      %% send-after state must have only 2 actions
      ?assert(length(RelevantEdges)==2);

    branch_state ->
      %% branch state must have more than 0 actions
      ?assert(length(RelevantEdges)>0);
    
    branch_after_state ->
      %% branch-after state must have more than 1 actions (since one is silent)
      ?assert(length(RelevantEdges)>1);

    select_state ->
      %% select state must have more than 0 actions
      ?assert(length(RelevantEdges)>0);

    select_after_state ->
      %% select-after state must have more than 1 actions (since one is silent)
      ?assert(length(RelevantEdges)>1);

    timer_start_state ->
      %% timer state must have only 1 action 
      ?assert(length(RelevantEdges)==1);

    delay_state ->
      %% timer state must have only 1 action 
      ?assert(length(RelevantEdges)==1);

    if_state ->
      %% timer state must have only 1 action 
      ?assert(length(RelevantEdges)==1);

    if_then_else_state ->
      %% timer state must have only 2 actions 
      ?assert(length(RelevantEdges)==2);

    error_state ->
      %% timer state must have only 1 action (to custom_end_state)
      ?assert(length(RelevantEdges)==1);

    custom_end_state ->
      %% timer state must have only 1 action (to end)
      ?assert(length(RelevantEdges)==1);
    
    _ ->
      ?SHOW("unexpected state: ~p.",[State])
  end.
%%

%% @doc checks the correct number of edges for the state
post_action_wrap(EdgesClause, State) ->
  case State of
    init_state -> EdgesClause;

    standard_state -> EdgesClause;

    recv_after_state -> [" receive "]++EdgesClause++[" end "];

    send_after_state -> EdgesClause;

    branch_state -> [" receive "]++EdgesClause++[" end "];
    
    branch_after_state -> [" receive "]++EdgesClause++[" end "];

    select_state -> EdgesClause;

    select_after_state -> EdgesClause;

    timer_start_state -> EdgesClause;

    delay_state -> EdgesClause;

    if_state -> EdgesClause;%[" receive "]++EdgesClause++[" end "]

    if_then_else_state -> EdgesClause;%[" receive "]++EdgesClause++[" end "]

    error_state -> EdgesClause;

    custom_end_state -> EdgesClause;
    
    _ -> ?SHOW("unexpected state: ~p.",[State]), EdgesClause
  end.
%%


%% @doc edge clause
%% @returns 4-tuple for edgeclause, nextstatefuns postedgeclause, postclause
edge_clause(Edge, State, StateID, {ScopeID, _ScopeName, ScopeData}=Scope, Edges, States, RecMap) ->
  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  % NextState = state_name(ToState, NextID),


  %% if send, still need data clause
  case Edge#edge.edge_data#edge_data.trans_type=:=send of
    true -> 
      {DataClause,StateData} = data_clause(StateID, ScopeID, ScopeData);
    _ ->
      case lists:member(State, [delay_state,if_state,if_then_else_state]) of 
        true -> StateData = ScopeData;
        _ ->  StateData = next_state_data(StateID, ScopeID, ScopeData)
      end,
      DataClause = []
  end,

  %% get clause for action
  {EdgeClause, PostClause} = edge(Edge, {ScopeData,StateData}),

  ?SHOW("~p, ~p, Checkpoint: EdgeClause:\n\t~p.",[Scope,{StateID},EdgeClause]),

  % ?SHOW("~p,~p, .",[Scope,{StateID}]),

  %% is current state in recursive map and not in its own scope?
  IsRecursive = is_state_recursive(StateID, RecMap) and (StateID=/=ScopeID),

  NextStateFuns = next_state_funs(IsRecursive, State, StateID, Scope, Edges, States, RecMap, {ToState, NextID, StateData}),

  PostEdgeClause = clauses_post_action(IsRecursive, State, state_name(ToState, NextID), StateData),

  ?SHOW("~p, ~p, Checkpoint: PostEdgeClause:\n\t~p.",[Scope,{StateID},PostEdgeClause]),

  {DataClause, EdgeClause, NextStateFuns, PostEdgeClause, PostClause}.
%%

%% @returns map of stateIDs to clauses
%% 
-spec state(atom(), integer(), {atom(), integer(), string()}, list(), map(), map(), map()) -> map().%{[{atom(),atom(),[[string()]]}],map()}.

%% @doc
%% on the way back out, we then doublewrap the statefuns in list when the scope-level state is exiting
state(State, StateID, {Scope, ScopeID}, Edges, States, RecMap, FunMap) -> 

  StateData = state_data(StateID, ScopeID),

  ?GAP(),
  ?SHOW("building new state...\n\tstate: ~p,\n\tstateID: ~p,\n\tScopeID: ~p,\n\tScope: ~p,\n\tStateData: ~p,\n\tEdges: ~p,\n\tStates: ~p,\n\tRecMap: ~p.\n\tFunMap: ~p.\n",[State,StateID,ScopeID,Scope,StateData,
  []% to_map(Edges)
,States,RecMap,FunMap]),

  %% check if this state is recursive and not in own scope?
  case is_state_recursive(StateID,RecMap) and (StateID/=ScopeID) of
    true -> 
      %% recursive state must not have been reached before
      ?assert(not is_map_key(StateID, FunMap)),

      %% get name of new scope
      NewScope = loop_name(State),

      % ?SHOW("starting new loop: ~p.",[NewScope]),
      % timer:sleep(500),

      %% must start within own scope
      {_ReturnMap, UpdatedFunMap} = state(State, StateID, {NewScope, StateID}, Edges, States, RecMap, maps:put(StateID, NewScope, FunMap)),

      % ?SHOW("returned from loop: ~p.",[NewScope]),
      % timer:sleep(5000),

      %% return map containing callback to new scope in old scope, and new scope
      CallbackMap = maps:put(ScopeID, [atom_to_list(NewScope)++"(CoParty, "++StateData++")"], _ReturnMap),

      %% make sure to doublewrap old scope just left
      ReturnMap = maps:update_with(StateID, fun(RecScope) -> [RecScope] end, [], CallbackMap);

    _ -> 
      %% check if in own scope anyway (as this will need an enter clause)
      case StateID=:=ScopeID of
        true -> EnterClause = clauses_enter(StateID, ScopeID); %% proceed
        _ -> EnterClause = []
      end,

      %% get edges relevant to current state
      RelevantEdges = get_outgoing_edges(StateID,Edges),
      % ?SHOW("\n(~p,~p)\nRelevantEdges:\n\t~p.",[{ScopeID,Scope},{StateID,State},to_map(RelevantEdges)]),
      assert_edges(RelevantEdges, State),

      %% check if special state to handle separately
      case State of 
        init_state -> 
          %% continue to next state
          InitEdge = lists:nth(1,RelevantEdges),
          NextStateID = InitEdge#edge.to,
          NextState = maps:get(NextStateID, States),
          {_ReturnMap, _UpdatedFunMap} = state(NextState, NextStateID, {Scope, ScopeID}, Edges, States, RecMap, FunMap),
          
          %% update funmap
          MainUpdatedFunMap = maps:put(-1, main, _UpdatedFunMap),
          UpdatedFunMap = maps:put(-2, run, MainUpdatedFunMap),

          % ?SHOW("\n(~p,~p)\n_ReturnMap:\n\t~p,\nUpdatedFunMap:\n\t~p.",[{ScopeID,Scope},{StateID,State},_ReturnMap,UpdatedFunMap]),

          %% add main enter clause to beginning
          MainMap = maps:update_with(-1, fun(LaterClauses) -> 
            % ?SHOW("LaterClauses:\n\t~p.",[LaterClauses]),
            %% return wrapped in list 
            MainClause = [ ["(CoParty, Data) -> "]++LaterClauses ],
            %% wrap once more for state funs
            [MainClause]
          end, [], _ReturnMap),

          %% add run clauses
          %% (we double wrap them so that we can have lists of clauses with the same num of params)
          RunClause1 = [ ["(CoParty) -> run(CoParty, #{timers=>#{},msgs=>#{}})"] ],
          RunClause2 = [ ["(CoParty, Data) -> main(CoParty, Data)"] ],
          RunClauses = [ RunClause1, RunClause2 ],

          ReturnMap = maps:put(-2, RunClauses, MainMap);

        custom_end_state -> 
          EndFunName = "stopping",

          EndClause2 = [ ["(CoParty, Data) -> "++EndFunName++"(normal, CoParty, Data)"] ],

          EndClause3 = [ ["(normal=_Reason, _CoParty, _Data) -> exit(normal)"],
                         ["({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details)"],
                         ["({error, Reason}, CoParty, Data) when is_atom(Reason) -> "++EndFunName++"({error, Reason, []}, CoParty, Data)"],
                         ["(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason)"] ],

          %% update funmap
          UpdatedFunMap = maps:put(StateID, list_to_atom(EndFunName), FunMap),

          %% insert current clauses in front of those from future states
          %% note: clause in current scope is not double wrapped list, since this will be resolved when leaving that scope
          ReturnMap = #{ScopeID => [ EndFunName++"(CoParty, "++StateData++")" ],
                        StateID => [ EndClause2, EndClause3 ] };

        _ -> %% continue as normal

          %% go through all action edges and build their clauses, and add them as a map against their label
          BuildActionClauses = fun(Edge,{EdgeClauseMap,StateFunMap, _FunMap}) ->
            %% depth-first build of Edge, returns function map
            {EdgeStateFunMap, EdgeFunMap} = clauses_edge(Edge, State, StateID, {Scope, ScopeID}, Edges, States, RecMap, _FunMap),
            %% merge fun maps
            MergedFunMap = maps:merge(_FunMap,EdgeFunMap),
            MergedStateMap = update_fun_maps(StateFunMap,EdgeStateFunMap),
            %% get for current scope (and remove from EdgeStateFunMap)
            {_, EdgeClause} = pop_map(ScopeID,EdgeStateFunMap,[]),
            %% edgeclause must not be empty, as otherwise how would action be performed
            ?assert(length(EdgeClause)>0),
            %% get action label
            EdgeLabel = get_msg_label(Edge),
            %% add to EdgeClauseMap
            {maps:put(EdgeLabel, EdgeClause, EdgeClauseMap), MergedStateMap, MergedFunMap}
          end,

          %% get silent and non-silent
          {ActionEdges, SilentEdges} = separate_edges(RelevantEdges),
          % ?GAP(),?SHOW("\n(~p,~p)\nActionEdges:\n\t~p,\nSilentEdges:\n\t~p.\n\n",[{ScopeID,Scope},{StateID,State},to_map(ActionEdges),to_map(SilentEdges)]),

          %% build clause for each non-silent relevant edge
          {ActionEdgesClauseMap, ActionReturnMap, ActionFunMap} = lists:foldl(BuildActionClauses, {#{},#{},FunMap}, ActionEdges),

          ?GAP(),?SHOW("\n(~p:~p)\nActionEdgesClauseMap:\n\t~p,\nActionReturnMap:\n\t~p,\nActionFunMap:\n\t~p.",[{ScopeID,Scope},{StateID,State},ActionEdgesClauseMap,ActionReturnMap,ActionFunMap]),?GAP(),


          %% unpack into action clauses (depending on type of action)
          case State of
            send_after_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> [K++" -> "]++V; _ -> AccIn++["; "++K++" -> "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = ActionReturnMap;
            select_after_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> [K++" -> "]++V; _ -> AccIn++["; "++K++" -> "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = maps:put(ScopeID, ActionClauses, ActionReturnMap);
            select_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> [K++" -> "]++V; _ -> AccIn++["; "++K++" -> "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = ActionReturnMap;
            branch_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> V; _ -> AccIn++["; "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = maps:put(ScopeID, ActionClauses, ActionReturnMap);
            branch_after_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> V; _ -> AccIn++["; "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = maps:put(ScopeID, ActionClauses, ActionReturnMap);
            recv_after_state ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> V; _ -> AccIn++["; "]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = maps:put(ScopeID, ActionClauses, ActionReturnMap);
            _ ->
              ActionClauses = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> V; _ -> AccIn++[";"]++V end end, [], ActionEdgesClauseMap),
              UpdatedActionReturnMap = ActionReturnMap
          end,

          ?GAP(),?SHOW("\n(~p:~p)\nActionClauses:\n\t~p.",[{ScopeID,Scope},{StateID,State},ActionClauses]),?GAP(),

          %% if there are any silent edges, apply them
          case length(SilentEdges)>0 of
            true -> 
              case State of
                if_then_else_state ->
                  %% if_then_else_states have two silent edges
                  ?assert(length(SilentEdges)==2),


                  %% go through all action edges and build their clauses, and add them as a map against their label
                  BuildIfClauses = fun(Edge,{EdgeClauseMap,StateFunMap, _FunMap}) ->
                    %% depth-first build of Edge, returns function map
                    {EdgeStateFunMap, EdgeFunMap} = clauses_edge(Edge, State, StateID, {Scope, ScopeID}, Edges, States, RecMap, _FunMap),
                    %% merge fun maps
                    MergedFunMap = maps:merge(_FunMap,EdgeFunMap),
                    MergedStateMap = update_fun_maps(StateFunMap,EdgeStateFunMap),
                    %% get for current scope (and remove from EdgeStateFunMap)
                    {_, EdgeClause} = pop_map(ScopeID,EdgeStateFunMap,[]),
                    %% edgeclause must not be empty, as otherwise how would action be performed
                    ?assert(length(EdgeClause)>0),
                    %% get if label
                    EdgeLabel = get_if_label(Edge),
                    %% add to EdgeClauseMap
                    {maps:put(EdgeLabel, EdgeClause, EdgeClauseMap), MergedStateMap, MergedFunMap}
                  end,

                  {IfEdgesClauseMap, IfReturnMap, IfFunMap} = lists:foldl(BuildIfClauses, {#{},#{},FunMap}, SilentEdges),

                  %% reorder the edges
                  Edge1 = lists:nth(1,SilentEdges),
                  case maps:get(is_not,Edge1#edge.edge_data#edge_data.if_stmt) of
                    true ->
                      ?assert(is_map_key(if_not_edge,IfEdgesClauseMap)),
                      ?assert(is_map_key(else_not_edge,IfEdgesClauseMap)),
                      EdgesClause = maps:get(else_not_edge,IfEdgesClauseMap)++maps:get(if_not_edge,IfEdgesClauseMap)++[" end"];
                    _ ->
                      ?assert(is_map_key(if_edge,IfEdgesClauseMap)),
                      ?assert(is_map_key(else_edge,IfEdgesClauseMap)),
                      EdgesClause = maps:get(if_edge,IfEdgesClauseMap)++maps:get(else_edge,IfEdgesClauseMap)++[" end"]
                  end,
                  _ReturnMap = IfReturnMap,
                  UpdatedFunMap = IfFunMap;

                _ ->
                %% should only be one silent edge
                ?assert(length(SilentEdges)==1),

                Timeout = lists:nth(1,SilentEdges),
                _TimeoutRef = maps:get(ref,Timeout#edge.edge_data#edge_data.timeout),
                case is_number(_TimeoutRef) of
                  true -> TimeoutRef = integer_to_list(floor(_TimeoutRef));
                  _ -> TimeoutRef = _TimeoutRef
                end,
                % {_SilentClause,SilentEsNextStateFuns} = resolve_edge_clause(Timeout,  State, StateID, Scope, Edges, States, RecMap),
                % %% remove front from _silentclause (which will either be 'after Timeout -> ' or '{timeout,...}')
                % SilentClause = lists:nthtail(1,_SilentClause),

                %% timeout edge
                {SilentReturnMap, SilentEdgeFunMap} = clauses_edge(Timeout, State, StateID, {Scope, ScopeID}, Edges, States, RecMap, FunMap),

                % ?GAP(),?SHOW("\n(~p:~p)\nUpdatedActionReturnMap:\n\t~p,\nActionFunMap:\n\t~p,\nSilentReturnMap:\n\t~p,\nSilentEdgeFunMap:\n\t~p.",[{ScopeID,Scope},{StateID,State},UpdatedActionReturnMap,ActionFunMap,SilentReturnMap,SilentEdgeFunMap]),?GAP(),


                %% merge fun maps
                _UpdatedFunMap = maps:merge(ActionFunMap,SilentEdgeFunMap),
                _ReturnMap = update_fun_maps(UpdatedActionReturnMap,SilentReturnMap),


                PayloadFunName=get_state_payload_fun_name(StateID),
                PayloadFunArgs=""++StateData++"",

                UpdatedFunMap1 = maps:update_with(-3,fun(OtherFuns) -> OtherFuns++[PayloadFunName] end, [], _UpdatedFunMap),

                case State of
                  select_after_state ->
                    SilentSelection = maps:get(ScopeID,SilentReturnMap);
                  select_after_timer_state ->
                    SilentSelection = maps:get(ScopeID,SilentReturnMap);
                  send_after_state ->
                    SilentSelection = maps:get(ScopeID,SilentReturnMap);
                  send_after_timer_state ->
                    SilentSelection = maps:get(ScopeID,SilentReturnMap);
                  _ ->
                    SilentSelection = maps:get(ScopeID,_ReturnMap)
                end,
                
                % ?GAP(),?SHOW("\n(~p:~p)\nSilentSelection:\n\t~p,\n_ReturnMap:\n\t~p,\n_UpdatedFunMap:\n\t~p.",[{ScopeID,Scope},{StateID,State},SilentSelection,_ReturnMap,_UpdatedFunMap]),?GAP(),

                % {SilentEdgesClauseMap, _ReturnMap, UpdatedFunMap} = lists:foldl(BuildEdgeClause, {[],ActionReturnMap,ActionFunMap}, SilentEdges),


                % SilentSelection = maps:fold(fun(K, V, AccIn) -> case length(AccIn)==0 of true -> [K++" -> "]++V; _ -> AccIn++["; "++K++" -> "]++V end end,  [], ActionEdgesClauseMap),


                %% handle send/select after
                case State of
                  send_after_state ->
                    case is_number(_TimeoutRef) of
                      true -> TimeoutDuration = TimeoutRef;
                      _ -> TimeoutDuration = "get_timer("++TimeoutRef++", "++StateData++")"
                    end,
                    EdgesClause = ["AwaitPayload = nonblocking_payload(fun "++atom_to_list(PayloadFunName)++"/1, "++PayloadFunArgs++", self(), "++TimeoutDuration++"),","receive {AwaitPayload, ok, {Label, Payload}} -> case Label of "]++ActionClauses++[";","_ -> error(unexpected_label_selected) end; {AwaitPayload, ko} -> "]++SilentSelection++["end"],
                    UpdatedFunMap = UpdatedFunMap1;

                  select_after_state ->
                    case is_number(_TimeoutRef) of
                      true -> TimeoutDuration = TimeoutRef;
                      _ -> TimeoutDuration = "get_timer("++TimeoutRef++", "++StateData++")"
                    end,
                    SelectionFunName=get_state_selection_fun_name(StateID),
                    SelectionFunArgs=""++StateData++"",

                    EdgesClause = ["AwaitSelection = nonblocking_selection(fun "++atom_to_list(SelectionFunName)++"/1, "++SelectionFunArgs++", self(), "++TimeoutDuration++"),","receive {AwaitSelection, ok, {Label, Payload}} -> case Label of "]++ActionClauses++[";", "_ -> error(unexpected_label_selected) end; {AwaitPayload, ko} -> "]++SilentSelection++["end"],
                    UpdatedFunMap = maps:update_with(-3,fun(OtherFuns) -> OtherFuns++[SelectionFunName] end, [], UpdatedFunMap1);

                  _ -> 
                    EdgesClause = SilentSelection,
                    UpdatedFunMap = UpdatedFunMap1
                end
              end;
              
            _ ->
              %% handle select 
              case State of
              %   send_after_state ->
              %     SelectionClause = lists:foldl(fun(ActionClause, AccIn) -> AccIn++["{}"])

                  % PayloadFunName=get_state_payload_fun_name(StateID),
                  % PayloadFunArgs="[]",

              %     EdgesClause = ["case {Label,Payload}="++atom_to_list(PayloadFunName)++"(["++PayloadArgs++"]) of"]++SelectionClause++["; _ -> error(unexpected_label_selected)"]++["end"]; 

                select_state ->
                  _ReturnMap = ActionReturnMap,
                  UpdatedFunMap=ActionFunMap,

                  PayloadFunName=get_state_payload_fun_name(StateID),
                  PayloadFunArgs="["++StateData++"]",

                  EdgesClause = ["case {Label,Payload}="++atom_to_list(PayloadFunName)++"("++PayloadFunArgs++") of"]++ActionClauses++["; _ -> error(unexpected_label_selected)"]++["end"]; 

                _ -> 
                  {EdgesClause, _ReturnMap, UpdatedFunMap} = {ActionClauses, ActionReturnMap, ActionFunMap}
              end
          end,

          % ?GAP(),?SHOW("\n(~p,~p)\nEdgesClause:\n\t~p\n_ReturnMap:\n\t~p,\nUpdatedFunMap:\n\t~p.\n\n",[{ScopeID,Scope},{StateID,State},EdgesClause,_ReturnMap,UpdatedFunMap]),

          %% post amendments
          PostWrappedClause = post_action_wrap(EdgesClause, State),
          % ?SHOW("\n(~p,~p)\nPostWrappedClause:\n\t~p.",[{ScopeID,Scope},{StateID,State},PostWrappedClause]),

          Clauses = []++EnterClause++PostWrappedClause,
          % ?SHOW("\n(~p,~p)\nClauses:\n\t~p.",[{ScopeID,Scope},{StateID,State},Clauses]),

          %% insert current clauses in front of those from future states
          ReturnMap = maps:update_with(ScopeID, fun(LaterClauses) -> Clauses%++LaterClauses 
          end, [], _ReturnMap)
      end
  end,

  % ?SHOW("\n(~p,~p)\nReturnMap:\n\t~p.",[{ScopeID,Scope},{StateID,State},ReturnMap]),

  %% if current case is a new recursive scope, then re-wrap statefuns in another list
  case is_state_recursive(StateID,RecMap) and (StateID==ScopeID) of
    true -> WrappedMap = maps:update_with(ScopeID, fun(ScopeClauses) -> [ScopeClauses] end, [], ReturnMap);
    false -> WrappedMap = ReturnMap
  end,

  ?SHOW("\n(~p,~p)\nWrappedMap:\n\t~p,\nUpdatedFunMap:\n\t~p.\n",[{ScopeID,Scope},{StateID,State},WrappedMap,UpdatedFunMap]),

  %% return map
  {WrappedMap, UpdatedFunMap}.






%% @doc if then else statement
state(if_then_else_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  %% TODO: use 0ms timeout for else
  

  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),
  

  {StateClause, NextStateFuns} = if_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc if statement
state(if_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  %% TODO: use 0ms timeout for else -> error
  
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),
  

  {StateClause, NextStateFuns} = if_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc select 
state(select_after_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  {StateClause, NextStateFuns} = select_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc select 
state(select_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  {StateClause, NextStateFuns} = select_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc send after 
state(send_after_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->

  ?GAP(),
  ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID,State},State]),
  ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID,State},StateID]),
  ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID,State},Scope]),
  ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID,State},States]),
  ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID,State},RecMap]),
  ?GAP(),


  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  {StateClause, NextStateFuns} = select_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  ?GAP(),
  ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;

  % {DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),

  % {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause++PostEdgeClause,PostClause),

  % StateClauses = [StateClause],


  % ?GAP(),
  % ?SHOW("~p, ~p, StateClauses:\n\t~p.",[Scope,{StateID},StateClauses]),

  % StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  % StateFuns = [StateFun]++NextStateFuns,
  % ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  % StateFuns;
%%

%% @doc branch after
state(branch_after_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  %% 
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),

  {StateClause, NextStateFuns} = branch_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  StateClauses = [EnterClause++StateClause],

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  % ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%


%% @doc branch 
state(branch_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  %% 
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),

  {StateClause, NextStateFuns} = branch_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  StateClauses = [EnterClause++StateClause],

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  % ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%



%% @doc recv after
state(recv_after_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->

  % ?GAP(),
  % ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID,State},State]),
  % ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID,State},StateID]),
  % ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID,State},Scope]),
  % ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID,State},States]),
  % ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID,State},RecMap]),

  %% 
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),

  EnterClause = clauses_enter(State, StateID, Scope),
  % ?SHOW("~p, ~p, Checkpoint: EnterClause.",[Scope,{StateID}]),

  {StateClause, NextStateFuns} = branch_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  StateClauses = [EnterClause++StateClause],

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  % ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc error state
state(error_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->

  %% 
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),
  
  {DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),

  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause++PostEdgeClause,PostClause),

  StateClauses = [StateClause],

  ?SHOW("~p, ~p, EnterClause:\n\t~p.",[Scope,{StateID,State},EnterClause]),
  ?SHOW("~p, ~p, EdgeClause:\n\t~p.",[Scope,{StateID,State},EdgeClause]),
  ?SHOW("~p, ~p, StateClause:\n\t~p.",[Scope,{StateID,State},StateClause]),
  ?SHOW("~p, ~p, NextStateFuns:\n\t~p.",[Scope,{StateID,State},NextStateFuns]),


  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  ?SHOW("~p, ~p, StateFuns:\n\t~p.",[Scope,{StateID},StateFuns]),
  StateFuns;
%%

%% @doc 
state(delay_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->

  RelevantEdges = get_outgoing_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),


  EnterClause = clauses_enter(State, StateID, Scope),

  {DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),

  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause++PostEdgeClause,PostClause),

  StateClauses = [StateClause],

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  StateFuns;

%%




%% @doc generates snippet for delay state
%% timers must be stored in the map Data 
state(timer_start_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  
  RelevantEdges = get_outgoing_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),

  {DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),

  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause++PostEdgeClause,PostClause),

  StateClauses = [StateClause],

  %% prepare to return state funs
  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  StateFuns;

%%


%% @doc generates snippets for a given state
%% if recursive state, 
%% @returns tuple of {list_of_state_funs, list_of_next_states}
state(standard_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->

  ?GAP(),
  ?SHOW("~p, ~p, State: ~p.", [Scope, {StateID,State},State]),
  ?SHOW("~p, ~p, StateID: ~p.", [Scope, {StateID,State},StateID]),
  ?SHOW("~p, ~p, Scope: ~p.", [Scope, {StateID,State},Scope]),
  ?SHOW("~p, ~p, States: ~p.", [Scope, {StateID,State},States]),
  ?SHOW("~p, ~p, RecMap: ~p.", [Scope, {StateID,State},RecMap]),


  RelevantEdges = get_outgoing_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = clauses_enter(State, StateID, Scope),

  {DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),

  {StateClause,NextStateFuns} = resolve_clauses(Scope,StateID,EnterClause,LaterStateFuns,DataClause,EdgeClause++PostEdgeClause,PostClause),

  StateClauses = [StateClause],

  StateFun = {?EXPORT_DEFAULT,ScopeName,StateClauses},
  StateFuns = [StateFun]++NextStateFuns,
  StateFuns;
%% 

%% @doc 
state(custom_end_state=State, StateID, {_ScopeID, ScopeName, ScopeData}=_Scope, Edges, _States, _RecMap) ->
  % {_, {_,FunName,_}=Fun, _} = special_state(State,StateID, Edges),
  % [{true,ScopeName,[atom_to_list(FunName)++"(CoParty, "++ScopeData++")"]},Fun];
  {_, Funs, _} = special_state(State,StateID, Edges),
  Fun=lists:nth(1,Funs),
  {_,FunName,_}=Fun,
  [{?EXPORT_DEFAULT,ScopeName,[atom_to_list(FunName)++"(CoParty, "++ScopeData++")"]}]++Funs;
%% 

%% @doc 
state(State, _StateID, {_ScopeID, _ScopeName, _ScopeData}=Scope, _Edges, _States, _RecMap) ->
  % StateFuns = [],
  % NextStateFuns = [],

  ?SHOW("~p, unhandled: ~p. (return empty)", [Scope,State]),


  [].
%% 


%% @doc adds the definitions for run, and adds
%% @returns triple {next_state, [StateFunClauses], {NextStateID, NextScopeID}}
special_state(init_state=_State, StateID, Edges) ->
  RelevantEdges = get_outgoing_edges(StateID, Edges),
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

  %% Clause1 is wrapper for only one param, and redurects to Clause2
  %% Clause2 redirects to main(CoParty,Data)
  {next_state, [{?EXPORT_DEFAULT, Name, [Clause1]},{?EXPORT_DEFAULT,Name,[Clause2]}], {NextStateID, Main}};
%%

%% @doc wrapper function for end state, should go to custom_end_state
%% @see state(custom_end_state, ...)
special_state(end_state=_State, StateID, Edges) -> 
  ?SHOW("~p...",[_State]),
  special_state(custom_end_state, StateID, Edges);

%% @doc custom end state is a function named 'stopping'
%% allows user to catch/perform actions during shutdown
%% @returns triple {none, [StateFunClauses], ok}
special_state(custom_end_state=_State, _StateID, _Edges) ->
  ?SHOW("~p...",[_State]),
  Name = stopping,

  %%
  ClauseDefault = [
      "%% @doc Adds default reason 'normal' for stopping. \n",
      "%% @see "++atom_to_list(Name)++"/3. \n",
      "(CoParty, Data) -> ",
      atom_to_list(Name)++"(normal, CoParty, Data)"
    ],

  %%
  ClauseNormal = [
      "%% @doc Adds default reason 'normal' for stopping. \n",
      "%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}. \n",
      "(normal=_Reason, _CoParty, _Data) -> ",
      "exit(normal)"
    ],

  %%
  ClauseError = [
      "%% @doc stopping with error.\n",
      "%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.\n",
      "%% @param CoParty is the process ID of the other party in this binary session.\n",
      "%% @param Data is a list to store data inside to be used throughout the program.\n",
      "({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> ",
        "erlang:error(Reason, Details)"
    ],

  %%
  ClausePartialError = [
      "%% @doc Adds default Details to error.\n",
      "({error, Reason}, CoParty, Data) when is_atom(Reason) -> ",
      atom_to_list(Name)++"({error, Reason, []}, CoParty, Data)"
    ],

  %%
  ClauseUnknown = [
      "%% @doc stopping with Unexpected Reason.\n",
      "(Reason, _CoParty, _Data) when is_atom(Reason) -> ",
      "exit(Reason)"
    ],

  %% clausedefault uses different multi-despatch
  Clauses = [ClauseNormal, ClauseError, ClausePartialError, ClauseUnknown],

  {none, [{?EXPORT_DEFAULT, Name, [ClauseDefault]},{?EXPORT_DEFAULT,Name,Clauses}], ok};
%%

%% @doc catch unexpected states
special_state(State, StateID, Edges) when is_atom(State) ->
  ?SHOW("unexpected state: ~p.", [State]),
  ?SHOW("stateid: ~p.", [StateID]),
  ?SHOW("edges: ~p.", [Edges]),
  {unknown, {true, State, []}, -1}.
%%
