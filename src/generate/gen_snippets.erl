%% @doc Erlang snippets used in stub generation
-module(gen_snippets).
-compile({nowarn_unused_function, [ {get_state_name,2}, {get_next_state_trans,2} ]}).

-define(EXPORT_DEFAULT, false).

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




%%%%%%%%%% todo
%% todo
%% todo :: fix issue with recursive if_state/if_then_else_state
%% todo :: test the above with selection/branch/after etc
%% todo
%% todo
%% todo
%% todo
%% todo
%% todo










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
    if_state -> list_to_atom("state" ++ StateID ++ "_if");
    if_then_else_state -> list_to_atom("state" ++ StateID ++ "_if_else");
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
timer_name(Name) when is_list(Name) -> ""++Name;
timer_name(Name) when is_atom(Name) -> ""++atom_to_list(Name).

%% @doc generates clause for actions/outgoing edges

%% @doc error state
edge(#edge{edge_data=#edge_data{error_reason=ErrorReason},is_error=true}, {_StateData,_NextStateData}) ->
  EdgeClause = " error("++atom_to_list(ErrorReason)++"), ",
  {[EdgeClause], []};

%% @doc for defining new timers and adding them to data
edge(#edge{edge_data=#edge_data{timer=#{duration:=Duration,name:=Name}},is_timer=true,is_delay=false}, {StateData,NextStateData}) ->
  EdgeClause = "{"++NextStateData++",_TID_"++timer_name(Name)++"} = set_timer("++timer_name(Name)++", "++integer_to_list(Duration)++", "++StateData++"), ",
  {[EdgeClause], []};

%% @doc for delay (via timer)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Timer}},is_delay=true,is_timer=false}, {StateData,_NextStateData}) when is_list(Timer) ->
  ?SHOW("timer ref: ~p.",[Timer]),
  EdgeClause = "% (delay until "++timer_name(Timer)++" completes, error if does not exist) \n case get_timer("++timer_name(Timer)++", "++StateData++") of {ok, TID_"++timer_name(Timer)++"} -> receive {timeout, TID_"++timer_name(Timer)++", {timer, "++timer_name(Timer)++"}} -> ok end; {ko, no_such_timer} -> error(no_such_timer) end,",
  {[EdgeClause], []};

%% @doc for delay (via delay)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Delay}},is_delay=true,is_timer=false}, {_StateData,_NextStateData}) when is_number(Delay) ->
  EdgeClause = "timer:sleep("++integer_to_list(floor(Delay))++"),",
  {[EdgeClause], []};


%% @doc for timeout edge using timer
%% adds sneaky ';' before to allow the joining to be easier, since there must always be some other receiving action before this
edge(#edge{edge_data=#edge_data{timeout=#{ref:=Timer}},is_silent=true}=_Edge, {_StateData,_NextStateData}) when is_list(Timer) -> 
  {["; {timeout, _TID_"++timer_name(Timer)++", {timer, "++timer_name(Timer)++"}} -> "], []};

%% @doc for timeout edge using duration
edge(#edge{edge_data=#edge_data{timeout=#{ref:=Duration}},is_silent=true}=_Edge, {_StateData,_NextStateData}) when is_number(Duration) -> 
  {["after "++integer_to_list(floor(Duration))++" -> "], []};


%% @doc for edge within choice
edge(#edge{edge_data=#edge_data{event = {_Act, Var} ,trans_type = TransType},is_silent=false,is_choice=true}=Edge, {StateData,NextStateData}) -> 

  Label = get_msg_label(Edge),
  ?SHOW("label: ~p.",[Label]),
  StrVar = atom_to_list(Var),
  StrPayload = "Payload_"++StrVar,
  case TransType of
    send ->{["CoParty ! {self(), "++Label++", Payload},"],[]};
      % PayloadClause = [%"%% replace 'ok' below with some payload \n",
      % StrPayload++" = \'payload\',"],
      % SendClause = [%"%% send \n",
      % "CoParty ! {self(), "++Label++", Payload_"++StrVar++"},"],
      % Clauses = PayloadClause++SendClause,
      % {Clauses, []};
    recv -> 
      RecvClause = [%"%% recv \n",
      "{CoParty, "++Label++", "++StrPayload++"} -> "],
      DataClause = [ NextStateData++" = save_msg("++Label++", "++StrPayload++", "++StateData++"), " ],
      Clauses = RecvClause++DataClause,
      {Clauses, []};
    _ ->
      {[], []}
  end;

%% @doc for lone action (prewrap in recv)
edge(#edge{edge_data=#edge_data{event = {_Act, Var} ,trans_type = TransType},is_silent=false,is_choice=false}=Edge, {StateData,NextStateData}) -> 

  % reng_show(edge, Edge, "\nbuilding edge snippet:\n"),

  case TransType of
    send -> %{["CoParty ! {self(), "++get_msg_label(Edge)++", Payload},"],[]}; % edge(Edge#edge{is_choice=true},{StateData,NextStateData});
    Label = get_msg_label(Edge),
    StrVar = atom_to_list(Var),
    StrPayload = "Payload_"++StrVar,
    PayloadClause = [%"%% replace 'ok' below with some payload \n",
      StrPayload++" = \'payload\',"],
      SendClause = [%"%% send \n",
      "CoParty ! {self(), "++Label++", Payload_"++StrVar++"},"],
      Clauses = PayloadClause++SendClause,
      {Clauses, []};
    recv -> 
      {Clauses, PostClause} = edge(Edge#edge{is_choice=true},{StateData,NextStateData}),
      {["receive "]++Clauses, PostClause++["end"]};
    _ ->
      {[], []}
  end;
%%

%% @doc other case
edge(_Edge, {_StateData,_NextStateData}) -> {[],[]}.

%% @doc strip away prefix to get edge (msg) label
get_msg_label(#edge{edge_data=#edge_data{event = {Act, _Var} ,trans_type = send}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "send_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end;
get_msg_label(#edge{edge_data=#edge_data{event = {Act, _Var} ,trans_type = recv}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "receive_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end.
%%

%% @doc combo of edge andresolve clause functions
resolve_edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap) ->
  %% get edge clause
  {_DataClause, EdgeClause, LaterStateFuns, PostEdgeClause, _PostClause} = edge_clause(Edge, State, StateID, Scope, Edges, States, RecMap),
  %% get resolved
  {ElemStateClause,ElemNextStateFuns} = resolve_clauses(Scope,StateID,[],LaterStateFuns,[],EdgeClause,PostEdgeClause),
  {ElemStateClause,ElemNextStateFuns}.

%% @doc adds any nextstatefun tuples from ToMerge to Primary that are not already in Primary
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

%% empty case
get_if_edges([]) -> {{undefined,undefined}, {undefined,undefined}};

%% if timer 
get_if_edges([#edge{to=To,is_if=true,is_else=false,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=false}}}=H|T]) ->
  %% get tail, which must go second
  {{undefined,undefined}, Tail} = get_if_edges(T),
  %% return first
  {{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}},Tail};

%% if timer else
get_if_edges([#edge{to=To,is_if=false,is_else=true,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=false}}}=H|T]) ->
  %% get tail, which must go first 
  {Tail, {undefined,undefined}} = get_if_edges(T),
  %% return second
  {Tail,{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}}};

%% if not timer 
get_if_edges([#edge{to=To,is_if=true,is_else=false,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=true}}}=H|T]) ->
  %% get tail, which must go first 
  {Tail, {undefined,undefined}} = get_if_edges(T),
  %% return second
  {Tail,{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}}};

%% if not timer else
get_if_edges([#edge{to=To,is_if=false,is_else=true,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=true}}}=H|T]) ->
  %% get tail, which must go second
  {{undefined,undefined}, Tail} = get_if_edges(T),
  %% return first
  {{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}},Tail}.

get_if_timer(#{is_timer:=IsTimer,ref:=Ref,to:=_To}) ->
  case IsTimer of
    true -> Ref;
    _ -> atom_to_list(Ref)
  end.

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

%% @doc edge clause
%% @returns 4-tuple for edgeclause, nextstatefuns postedgeclause, postclause
edge_clause(Edge, State, StateID, {ScopeID, _ScopeName, ScopeData}=Scope, Edges, States, RecMap) ->
  %% get next state after the transition
  NextID = Edge#edge.to,
  ToState = maps:get(NextID, States),
  % NextState = get_state_name(ToState, NextID),


  %% if send, still need data clause
  case Edge#edge.edge_data#edge_data.trans_type=:=send of
    true -> 
      {StateData, DataClause} = data_clause(State, StateID, Scope);
    _ ->
      case lists:member(State, [delay_state,if_state,if_then_else_state]) of 
        true -> StateData = ScopeData;
        _ ->  {StateData, _} = next_state_data(StateID, Scope)
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

  PostEdgeClause = post_edge_clauses(IsRecursive, State, get_state_name(ToState, NextID), StateData),

  ?SHOW("~p, ~p, Checkpoint: PostEdgeClause:\n\t~p.",[Scope,{StateID},PostEdgeClause]),

  {DataClause, EdgeClause, NextStateFuns, PostEdgeClause, PostClause}.
%%

%% @doc 
% post_action_clauses(standard_state=State) -> {}.

%% @doc
next_state_funs(true=_IsRecursive, standard_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {ToState, NextID, StateData}=_NextData) ->
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(), 
  state(State, StateID, {StateID, get_loop_name(atom_to_list(get_state_name(ToState, NextID))), StateData}, Edges,States,RecMap);
%%

%% @doc
next_state_funs(true=_IsRecursive, if_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {_ToState, _NextID, StateData}=_NextData) -> 
  % timer:sleep(500),
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(),
  S = state(State, StateID, {StateID, get_loop_name(atom_to_list(get_state_name(State, StateID))), StateData}, Edges,States,RecMap),
  ?GAP(),?SHOW("~p,\n\t~p, ~p,\n\tS: ~p.",[_Scope,{StateID,State},_NextData,S]),
  ?GAP(),
  S;


%% @doc
next_state_funs(true=_IsRecursive, if_then_else_state=State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {_ToState, _NextID, StateData}=_NextData) -> 
  % timer:sleep(500),
  ?GAP(),?SHOW("~p,\n\t~p, ~p.",[_Scope,{StateID,State},_NextData]),?GAP(),
  S = state(State, StateID, {StateID, get_loop_name(atom_to_list(get_state_name(State, StateID))), StateData}, Edges,States,RecMap),
  ?GAP(),?SHOW("~p,\n\t~p, ~p,\n\tS: ~p.",[_Scope,{StateID,State},_NextData,S]),
  ?GAP(),
  S;


%% @doc
% next_state_funs(true=_IsRecursive, State, StateID, {_ScopeID, _ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {ToState, NextID, StateData}) -> 
%   state(State, StateID, {StateID, get_loop_name(atom_to_list(get_state_name(ToState, NextID))), StateData}, Edges,States,RecMap);
%%

%% @doc 
next_state_funs(false=_IsRecursive, _State, _StateID, {ScopeID, ScopeName, _ScopeData}=_Scope, Edges, States, RecMap, {ToState, NextID, StateData}=_NextData) -> 
  ?GAP(),?SHOW("~p, not recursive\n\t~p, ~p.",[_Scope,{_StateID,_State},_NextData]),?GAP(),
  %% check if next state is recursive 
  case is_state_recursive(NextID, RecMap) and (NextID=:=ScopeID) of %% and(StateID=:=ScopeID) of
    true -> %% reconstruct loop name to go back
      Return=[{?EXPORT_DEFAULT,ScopeName,[atom_to_list(get_loop_name(get_state_name(ToState, NextID)))++"(CoParty,"++StateData++")"]}],
      ?SHOW("~p, ~p, not recursive return:\n\t~p.",[_Scope,{_StateID,_State},Return]),
      Return;
    _ -> %% some other state to explore
      state(ToState, NextID, {ScopeID, ScopeName, StateData}, Edges,States,RecMap)
  end.
%%

%% @doc for adding function calls (recursive loops) to the end of clauses (after action)
%% only communicating, recursive state are likely to return anything non-empty.
post_edge_clauses(true=_IsRecursive, standard_state=_State, NextState, StateData) -> [atom_to_list(get_loop_name(atom_to_list(NextState)))++"(CoParty, "++StateData++")"];
post_edge_clauses(false=_IsRecursive, standard_state=_State, _NextState, _StateData) -> [];
post_edge_clauses(_IsRecursive, _State, _NextState, _StateData) -> [].

%%

%% @doc enter clauses
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
%% if second elem is -1, thenresolve_edge_clause this is a fresh scope
next_state_data(StateID, {ScopeID, _ScopeName, _ScopeData}=_Scope) 
  when ScopeID==-1 -> {"Data"++integer_to_list(StateID), -1};
next_state_data(StateID, {_ScopeID, _ScopeName, ScopeData}=_Scope) -> 
  {ScopeData++"_"++integer_to_list(StateID), StateID}.


%% @doc if then else statement
state(if_then_else_state=State, StateID, {_ScopeID, ScopeName, _ScopeData}=Scope, Edges, States, RecMap) ->
  %% TODO: use 0ms timeout for else
  

  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
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
  
  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
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


  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),
  % Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),

  {StateClause, NextStateFuns} = branch_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)>0),

  {StateClause, NextStateFuns} = branch_clauses(RelevantEdges, State, StateID, Scope, Edges, States, RecMap),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  % ?SHOW("~p, ~p, Checkpoint: RelevantEdges:\n\t~p",[Scope,{StateID},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==2),

  EnterClause = enter_clause(State, StateID, Scope),
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
  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),
  
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

  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),


  EnterClause = enter_clause(State, StateID, Scope),

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
  
  RelevantEdges = get_relevant_edges(StateID, Edges),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),

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


  RelevantEdges = get_relevant_edges(StateID, Edges),
  ?SHOW("~p, ~p, RelevantEdges:\n\t~p.", [Scope, {StateID,State},RelevantEdges]),
  %% standard state should only have single action
  ?assert(length(RelevantEdges)==1),
  Edge = lists:nth(1, RelevantEdges),

  EnterClause = enter_clause(State, StateID, Scope),

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

  {next_state, [{?EXPORT_DEFAULT, Name, [Clause1]},{?EXPORT_DEFAULT,Name,[Clause2]}], {NextStateID, Main}};
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

  {none, [{?EXPORT_DEFAULT, Name, [ClauseDefault]},{?EXPORT_DEFAULT,Name,Clauses}], ok};
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

%% @doc returns true if edge is silent
is_edge_silent(Edge) -> Edge#edge.is_silent.
is_edge_not_silent(Edge) -> not is_edge_silent(Edge).

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

