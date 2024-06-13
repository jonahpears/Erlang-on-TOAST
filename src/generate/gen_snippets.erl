-module(gen_snippets).
% -compile({nowarn_unused_function,[ {state_name,2} ]}).

-export([state/6,state/8]).

-define(EXPORT_DEFAULT, false).

%% disable parse transforms
% -define(MERL_NO_TRANSFORM, true).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").

%% for records
-include("reng.hrl").

%% for helper functions
-include("stub_tools.hrl").
-include("fsm_tools.hrl").

%%%%%%%%%%%
%%% state
%%%%%%%%%%%

%% @doc 
%% @returns map of state functions, between scopeIDs and program text.
-spec state(atom(), integer(), list(), map(), map(), map()) -> map().

%% @doc for first pass, sets up initial/stopping states 
state(init_state=State, 0=StateID, Edges, States, RecMap, FunMap) ->
  
  ?VSHOW("starting new fsm.\nedges:\t~p,\nstates:\t~p,\nrecmap:\t~p,\nfunmap:\t~p.",[Edges,States,RecMap,FunMap]),

  Scope = State,
  ScopeID = StateID,

  %% get run and init snippets
  InitClauses = special_snippet_clauses(init, States),
  RunClauses = special_snippet_clauses(run, States),

  StopID = get_state_id(custom_end_state, States),
  case StopID of 
    
    no_such_state ->
      %% setup statefunmap (run/main/stopping) 
      StateFunMap = #{%% init  
                      -2 => InitClauses,
                      %% run state
                      -1 => RunClauses,
                      %% main -- this is just a placeholder
                      0 => []
                    };

        _ -> 
          %% get stop snippet (specifically needed later on)
          StopClauses = special_snippet_clauses(stop, States),
          %% setup statefunmap (run/main/stopping) 
          StateFunMap = #{%% init  
                          -2 => InitClauses,
                          %% run state
                          -1 => RunClauses,
                          %% main -- this is just a placeholder
                          0 => [],
                          %% stop state
                          StopID => StopClauses
                        }
  end,


  %% get single next state edge
  RelevantEdges = get_outgoing_edges(StateID,Edges),
  ?assert(length(RelevantEdges)==1),

  %% get edge 
  Edge = to_map(lists:nth(1,RelevantEdges)),
  %% build from next state 
  #{to:=ToStateID} = Edge,
  %% get next state
  #{ToStateID:=ToState} = States,

  %% reverse directions of rec map
  RevRecMap = maps:fold(fun(K,V,Rec) -> maps:put(V,K,Rec) end, #{}, RecMap),

  %% get continuation
  ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {0, 1}, Edges, States, RevRecMap, StateFunMap),
  % ContinuationSnippet = states(ToState, ToStateID, {Scope, ScopeID}, {DataIndex, NextDataIndex}, Edges, States, RecMap, StateFunMap),
  %% unpack 
  {Continuation, NextFunMap, RecMap1} = ContinuationSnippet,

  ?VSHOW("(~p :: ~p)\nContinuation:\t~p.",[{ScopeID,Scope},{StateID,State},Continuation]),
  ?VSHOW("(~p :: ~p)\nNextFunMap:\t~p.",[{ScopeID,Scope},{StateID,State},NextFunMap]),
  ?VSHOW("(~p :: ~p)\nRecMap1:\t~p.",[{ScopeID,Scope},{StateID,State},RecMap1]),

  %% add comments to snippets
  MainComments = ["%% @doc the main loop of the stub implementation.",
                  "%% CoParty is the process ID corresponding to either:",
                  "%%   (1) the other party in the session;",
                  "%%   (2) if this process is monitored, then the transparent monitor.",
                  "%% Data is a map that accumulates messages received, sent and keeps track of timers as they are set.",
                  "%% Any loops are implemented recursively, and have been moved to their own function scope.",
                  "%% @see stub.hrl for further details and the functions themselves."],
                  
  Snippets = erl_syntax:add_precomments(lists:map(fun(Com) -> 
    erl_syntax:comment([Com]) 
  end, MainComments), ?Q(["(CoParty, Data) -> "]++Continuation)),

  %% add the snippets within the body of main scope
  ExitFunMap = maps:put(0,[{false, main, [ Snippets ]}],NextFunMap),
  ?VSHOW("(~p :: ~p)\nExitFunMap:\t~p.",[{ScopeID,Scope},{StateID,State},ExitFunMap]),

  %% check that all rec in rec map have been finished unfolding
  FinishedUnfolding = maps:fold(fun(_K,Rec,Unfolding) ->
    case Rec of
      {unfolded, RecState, _} -> Unfolding and true;
      _ -> Unfolding and false
  end end, true, RecMap1),
  %% warn if not all were unfolded
  case FinishedUnfolding of true -> ok; 
    _ -> ?SHOW("Warning, it looks like one (or more) of the recursive variables did not finish unfolding, perhaps there is a mistake in the protocol?\nRecMap:\t~p.",[RecMap1])
  end,
  %% pack as final recmap
  ExitRecMap = RecMap1,

  ?VSHOW("(~p :: ~p)\nExitRecMap:\t~p.",[{ScopeID,Scope},{StateID,State},ExitRecMap]),

  %% return (snippets empty because no scope beyond this)
  ExitFunMap.
%%


%% @doc for general states
%% @returns tuple containing: 
%% (1) list of strings for text of current scope within program.
%% (2) map of state functions, between scopeIDs and program text.
%% (3) recmap, updated with the information of the current unfoldings.
-spec state(atom(), integer(), {atom(), integer()}, {list(), list()}, list(), map(), map(), map()) -> {list(), map(), map()}.

state(State, StateID, {InScope, InScopeID}, {PrevDataIndex, StateDataIndex}, Edges, States, RecMap, FunMap) ->

  %% create state datas
  PrevData = state_data(PrevDataIndex),
  StateData = state_data(StateDataIndex),

  ?VSHOW("building new state.\nstate:\t~p => ~p,\nscope:\t~p => ~p,\ndata:\t~p => ~p.",[StateID,State,InScopeID,InScope,PrevData,StateData]),
  
  % ?VSHOW("\nedges:\t~p,\nstates:\t~p,\nrecmap:\t~p,\nfunmap:\t~p.",[Edges,States,RecMap,FunMap]),

  %% check if recursive state, and if already explored
  InitRecState = maps:get(StateID,RecMap,not_recursive_state),
  case InitRecState of 

    %% if already unfolding, just insert callback
    {already_unfolding, _Var, StrFun} -> 
      RecMap1 = RecMap,
      %% keep same scope
      Scope = InScope,
      ScopeID = InScopeID,
      %% return call to function
      ExitSnippet = [StrFun++"(CoParty, "++PrevData++")"], 
      ExitFunMap = FunMap, 
      ExitRecMap = RecMap,
      
      ?VSHOW("(~p :: ~p), is already unfolding, just insert callback,\nExitSnippet:\t~p.",[{InScopeID,InScope},{StateID,State},ExitSnippet]);

    %% otherwise, proceed
    _ -> 
      %% but mark in the RecMap for any proceeding states explored
      case InitRecState of 

        %% not a recursive state
        not_recursive_state -> 
          RecMap1 = RecMap,
          %% keep same scope/id
          ScopeID = InScopeID,
          Scope = InScope,
          
          ?VSHOW("(~p :: ~p), not_recursive_state,\nRecMap:\t~p.",[{InScopeID,InScope},{StateID,State},RecMap]);

        %% is a recursive state, but not in new scope so restart
        _ -> 
          ?VSHOW("(~p :: ~p), updating recstate (~p) as already_unfolding.",[{InScopeID,InScope},{StateID,State},InitRecState]),
          ?assert(InScopeID=/=StateID),
          RecMap1 = maps:put(StateID,{already_unfolding,InitRecState,"loop_state"++integer_to_list(StateID)},RecMap),
          %% mark scope/id to be new,
          ScopeID = StateID,
          Scope = State

      end,

      ?VSHOW("(~p :: ~p), entering main build phase.",[{ScopeID,Scope},{StateID,State}]),

      %% get edges relevant to current state
      RelevantEdges = get_outgoing_edges(StateID,Edges),

      %% explore from state
      case State of 

        if_then_else_state -> 
          ?assert(length(RelevantEdges)==2),

          %% need to sort them by which needs the timer to complete or not
          %% inspect top, see if it is negated or not
          A = to_map(lists:nth(1,RelevantEdges)),
          B = to_map(lists:nth(2,RelevantEdges)),

          %% for both A and B, most of the following is shared, with the only differing info being is_if and is_else
          #{is_if:=AIsIf,is_else:=AIsElse,edge_data:=#{if_stmt:=#{is_timer:=IsTimer,ref:=Ref,is_not:=IsNot}}} = A,
          ?assert(not(AIsIf=:=AIsElse)),

          %% determine which is top and bot
          case IsNot of 
            true -> %% put else in top
              case AIsIf of 
                true -> 
                  Top = B,
                  Bot = A;
                _ -> 
                  Top = A,
                  Bot = B
              end;
            _ -> %% put if in top
              case AIsIf of 
                true -> 
                  Top = B,
                  Bot = A;
                _ -> 
                  Top = A,
                  Bot = B
              end
          end,

          %% get top state info
          #{to:=TopToStateID} = Top,
          TopToState = maps:get(TopToStateID, States),

          %% get continuation of top
          {TopContinuation, TopNextFunMap, TopRecMap} = state(TopToState, TopToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, FunMap),

          %% get bot state info
          #{to:=BotToStateID} = Bot,
          BotToState = maps:get(BotToStateID, States),

          %% get continuation of bot
          {BotContinuation, NextFunMap, BotRecMap} = state(BotToState, BotToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, TopNextFunMap),

          %% generate
          case IsTimer of 
            true -> ok;
            _ -> 
              error(only_timers_are_supported_with_if_statements) %% unsupported
          end,

          {Snippet, StateFuns} = snippet({if_then, Ref, TopContinuation, BotContinuation}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps([TopRecMap,BotRecMap]);


        delay_state -> 
          ?assert(length(RelevantEdges)==1),
          %% get edge 
          Edge = to_map(lists:nth(1,RelevantEdges)),
          %% depends on case of single direction kind
          #{to:=ToStateID,edge_data:=#{delay:=#{ref:=Duration}}} = Edge,  
          %% get next state
          #{ToStateID:=ToState} = States,
        
          %% get continuation
          ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, FunMap),
          %% unpack 
          {Continuation, NextFunMap, RecMap2} = ContinuationSnippet,
          
          {Snippet, StateFuns} = snippet({delay, Duration, Continuation}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards
          FinalRecMap = RecMap2;


        timer_start_state -> 
          ?assert(length(RelevantEdges)==1),
          %% get edge 
          Edge = to_map(lists:nth(1,RelevantEdges)),
          %% depends on case of single direction kind
          #{to:=ToStateID,edge_data:=#{timer:=#{duration:=Duration,name:=Name}}} = Edge,  
          %% get next state
          #{ToStateID:=ToState} = States,
        
          %% get continuation
          ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, FunMap),
          %% unpack 
          {Continuation, NextFunMap, RecMap2} = ContinuationSnippet,
          
          {Snippet, StateFuns} = snippet({set_timer, Name, Duration, Continuation}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards
          FinalRecMap = RecMap2;


        standard_state -> 
          ?assert(length(RelevantEdges)==1),
          %% get edge 
          Edge = to_map(lists:nth(1,RelevantEdges)),
          %% depends on case of single direction kind
          #{to:=ToStateID,edge_data:=#{trans_type:=Kind}} = Edge,
          %% get next state
          #{ToStateID:=ToState} = States,
          %% get continuation
          ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, FunMap),
          %% unpack 
          {Continuation, NextFunMap, RecMap2} = ContinuationSnippet,

          %% string helpers
          Label = get_msg_label(Edge),
          %% depending on direction
          {Snippet, StateFuns} = case Kind of 
            send -> 
              snippet({send_blocking_payload, Label, Continuation}, {StateID, ScopeID}, {PrevData, StateData});

            recv -> %% branching, but package as map and list
              snippet({branching, [list_to_atom(Label)], #{Label=>Continuation}}, {StateID, ScopeID}, {PrevData, StateData});

            _ -> 
              ?SHOW("(~p :: ~p), unexpected direction, Kind:\t~p.",[{ScopeID,Scope},{StateID,State},Kind]),
              error(unexpected_direction_kind)
          end,

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards
          FinalRecMap = RecMap2;


        branch_state -> 
          ?assert(length(RelevantEdges)>0),

          %% map the continuation of each edge to the label
          {ContinuationMap, NextFunMap, Labels, RecMaps} = explore_states(RelevantEdges, StateDataIndex, Scope, ScopeID, Edges, States, RecMap1, FunMap),

          {Snippet, StateFuns} = snippet({branching, Labels, ContinuationMap}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps(RecMaps);


        select_state -> 
          ?assert(length(RelevantEdges)>0),

          %% map the continuation of each edge to the label
          {ContinuationMap, NextFunMap, Labels, RecMaps} = explore_states(RelevantEdges, StateDataIndex, Scope, ScopeID, Edges, States, RecMap1, FunMap),

          {Snippet, StateFuns} = snippet({blocking_selection, Labels, ContinuationMap}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps(RecMaps);


        recv_after_state -> 
          {Actions, Silents} = separate_edges(RelevantEdges),
          ?assert(length(Actions)==1),
          ?assert(length(Silents)==1),
          
          %% map the continuation of each edge to the label
          {ContinuationMap, NextFunMap, Labels, RecMaps} = explore_states(Actions, StateDataIndex, Scope, ScopeID, Edges, States, RecMap1, FunMap),

          %% only one silent transition
          Silent = to_map(lists:nth(1,Silents)),
          #{to:=TimeoutStateID,edge_data:=#{timeout:=#{ref:=Duration}}} = Silent,
          %% get timeout
          TimeoutState = maps:get(TimeoutStateID, States),
          TimeoutSnippet = state(TimeoutState, TimeoutStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, NextFunMap),
          %% unpack (ignoring recmap)
          {TimeoutContinuation, TimeoutFunMap, TimeoutRecMap2} = TimeoutSnippet,

          %% depending on timeout ref
          case is_integer(Duration) of

            true -> 
              {Snippet, StateFuns} = snippet({timeout, Labels, ContinuationMap, Duration, TimeoutContinuation}, {StateID, ScopeID}, {PrevData, StateData});
            
            _ -> %% add timeout as branch
              TimeoutMap = maps:put(Duration,{is_timeout,Duration,["{timeout, "++tag_state_thing("TID_"++Duration++"_",StateID)++", "++Duration++"} -> "],TimeoutContinuation},ContinuationMap),
              %% add timer to labels
              TimeoutLabels = Labels++[list_to_atom(Duration)],
              %% get snippet
              {Snippet, StateFuns} = snippet({branching, TimeoutLabels, TimeoutMap}, {StateID, ScopeID}, {PrevData, StateData})

          end,

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(TimeoutFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps(RecMaps++[TimeoutRecMap2]);


        branch_after_state -> %% ? similar to recv_after_state
          {Actions, Silents} = separate_edges(RelevantEdges),
          ?assert(length(Actions)>0),
          ?assert(length(Silents)==1),
          
          %% map the continuation of each edge to the label
          {ContinuationMap, NextFunMap, Labels, RecMaps} = explore_states(Actions, StateDataIndex, Scope, ScopeID, Edges, States, RecMap1, FunMap),

          %% only one silent transition
          Silent = to_map(lists:nth(1,Silents)),
          #{to:=TimeoutStateID,edge_data:=#{timeout:=#{ref:=Duration}}} = Silent,
          %% get timeout
          TimeoutState = maps:get(TimeoutStateID, States),
          TimeoutSnippet = state(TimeoutState, TimeoutStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, NextFunMap),
          %% unpack (ignoring recmap)
          {TimeoutContinuation, TimeoutFunMap, TimeoutRecMap2} = TimeoutSnippet,

          %% depending on timeout ref
          case is_integer(Duration) of

            true -> 
              {Snippet, StateFuns} = snippet({timeout, Labels, ContinuationMap, Duration, TimeoutContinuation}, {StateID, ScopeID}, {PrevData, StateData});
            
            _ -> %% add timeout as branch
              TimeoutMap = maps:put(Duration,{is_timeout,Duration,["{timeout, "++tag_state_thing("TID_"++Duration++"_",StateID)++", "++Duration++"} -> "],TimeoutContinuation},ContinuationMap),
              %% add timer to labels
              TimeoutLabels = Labels++[list_to_atom(Duration)],
              %% get snippet
              {Snippet, StateFuns} = snippet({branching, TimeoutLabels, TimeoutMap}, {StateID, ScopeID}, {PrevData, StateData})

          end,

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(TimeoutFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps(RecMaps++[TimeoutRecMap2]);


        send_after_state -> 
          {Actions, Silents} = separate_edges(RelevantEdges),
          ?assert(length(Actions)==1),
          ?assert(length(Silents)==1),
          
          %% only one silent transition
          Silent = to_map(lists:nth(1,Silents)),
          #{to:=TimeoutStateID,edge_data:=#{timeout:=#{ref:=Duration}}} = Silent,
          %% get timeout
          TimeoutState = maps:get(TimeoutStateID, States),
          TimeoutSnippet = state(TimeoutState, TimeoutStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, FunMap),
          %% unpack (ignoring recmap)
          {TimeoutContinuation, TimeoutFunMap, TimeoutRecMap2} = TimeoutSnippet,


          %% get edge 
          Edge = to_map(lists:nth(1,Actions)),
          %% depends on case of single direction kind
          #{to:=ToStateID,edge_data:=#{trans_type:=Kind}} = Edge,
          %% get next state
          #{ToStateID:=ToState} = States,
          %% get continuation
          ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, TimeoutFunMap),
          %% unpack 
          {Continuation, NextFunMap, RecMap2} = ContinuationSnippet,

          %% string helpers
          Label = get_msg_label(Edge),

          {Snippet, StateFuns} = snippet({nonblocking_payload, Label, Continuation, Duration, TimeoutContinuation}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(NextFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps([RecMap2,TimeoutRecMap2]);


        select_after_state -> %% ? similar to send_after_state
          {Actions, Silents} = separate_edges(RelevantEdges),
          ?assert(length(Actions)>0),
          ?assert(length(Silents)==1),
          
          %% map the continuation of each edge to the label
          {ContinuationMap, NextFunMap, Labels, RecMaps} = explore_states(Actions, StateDataIndex, Scope, ScopeID, Edges, States, RecMap1, FunMap),

          %% only one silent transition
          Silent = to_map(lists:nth(1,Silents)),
          #{to:=TimeoutStateID,edge_data:=#{timeout:=#{ref:=Duration}}} = Silent,
          %% get timeout
          TimeoutState = maps:get(TimeoutStateID, States),
          TimeoutSnippet = state(TimeoutState, TimeoutStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap1, NextFunMap),
          %% unpack (ignoring recmap)
          {TimeoutContinuation, TimeoutFunMap, TimeoutRecMap2} = TimeoutSnippet,

          %% get snippet
          {Snippet, StateFuns} = snippet({nonblocking_selection, Labels, ContinuationMap, Duration, TimeoutContinuation}, {StateID, ScopeID}, {PrevData, StateData}),

          %% merge state fun list with state fun map
          FinalFunMap = map_state_funs(TimeoutFunMap, StateFuns),

          %% repackage for consistency afterwards, merge all recmaps
          FinalRecMap = merge_recmaps(RecMaps++[TimeoutRecMap2]);


        error_state -> 
          ?assert(length(RelevantEdges)==1),
          %% get edge 
          Edge = to_map(lists:nth(1,RelevantEdges)),
          %% depends on case of single direction kind
          #{to:=ToStateID,edge_data:=#{error_reason:=ErrorReason}} = Edge,

          %% get call to stopping from funmap 
          StoppingFunDefs = maps:get(ToStateID,FunMap,stopping_function_undefined),
          ?assert(StoppingFunDefs=/=stopping_function_undefined),
          ?assert(length(StoppingFunDefs)>0),
          %% should all be the same enough
          {_Exported, StopFunName, _} = lists:nth(1, StoppingFunDefs),
        
          %% string helpers
          StrReason = atom_to_list(ErrorReason),
          StrStopName = atom_to_list(StopFunName),
          %% make snippet for error and calling stop
          Snippet = ["error("++StrReason++"),",
                      StrStopName++"(CoParty, "++PrevData++", "++StrReason++")"],
          %% package as necessary
          FinalFunMap = FunMap,
          FinalRecMap = RecMap1;

        custom_end_state -> 
          %% is predefined, within funmap
          StopClauses = maps:get(StateID, FunMap),
          ?assert(length(StopClauses)==2),
          %% only one is needed to get fun name
          {_Exported, Fun, _Clauses} = lists:nth(1,StopClauses),

          %% string helpers
          StrStopFun = atom_to_list(Fun),
          %% set snippet to be call to stop function
          Snippet = [StrStopFun++"(CoParty, "++PrevData++", normal)"],

          %% package as necessary
          FinalFunMap = FunMap,
          FinalRecMap = RecMap1;

        _ -> 
          ?SHOW("(~p :: ~p), unexpected state.",[{ScopeID,Scope},{StateID,State}]),
          timer:sleep(5000),
          error(unexpected_state),
          %% just to compile
          Snippet = [],
          FinalFunMap = FunMap,
          FinalRecMap = RecMap1

      end,

      ?VSHOW("(~p :: ~p), finished state & snippets.",[{ScopeID,Scope},{StateID,State}]),

      ?VSHOW("(~p :: ~p)\nSnippet:\t~p.",[{ScopeID,Scope},{StateID,State},Snippet]),
      % ?VSHOW("(~p :: ~p)\nFinalFunMap:\t~p.",[{ScopeID,Scope},{StateID,State},FinalFunMap]),
      % ?VSHOW("(~p :: ~p)\nFinalRecMap:\t~p.",[{ScopeID,Scope},{StateID,State},FinalRecMap]),


      %% check if this is a new scope
      case StateID=:=ScopeID of 

        %% exiting scope
        true -> 
          case ExitRecState = maps:get(StateID,FinalRecMap,not_recursive_state) of 

            not_recursive_state ->
              ?VSHOW("(~p :: ~p), exiting non-recursive scope.",[{ScopeID,Scope},{StateID,State}]),
              %% must be main 
              ?assert(State==main),
              ?assert(StateID==0),

              %% put snippets into relevant scope map
              ExitSnippet = Snippet,
              ExitFunMap = FinalFunMap,
              ExitRecMap = FinalRecMap;

            _ ->
              ?VSHOW("(~p :: ~p), exiting recursive scope.",[{ScopeID,Scope},{StateID,State}]),
              %% exit snippets should just be a call to this new function, using the data provided
              %% unpack
              {already_unfolding, RecState, StrRecFun} = ExitRecState,
              %% add function call to recursive scope in outer scope snippets
              ExitSnippet = [StrRecFun++"(CoParty, "++PrevData++")"],
              %% move all snippets from the states within the scope to the scope fun map
              FunAtomName = list_to_atom(StrRecFun),
              %% remember, for each ID there is a list of clause definitions (one for each number of params, each with their own name and list of clauses)
              ExitFunMap = maps:put(ScopeID,[{false, FunAtomName, [ ?Q(["(CoParty, Data) -> "]++Snippet) ]}],FinalFunMap),
              %% return updated recmap
              ExitRecMap = maps:put(StateID,{unfolded,RecState,StrRecFun},FinalRecMap)

          end;

        %% not a recursive state/scope, add as normal
        _ -> 
          %% put snippets into relevant scope map
          ExitSnippet = Snippet,
          ExitFunMap = FinalFunMap,
          ExitRecMap = FinalRecMap
      end

  end,

  ?VSHOW("(~p :: ~p),\nExitSnippet:\t~p.",[{ScopeID,Scope},{StateID,State},ExitSnippet]),
  ?VSHOW("(~p :: ~p),\nExitFunMap:\t~p.",[{ScopeID,Scope},{StateID,State},ExitFunMap]),
  ?VSHOW("(~p :: ~p),\nExitRecMap:\t~p.",[{ScopeID,Scope},{StateID,State},ExitRecMap]),

  %% return
  {ExitSnippet, ExitFunMap, ExitRecMap}.
%%

%% @doc function snippet/3 (Snippet, State, Data).
%% Snippet is specific to each clause, and often will require the inner-clauses derived from another snippet function.
%% This approach is intended for a depth-first, tail-recursive approach.
%%
%% The first tuple is specific to each snippet.
%% State, StateID corresponds to the current state.
%% State, ScopeID corresponds to the state of the current function (-1 for main).
%% Data, InData is what ths state receives upon entering.
%% Data, OutData is what the state may result in using upon leaving.
%%
%% @returns clause list (with comments? --maybe) and next StateData (if changed).
-spec snippet(tuple(), {integer(), integer()}, {string(), string()}) -> {map(), string(), list()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% time-sensitive if-statements & delays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc if-statements
%% for if_not statements, just inverse the True or False variables when called.
snippet({if_then, Timer, True, False}, {StateID, ScopeID}, {InData, OutData}) -> 
  %% string helpers
  StrTimer = string_wrap(Timer),
  StrTimerVar = tag_state_thing("TID_"++StrTimer++"_",StateID),

  %% build clause
  Clause = ["receive {timeout, "++StrTimerVar++", "++StrTimer++"} -> ",
            "temp_vshow(\"took branch for timer (~p) completing.\", ["++StrTimer++"], "++InData++"),"
            ]++True++["after 0 -> ",
                       "temp_vshow(\"took branch for timer (~p) still running.\", ["++StrTimer++"], "++InData++"),"]++False++[" end"],

  %% add comments
  _Commented = erl_syntax:comment(["% ."]),


  %% wrap in get_timer
  {Snippet, StateFuns} = snippet({get_timer, Timer, Clause}, {StateID, ScopeID}, {InData, OutData}),

  %% return with comments and state funs
  {Snippet, StateFuns};
%%


%% @doc delays
snippet({delay, Duration, Continuation}, {StateID, ScopeID}, {InData, _OutData})
when (is_list(Duration) or is_integer(Duration)) and is_list(Continuation) -> 
  %% string helpers
  StrDuration = string_wrap(Duration),

  %% build clause
  Clause = case is_integer(Duration) of 
    true -> %% is hard value
      ["temp_vshow(\"delay (~p).\", ["++StrDuration++"], "++InData++"),",
       "timer:sleep("++StrDuration++"),"]++Continuation;
    _ -> %% is timer
      StrTimer = tag_state_thing("TID_"++StrDuration++"_",StateID),
      %% get timer from data, wait until receive from it
      ["receive {timeout, "++StrTimer++", "++StrDuration++"} -> ",
      %% actually, just assert that the timer matches
       "temp_assert("++StrTimer++"=:=get_timer("++StrDuration++", "++InData++")),",
       "temp_vshow(\"timer (~p) finished delay.\", ["++StrDuration++"], "++InData++"),"]++Continuation++[" end"]
  end,
  
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, []};
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% receptions & timeouts
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc for integer timeouts.
snippet({timeout, Labels, Continuations, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and is_map(Continuations) and is_integer(Duration) and is_list(Timeout) -> 
  %% string helpers
  StrDuration = integer_to_list(Duration),
  %% tail is just the timeout wrapped in after end
  Tail = ["after "++StrDuration++" -> "]++Timeout++[" end"],

  %% get the branching for the rest of the branches
  {Branches, BranchFunMap} = snippet({branching, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData}),
  %% then, insert before the last occurance of "end" in Branches
  ?assert(lists:last(Branches)==" end"),
  %% if integer, then we will use after, which requires no ';' on previous term
  BHead = lists:droplast(Branches),
  case lists:last(BHead)=="; " of
    true -> Head = lists:droplast(BHead);
    _ -> Head = BHead
  end,
  
  %% build total clause
  Clause = Head++Tail,
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, BranchFunMap};
%%


%% @doc for any other kind of reception. (including single receptions)
%% expects a timeout via a timer to be already wrapped inside the labels and continuations
snippet({branching, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and is_map(Continuations) -> 
  %% make string helpers
  StrPayload = tag_state_thing("Payload",StateID),
  StrLabelVar = tag_state_thing("Label",StateID),
  %% line by line clause for snippet
  Clause = ["temp_show(\"waiting to recv.\", [], "++InData++"),",
            "receive "]++lists:foldl(fun(Label, Cases) -> 
              StrLabel = atom_to_list(Label),
              %% connecter is previous cases, with ';' if needed
              Conn = case length(Cases)>0 of true -> Cases++["; "]; _ -> Cases end,
              %% get special head (if timer timeout)
              Special = maps:get(StrLabel, Continuations),
              case Special of 
                {is_timeout, _Timer, Head, Tail} -> %% is a timeout
                  ?VSHOW("\n\n_Timer:\t~p,\nHead:\t~p,\nTail:\t~p.",[_Timer,Head,Tail]),
                  ok;

                _ -> %% not timeout
                  Head = ["{CoParty, "++StrLabel++"="++StrLabelVar++", "++StrPayload++"} -> "],
                  Tail = Special
              end,
              %% join all together
              Conn++Head++[OutData++" = save_msg(recv, "++StrLabel++", "++StrPayload++", "++InData++"),",
                           "temp_show(\"recv ~p: ~p.\", ["++StrLabelVar++", "++StrPayload++"], "++OutData++"),"] ++ Tail
            end, [], Labels)++[" end"],

  %% add comments
  _Commented = erl_syntax:comment(["% ."]),

  %% double check if there was a timer (if so, wrap all in get_timer)
  PossibleTimeout = maps:to_list(maps:filter(fun(K,V) -> case V of {is_timeout, Timer, Head, Tail} -> true; _ -> false end end, Continuations)),

  case length(PossibleTimeout)>0 of 
    true -> %% must add "get timer" to beginning
      ?assert(length(PossibleTimeout)==1),
      {_K, {is_timeout, Timer, _Head, _Tail}} = lists:nth(1,PossibleTimeout),
      {Snippet, StateFuns} = snippet({get_timer, Timer, Clause}, {StateID, ScopeID}, {InData, OutData});
    
    _ -> %% not timer
      Snippet = Clause,%#{clause=>Clause,comment=>Comment},
      StateFuns = []
  end,

  %% return with comments and state funs
  {Snippet, StateFuns};
%%


%%%%%%%%%%%%%%%%%%%%%%%%
%%% payload & selection
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc snippet for (potentially) blocking payload (or, for sends without a timeout)
%% requires the name of the variable to be used for the payload to be predefined
snippet({send_blocking_payload, Label, Continuation}, {StateID, ScopeID}, {InData, OutData})
when is_list(Label) -> 
  %% make string helpers
  StrFun = tag_state_thing("get_payload",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = [StrPayload++" = "++StrFun++"("++Label++", "++InData++"),",
            "CoParty ! {self(), "++Label++", "++StrPayload++"},",
            OutData++" = save_msg(send, "++Label++", "++StrPayload++", "++InData++"),",
            "temp_show(\"sent "++Label++".\", [], "++OutData++"),"]++Continuation,
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, [{payload_fun,{StrFun,list_to_atom(Label++"b")}}]};
%%


%% @doc snippet for (potentially) blocking selection (or, for sends without a timeout)
%% requires the name of the variable to be used for the payload to be predefined
snippet({blocking_selection, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and is_map(Continuations) -> 
  %% make string helpers
  StrLabels = "["++atom_list_to_list(Labels)++"]",
  StrSelection = tag_state_thing("Selection",StateID),
  StrFun = tag_state_thing("make_selection",StateID),
  StrLabelVar = tag_state_thing("Label",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = ["{"++StrLabelVar++", "++StrPayload++"} = "++StrFun++"("++StrLabels++", "++InData++"),",
            "temp_vshow(\"selection made: ~p.\",["++StrLabelVar++"], "++InData++"),",
            "case "++StrLabelVar++" of "] ++ lists:foldl(fun(Label, Cases) ->
              StrLabel = atom_to_list(Label),
              %% build new case
              Case = [StrLabel++" -> ",
                      "CoParty ! {self(), "++StrLabel++", "++StrPayload++"},",
                      OutData++" = save_msg(send, "++StrLabel++", "++StrPayload++", "++InData++"),",
                      "temp_show(\"sent "++StrLabel++".\", [], "++OutData++"),"] ++ maps:get(StrLabel,Continuations) ++ ["; "],
              %% return new case
              Cases ++ Case
            end, [], Labels) ++ [" _Err -> temp_show(\"error, unexpected selection: ~p.\", [_Err], "++InData++"),",
            "error(unexpected_label_selected)",
            "end"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, [{selection_fun,{StrFun,Labels}}]};
%%


%% @doc snippet for obtaining non-blocking payloads.
%% Label is an string.
%% Continuation is the continuation of the Label.
%% Duration is either an integer or string (referencing a timer) for the selection to complete within.
%% Timeout is the continuation for if the payload takes too long to return.
snippet({nonblocking_payload, Label, Continuation, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_list(Label) and (is_list(Duration) or is_integer(Duration)) and is_list(Continuation) and is_list(Timeout) -> 
  %% make string helpers
  StrDuration = case is_integer(Duration) of true -> integer_to_list(Duration); _ -> Duration end,
  StrFun = tag_state_thing("get_payload",StateID),
  StrLabelVar = tag_state_thing("Label",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = ["Await"++StrPayload++" = nonblocking_payload(fun "++StrFun++"/1, {"++Label++", "++InData++"}, self(), "++StrDuration++", "++InData++"),"
            "temp_vshow(\"waiting for payload to be returned.\", [], "++InData++"),",
            "receive {Await"++StrPayload++", ok, {"++StrLabelVar++", "++StrPayload++"}} -> ",
            "temp_vshow(\"payload obtained: (~p: ~p).\",["++StrLabelVar++","++StrPayload++"], "++InData++"),",
            "CoParty ! {self(), "++Label++", "++StrPayload++"},",
            OutData++" = save_msg(send, "++Label++", "++StrPayload++", "++InData++"),"
           ]++Continuation++["; ",
            "{Await"++StrPayload++", ko} -> ",
            "temp_vshow(\"unsuccessful payload. (probably took too long)\", [], "++InData++"),"]++Timeout++[" end"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, [{payload_fun,{StrFun,list_to_atom(Label++"a")}}]};
%%


%% @doc snippet for non-blocking selection.
%% Labels is a list of string labels, corresponding to the options in the selection.
%% Continuations is a map between atom labels and their continuation snippets.
%% Duration is either an integer or string (referencing a timer) for the selection to complete within.
%% Timeout is the continuation of if the selection cannot be made.
snippet({nonblocking_selection, Labels, Continuations, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and (is_list(Duration) or is_integer(Duration)) and is_map(Continuations) and is_list(Timeout) -> 
  %% make string helpers
  StrLabels = "["++atom_list_to_list(Labels)++"]",
  StrDuration = case is_integer(Duration) of true -> integer_to_list(Duration); _ -> Duration end,
  StrSelection = tag_state_thing("Selection",StateID),
  StrFun = tag_state_thing("make_selection",StateID),
  StrLabelVar = tag_state_thing("Label",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = [StrSelection++" = "++StrLabels++",",
            "Await"++StrSelection++" = nonblocking_selection(fun "++StrFun++"/1, {"++StrSelection++", "++InData++"}, self(), "++StrDuration++", "++InData++"),"
            "temp_vshow(\"waiting for selection to be made (out of: ~p).\", ["++StrSelection++"], "++InData++"),",
            "receive {Await"++StrSelection++", ok, {"++StrLabelVar++", "++StrPayload++"}} -> ",
            "temp_vshow(\"selection made: ~p.\",["++StrLabelVar++"], "++InData++"),",
            "case "++StrLabelVar++" of "] ++ lists:foldl(fun(Label, Cases) ->
              StrLabel = atom_to_list(Label),
              %% build new case
              Case = [StrLabel++" -> ",
                      "CoParty ! {self(), "++StrLabel++", "++StrPayload++"},",
                      OutData++" = save_msg(send, "++StrLabel++", "++StrPayload++", "++InData++"),",
                      "temp_show(\"sent "++StrLabel++".\", [], "++OutData++"),"] ++ maps:get(StrLabel,Continuations) ++ ["; "],
              %% return new case
              Cases ++ Case
            end, [], Labels) ++ [" _Err -> temp_show(\"error, unexpected selection: ~p.\", [_Err], "++InData++"),",
            "error(unexpected_label_selected)",
            "end;",
            "{Await"++StrSelection++", ko} -> ",
            "temp_vshow(\"unsuccessful selection. (probably took too long)\", [], "++InData++"),"]++Timeout++[" end"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, [{selection_fun,{StrFun,Labels}}]};
%%


%%%%%%%%%%%
%%% timers
%%%%%%%%%%%

%% @doc snippet for getting timers
snippet({get_timer, StrName, Continuation}, {StateID, ScopeID}, {InData, _OutData})
when is_list(StrName) ->
  %% string helpers
  % Name = list_to_atom(StrName),
  StrVar = tag_state_thing("TID_"++StrName++"_",StateID),
  %% line by line clause for snippet
  Clause = [StrVar++" = get_timer("++StrName++", "++InData++"),"]++Continuation,
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, []};
%%


%% @doc snippet for setting timers
snippet({set_timer, StrName, Duration, Continuation}, {StateID, ScopeID}, {InData, OutData})
when is_list(StrName) and is_integer(Duration) ->
  %% string helpers
  StrVar = tag_state_thing("TID_"++StrName++"_",StateID),
  StrDuration = integer_to_list(Duration),
  %% line by line clause for snippet
  Clause = [OutData++" = set_timer("++StrName++", "++StrDuration++", "++InData++"),",
            "temp_vshow(\"set timer "++StrName++" with duration: "++StrDuration++".\", [], "++OutData++"),"]++Continuation,
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments and state funs
  {Snippet, []};
%%


%% @doc stopping snippet
%% @returns a fully commented (?Q) function definition for stopping function
snippet({stop_state}, _IDs, _Stuff) -> 
  Name = stopping,
  StrName = atom_to_list(Name),

  %%
  ClauseDefault = merl_commented(pre, [
      "%% @doc Adds default reason 'normal' for stopping.",
      "%% @see "++atom_to_list(Name)++"/3."
  ],?Q([
      "(CoParty, Data) -> ",
      "temp_vshow(\"\nData:\t~p.\",[Data],Data),",
      StrName++"(normal, CoParty, Data)"
    ])),

  %%
  ClauseNormal = merl_commented(pre, [
      "%% @doc Adds default reason 'normal' for stopping.",
      "%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}."
  ],?Q([
    "(normal=_Reason, _CoParty, _Data) -> ",
      "temp_vshow(\"stopping normally.\",[],_Data),",
      "exit(normal)"
    ])),

  %%
  ClauseError = merl_commented(pre, [
      "%% @doc stopping with error.",
      "%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.",
      "%% @param CoParty is the process ID of the other party in this binary session.",
      "%% @param Data is a list to store data inside to be used throughout the program."
  ],?Q([
    "({error, Reason, Details}, _CoParty, _Data) ",
      "when is_atom(Reason) -> ",
      "temp_vshow(\"error, stopping...\nReason:\t~p,\nDetails:\t~p,\nCoParty:\t~p,\nData:\t~p.\",[Reason,Details,_CoParty,_Data],_Data),",
      "erlang:error(Reason, Details)"
    ])),

  %%
  ClausePartialError = merl_commented(pre, [
      "%% @doc Adds default Details to error."
  ],?Q([
    "({error, Reason}, CoParty, Data) ",
      "when is_atom(Reason) -> ",
      "temp_vshow(\"error, stopping...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.\",[Reason,_CoParty,_Data],_Data),",
      StrName++"({error, Reason, []}, CoParty, Data)"
    ])),

  %%
  ClauseUnknown = merl_commented(pre, [
      "%% @doc stopping with Unexpected Reason."
  ],?Q([
    "(Reason, _CoParty, _Data) ",
      "when is_atom(Reason) -> ",
      "temp_vshow(\"unexpected stop...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.\",[Reason,_CoParty,_Data],_Data),",
      "exit(Reason)"
    ])),

  %% clausedefault uses different multi-despatch
  Clauses = [ClauseNormal, ClauseError, ClausePartialError, ClauseUnknown],

  % ?VSHOW("stop_state, Clauses:\n\t~p.",Clauses),

  {[], [{stop_fun, [{false, Name, [ClauseDefault]},{?EXPORT_DEFAULT,Name,Clauses}]}]};
%%


%% @doc init snippet
%% @returns a fully commented (?Q) function definition 
snippet({init_state}, _IDs, _Stuff) -> 
  Name = init,
  StrName = atom_to_list(Name),

  % CommVerbose = merl:var("%% set printout to be verbose."),

  Clause = merl_commented(pre, [
      "% @doc Called to finish initialising process.",
      "% @see stub_init/1 in `stub.hrl`.",
      "% If this process is set to be monitored (i.e., temp_monitored()=:=true) then, in the space indicated below setup options for the monitor may be specified, before the session actually commences.",
      "% Processes wait for a signal from the session coordinator (SessionID) before beginning."
    ],?Q([
      "(Args) -> ",
      "printout(\"args:\n\t~p.\",[Args],Args),",
      % "\n",
      "{ok,Data} = stub_init(Args),",
      "printout(\"data:\n\t~p.\",[Data],Data),",
      % "\n",
      "CoParty = maps:get(coparty_id,Data),",
      "SessionID = maps:get(session_id,Data),",
      % "\n",
      "case (temp_monitored()=:=true) of ",
      "true -> ",
      % "%% add calls to specify behaviour of monitor here.",
      % "\n",
      % "%% set printout to be verbose.",%"_@CommVerbose",
      "CoParty ! {self(), setup_options, {printout, #{enabled=>true,verbose=>true,termination=>true}}},",
      % "\n",  
      "CoParty ! {self(), ready, finished_setup},",
      "temp_vshow(\"finished setting options for monitor.\",[],Data);",
      "_ -> ok",
      "end,",
      % "\n",
      % "%% wait for signal from session",
      "receive {SessionID, start} -> ",
      "temp_show(\"received start signal from session.\",[],Data),",
      "run(CoParty, Data)",
      "end"
    ])),

  ?VSHOW("init_state, Clause:\n\t~p.",[Clause]),

  {[], [{init_fun, [{true, Name, [Clause]}]}]};
%%


%% @doc stopping snippet
%% @returns a fully commented (?Q) function definition 
snippet({run_state}, _IDs, _Stuff) -> 
  Name = run,
  StrName = atom_to_list(Name),

  Main = main,
  StrMain = atom_to_list(Main),

  Clause1 = merl_commented(pre, [
      "% @doc Adds default empty list for Data.",
      "% @see "++StrName++"/2."
    ],?Q([
      "(CoParty) -> ",
      "Data = #{coparty_id=>CoParty,timers=>#{},msgs=>#{},logs=>#{}},",
      "temp_vshow(\"using default Data.\",[],Data),",
      StrName++"(CoParty, Data)"
    ])),

  Clause2 = merl_commented(pre, [
      "% @doc Called immediately after a successful initialisation.",
      "% Add any setup functionality here, such as for the contents of Data.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a map that accumulates data as the program runs, and is used by a lot of functions in `stub.hrl`.",
      "% @see stub.hrl for helper functions and more."
    ],?Q([
      "(CoParty, Data) -> ",
      "temp_do_show(\"running...\nData:\t~p.\n\",[Data],Data),",
      StrMain++"(CoParty, Data)"
    ])),

  % ?VSHOW("run_state, Clause1:\n\t~p.",[Clause1]),
  % ?VSHOW("run_state, Clause2:\n\t~p.",[Clause2]),

  %% Clause1 is wrapper for only one param, and redurects to Clause2
  %% Clause2 redirects to main(CoParty,Data)
  {[], [{run_fun, [{true, Name, [Clause1]},{true, Name, [Clause2]}]}]};
%%




%% @doc unhandled snippet requested.
snippet(Unhandled, IDs, Stuff) -> 
  ?SHOW("unhandled:\t~p,\nIDs:\t~p,\nstuff:\t~p.",[Unhandled,IDs,Stuff]),
  timer:sleep(5000),
  error(unhandled_snippet).
%%



%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%

-spec special_snippet_clauses(atom(), map()) -> list().

%% @doc formats special state funs correctly, for pre-generation.
special_snippet_clauses(Kind, States) ->
  State = list_to_atom(atom_to_list(Kind)++"_state"),
  Fun = list_to_atom(atom_to_list(Kind)++"_fun"),

  {EmptySnippet, FunList} = snippet({State}, {0,0}, {"",""}),
  ?assert(length(EmptySnippet)==0),
  ?assert(length(FunList)==1),
  
  {Fun1, Clauses} = lists:nth(1,FunList),
  ?assert(Fun=:=Fun1),

  %% return
  Clauses.
%%

%% @doc returns stateID (key) from given state (value) in map of states
get_state_id(State, States) 
when is_atom(State) and is_map(States) ->
  StateIDs = maps:keys(maps:filter(fun(K,V) -> V=:=State end,States)),
  ?VSHOW("\nStates:\t~p,\nState:\t~p,\nStateIDS:\t~p.",[States, State, StateIDs]),
  case length(StateIDs)==0 of 
    true -> %% this is likely a recursive protocol
      ?VSHOW("no (~p) found, this is likely a recursive protocol.",[State]),
      no_such_state;
    _ -> 
      ?assert(length(StateIDs)==1),
      StateID = lists:nth(1,StateIDs),
      StateID
  end.
%%


%% @doc takes a given Thing with a suffix of the given StateID.
%% @returns a string
tag_state_thing(Thing, StateID) when is_list(Thing) and is_integer(StateID) -> Thing++integer_to_list(StateID);
tag_state_thing(Thing, StateID) when is_atom(Thing) and is_integer(StateID) -> atom_to_list(Thing)++integer_to_list(StateID).


%% @doc wraps as a string
string_wrap(Thing) when is_list(Thing) -> Thing;
string_wrap(Thing) when is_integer(Thing) -> integer_to_list(Thing);
string_wrap(Thing) when is_atom(Thing) -> atom_to_list(Thing).



%% @doc returns next state data incrememnted
state_data(Index) when is_integer(Index) ->
  case Index of 
    0 -> "Data";
    _ -> "Data"++integer_to_list(Index)
end.
%%



%% @doc returns timer name
timer_name(Name) when is_list(Name) -> "timer_"++Name;
%%
timer_name(Name) when is_atom(Name) -> "timer_"++atom_to_list(Name).
%%


%% @doc merges state funs returned by snippets with map of state funs returned by state
map_state_funs(StateFunMap, StateFunList)
when is_map(StateFunMap) and is_list(StateFunList) ->
  ?VSHOW("\nStateFunMap:\t~p,\nStateFunList:\t~p.",[StateFunMap,StateFunList]),
  lists:foldl(fun({FunKind,{Name,_Args}}=Fun, Funs) 
  when is_map(Funs) and is_atom(FunKind) and is_list(Name) -> 
    AtomName = list_to_atom(Name),
    % StrArgs = case is_atom(Args) of 
    %   true -> atom_to_list(Args);
    %   _ -> "["++lists:foldl(fun(A, In) -> In++)++"]"
    % end,
    case FunKind of 
      payload_fun ->
        maps:put(AtomName, [{false, AtomName, [ ?Q(["(_Args) -> extend_with_functionality_for_obtaining_payload"]) ]}], Funs);
      selection_fun ->
        maps:put(AtomName, [{false, AtomName, [ ?Q(["(_Args) -> extend_with_functionality_for_making_selection"]) ]}], Funs);
      _ -> 
        ?VSHOW("unexpected FunKind (~p), named (~p) with args (~p).",[FunKind,Name,_Args]),
        Funs
    end
  end, StateFunMap, StateFunList).
%%

atom_list_to_list(List) when is_list(List) -> lists:foldl(fun(A,L) -> Head = case length(L)==0 of true -> ""; _ -> "," end, L++Head++atom_to_list(A) end, "", List).


%% @doc explores the edges from a given state
explore_states(RelevantEdges, StateDataIndex, Scope, ScopeID, Edges, States, RecMap, FunMap) -> 
  lists:foldl(fun(_Edge, {Cont,FMap, Ls, RecMaps}) ->
    %% get map edge
    Edge = to_map(_Edge),
    %% get next state
    #{to:=ToStateID} = Edge,
    #{ToStateID:=ToState} = States,
    %% get continuation
    ContinuationSnippet = state(ToState, ToStateID, {Scope, ScopeID}, {StateDataIndex, StateDataIndex+1}, Edges, States, RecMap, FunMap),
    %% unpack 
    %% ! (decicing to not both collecting and remerging the recmap?)
    {Continuation, NextFunMap2, RecMap} = ContinuationSnippet,

    %% string helpers
    Label = get_msg_label(Edge),
    %% add to maps
    ContMap = maps:put(Label, Continuation, Cont),
    %% merge fun maps
    FMap1 = maps:from_list(maps:to_list(NextFunMap2)++maps:to_list(FMap)),
    %% return
    {ContMap, FMap1, Ls++[list_to_atom(Label)], RecMaps++[RecMap]}
  end, {#{},#{},[], []}, RelevantEdges).
%%

%% @docs takes a list of recmaps, and merges them, allowing those with higher priority status to overwrite others.
%% unfolded > already_unfolding > _Else
merge_recmaps([H]) -> H;
merge_recmaps([H|T]) ->
  %% do tail first
  Tail = merge_recmaps(T),
  %% should have the same keys
  ?assert(maps:keys(Tail)==maps:keys(H)),
  %% for each key in head, compare the values of both
  maps:fold(fun(K,Val,Acc) ->
    case Val of 
      {unfolded, _, _} -> maps:put(K,Val,Acc);
      _ -> %% does not automatically win priotity
        case maps:get(K,Acc) of
          {unfolded, _, _} -> Acc;
          {already_unfolding, _, _} -> Acc;
          _ -> maps:put(K,Val,Acc)
        end
    end
  end, Tail, H).
  

