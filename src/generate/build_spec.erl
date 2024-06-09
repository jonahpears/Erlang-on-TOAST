%% @doc Processes Fsm (from build_fsm:to_fsm) to map() used by monitors
-module(build_spec).

-export([to_monitor_spec/1]).

-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").

-include("snippets/stub_tools.hrl").
-include("snippets/fsm_tools.hrl").

-spec to_monitor_spec({list(), map(), map()}) -> map().
to_monitor_spec(Fsm) -> 
  {Edges, States, RecMap} = Fsm,

  ?GAP(),?GAP(),?GAP(),?GAP(),?GAP(),?SHOW("starting to build monitor spec.\n\n\n",[]),?GAP(),

  ?SHOW("input fsm...\nEdges:\n\t~p,\nStates:\n\t~p,\nRecMap:\n\t~p.",[Edges,States,RecMap]),

  InitState = maps:get(0,States),
  InitSpec = #{ init => InitState,
                timeouts => #{},
                timers => #{},
                resets => #{},
                map => #{},
                states_to_resolve => #{} },

  Spec = to_spec(InitState,0,Edges,States, RecMap, InitSpec),

  ?SHOW("input fsm...\nEdges:\n\t~p,\nStates:\n\t~p,\nRecMap:\n\t~p,\nfinished spec:\n\t~p.",[Edges,States,RecMap,Spec]),

  % timer:sleep(1000),

  {ok, Spec}.

-spec to_spec(atom(), integer(), list(), map(), map(), map()) -> map().

% %% @doc 
% to_spec(State, StateID, Edges, States, Spec) ->
%   pass;

% %% @doc 
% to_spec(State, StateID, Edges, States, Spec) ->
%   pass;

% %% @doc 
% to_spec(State, StateID, Edges, States, Spec) ->
%   pass;

% %% @doc 
% to_spec(State, StateID, Edges, States, Spec) ->
%   pass;


%% @doc generic handler
to_spec(State, StateID, Edges, States, RecMap, Spec) ->
  ?GAP(),
  %% get relevant edges
  RelevantEdges = get_outgoing_edges(StateID,Edges),

  %% check for stateid in states_to_resolve, and then reverse and add current state
  InitSpec = check_resolvable_state(State,StateID,Spec),

  %% create new spec from combination of all other specs reachable via edges
  OutSpec = lists:foldl(fun(_Edge, InSpec) -> 
    %% convert to map
    Edge = to_map(_Edge),
    %% update resolvable states (that havent been resolved by current state) to next edge
    UpdatedSpec = update_resolvable_states(Edge, InSpec),
    % ?SHOW("\n(~p:~p)\nedge:\n\t~p.\n",[StateID,State,Edge]),
    %% add current state/edges to inspec
    {EdgeSpec, Resolve} = edge_spec(Edge, State, StateID, Edges, States, RecMap, UpdatedSpec),
    % ?SHOW("\n(~p:~p)\nEdgeSpec:\n\t~p.\n",[StateID,State,EdgeSpec]),
    %% get spec from edges and further
    ToStateID = maps:get(to, Edge),
    ToState = maps:get(ToStateID, States),
    %% do not explore custom_end_state
    StateSpec = case is_end_state(ToState) of
      true -> 
        % ?SHOW("\n(~p:~p) is end state.",[StateID,State]),
        EdgeSpec;
      _ -> %% check if reccursive
        RecursiveVars = get_recursive_vars(StateID, RecMap),
        %% there can only be zero or one
        ?assert(length(RecursiveVars)<2),
        case length(RecursiveVars)>0 of
          true -> %% do not explore recursive state more than once
            case maps:get(StateID,RecMap,false) of
              true -> %% already explored
                % ?SHOW("\n(~p:~p) recursion already explored (is true).",[StateID,State]),
                EdgeSpec;
              _ -> %% not been visited before
                % ?SHOW("\n(~p:~p) is new recursive state.",[StateID,State]),
                %% make sure it wont be visited next time
                RecMap1 = maps:put(StateID, true, RecMap),
                to_spec(ToState, ToStateID, Edges, States, RecMap1, EdgeSpec)
            end;
          _ -> %% is not a recursive state
            % ?SHOW("\n(~p:~p) is not a recursive state,\nRecMap:\n\t~p.",[StateID,State,RecMap]),
            to_spec(ToState, ToStateID, Edges, States, RecMap, EdgeSpec)
        end
    end,
    %% check for unresolved
    case Resolve=:=undefined of
      true -> ResolvedSpec = StateSpec;
      _ -> ResolvedSpec = resolve_spec(Resolve,Edge,State,StateID,Edges,States,RecMap,StateSpec)
    end,
    ?SHOW("\n(~p:~p)\nResolvedSpec:\n\t~p.\n",[StateID,State,ResolvedSpec]),
    % timer:sleep(1000),
    %% merge edgespec with inspec
    maps:merge_with(fun(_K, EdgeVal, InVal) -> 
      case is_map(EdgeVal) of
        true ->
          ?assert(is_map(InVal)),
          %% merge both maps
          maps:merge(InVal, EdgeVal);
        _ -> 
          %% should just be init
          ?assert(_K=:=init),
          ?assert(EdgeVal=:=InVal),
          EdgeVal
      end
    end, ResolvedSpec, EdgeSpec)
  end, InitSpec, RelevantEdges),

  ?SHOW("\n(~p:~p)\nin spec:\n\t~p,\noutspec:\n\t~p.\n",[StateID,State,Spec,OutSpec]),
  
  OutSpec.

%% @doc looks in states_to_resolve for one that corresponds to the StateID.
%% if found, it removes the entry and adds a new one pointing to the current states name.
check_resolvable_state(State, StateID, Spec) -> 
  ResolvableStates = [standard_state,recv_after_state,send_after_state,branch_state,branch_after_state,custom_end_state,if_then_else_state],
  case length(maps:keys(maps:get(states_to_resolve,Spec)))>0 of 
    true -> 
      case lists:member(State,ResolvableStates) of 
      true ->
        ?SHOW("\n(~p:~p) state is able to be used for resolve,\nSpec:\n\t~p.",[StateID,State,Spec]),
        case is_map_key(StateID,maps:get(states_to_resolve,Spec)) of 
          true -> %% get relevant state_to_resolve
            {StateToResolve,StateIDToResolve,unresolved} = maps:get(StateID,maps:get(states_to_resolve,Spec)),
            %% remove from outspec
            RemSpec = maps:remove(StateID,maps:get(states_to_resolve,Spec)),
            %% there should not be another of same name in RemSpec
            ?assert(not is_map_key(StateToResolve,RemSpec)),
            %% add back name
            Return = maps:put(states_to_resolve,maps:put(StateToResolve,{state_name(State,StateID),StateIDToResolve,resolved},RemSpec),Spec),

            ?SHOW("\n(~p:~p) resolved a state! (~p:~p)\nSpec:\n\t~p,\nReturn:\n\t~p.",[StateID,State,StateToResolve,StateIDToResolve,Spec,Return]),

            Return;
            _ -> 
            ?SHOW("\n(~p:~p) not a corresponding state waiting to be resolved,\nSpec:\n\t~p.",[StateID,State,Spec]),
            Spec
        end;
      _ -> 
        ?SHOW("\n(~p:~p) not a state capable of being resolved to,\nSpec:\n\t~p.",[StateID,State,Spec]),
        Spec
      end;
    _ -> 
      ?SHOW("\n(~p:~p) no states to resolve,\nSpec:\n\t~p.",[StateID,State,Spec]),
      Spec
  end.
%%

%% @doc 
resolve_spec(Name,#{to:=_To,is_timer:=true,is_silent:=true,edge_data:=#{timer:=#{name:=Timer,duration:=_Value}}}=_Edge, timer_start_state=_State, _StateID, _Edges, _States, _RecMap, Spec) ->
  %% should have been resolved by now
  ?assert(is_map_key(Name,maps:get(states_to_resolve,Spec))),
  %% get resolution
  ResolvedState = maps:get(Name,maps:get(states_to_resolve,Spec)),
  %% remove from states_to_resolve
  RemResolvedSpec = maps:remove(Name,maps:get(states_to_resolve,Spec)),
  RemSpec = maps:put(states_to_resolve,RemResolvedSpec,Spec),
  %% get old reset entry
  OldResets = maps:get(resets, RemSpec),
  OldResetSpec = maps:get(Name,OldResets),
  ToResolve = maps:get(list_to_atom(Timer),OldResetSpec),
  %% remove old reset entry
  RemToResolve = maps:remove(Name,OldResets),
  %% update entry 
  ResolvedResets = maps:put(ResolvedState,ToResolve,RemToResolve),
  %% return
  maps:put(resets, ResolvedResets, RemSpec).
%%

%% @doc replacesc all of the entries in state_to_resolve with (To) from edge
update_resolvable_states(#{to:=To}=_Edge, Spec) ->
  StatesToResolve = maps:fold(fun(K,V,AccIn) -> 
    case V of 
      {_,_,unresolved} -> maps:put(To, V, AccIn);
      {_,_,resolved} -> maps:put(K, V, AccIn)
    end
  end, #{}, maps:get(states_to_resolve,Spec)),
  maps:put(states_to_resolve, StatesToResolve, Spec).
%%


-spec edge_spec(map(), atom(), integer(), list(), map(), map(), map()) -> {map(), atom()}.

%% @doc for init edge
edge_spec(#{edge_data:=#{event_type:=init}}=_Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) -> {Spec, undefined};

%% @doc for end state
% edge_spec(#{to:=To}=_Edge, State, StateID, Edges, States, Spec) 
% when is_map_key(To,States)
% and map_get(To, States)=:=custom_end_state ->
%   pass;

%% @doc communicating actions
edge_spec(#{from:=StateID,to:=To,is_silent:=false,edge_data:=#{trans_type:=Kind}}=Edge, State, StateID, _Edges, States, _RecMap, Spec) 
when (Kind=:=send) or (Kind=:=recv) ->
  %% get next state info
  ToState = maps:get(To, States),
  ToStateName = case is_end_state(ToState) of
    true -> stop_state;
    _ -> state_name(ToState, To)
  end,
  
  StateName = state_name(State, StateID),
  %% update with this edge
  NewSpec = add_to_state_map(StateName,Kind,get_msg_label(Edge),ToStateName,Spec),
  % ?GAP(),?SHOW("\n(~p:~p)\nSpec:\n\t~p,\nNewSpec:\n\t~p,\nStates:\n\t~p.\n",[StateID,State,Spec,NewSpec,States]),
  %% return new spec
  {NewSpec, undefined};
%%

%% @doc for setting timers in the current state
edge_spec(#{to:=To,is_timer:=true,is_silent:=true,edge_data:=#{timer:=#{name:=Timer,duration:=Value}}}=_Edge, timer_start_state=State, StateID, _Edges, _States, _RecMap, Spec) ->
  %% get state name
  StateName = state_name(State, StateID),
  %% update spec with this reset
  NewSpec = add_to_resets(StateName,Timer,Value,Spec),
  % ?GAP(),?SHOW("\n(~p:~p)\nSpec:\n\t~p,\nNewSpec:\n\t~p,\nStates:\n\t~p.\n",[StateID,State,Spec,NewSpec,_States]),
  %% the above name will be unresolvable since it depends on a future state, add next state (To) to states to resolve, to be checked later on
  NewResolves = maps:put(To, {StateName,StateID,unresolved}, maps:get(states_to_resolve, NewSpec, #{})),
  %% return new spec
  {maps:put(states_to_resolve, NewResolves, NewSpec), StateName};
%%

% %% @doc 
% edge_spec(Edge, State, StateID, Edges, States, _RecMap, Spec) ->
%   {Spec, undefined};

% %% @doc 
% edge_spec(Edge, State, StateID, Edges, States, _RecMap, Spec) ->
%   {Spec, undefined};

% %% @doc for other silent edges
% edge_spec(#{to:=To,is_silent:=true}=Edge, State, StateID, Edges, States, _RecMap, Spec) ->
%   {Spec, undefined};

%% @doc 
edge_spec(Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) ->
  ?GAP(),?SHOW("\n(~p:~p)\nunhandled edge:\nSpec:\n\t~p,\nEdge:\n\t~p,\nStates:\n\t~p.\n",[_StateID,_State,Spec,Edge,_States]),
  {Spec, undefined}.


%% @doc adds event of Kind with Label and ToState under the map for State
%% note: make sure to name the State before this function
add_to_state_map(StateName,Kind,Label,ToState,Spec) 
when is_atom(Label) ->
  %% get map for this state
  OldMap = maps:get(map, Spec),
  OldStateMap = maps:get(StateName,OldMap,#{}),
  % ?SHOW("\nOldStateMap:\n\t~p.\n",[OldStateMap]),
  %% check it is mixed-safe
  OldStateKeys = maps:keys(OldStateMap),
  ?assert(length(OldStateKeys)<2),
  case length(OldStateKeys)==1 of
    true -> ?assert(lists:nth(1,maps:keys(OldStateMap))=:=Kind);
    _ -> ok
  end,
  NewStateMap = maps:update_with(Kind, fun(OldVal) -> 
    ?assert(is_map(OldVal)),
    %% add map of message label to next state (second list [] param is for clock resets)
    maps:merge(OldVal,#{Label => {ToState, []}})
  end, #{Label => {ToState, []}}, OldStateMap),
  %% return updated map
  NewMap = maps:put(StateName,NewStateMap,OldMap),
  maps:put(map, NewMap, Spec);
%%

add_to_state_map(StateName,Kind,Label,ToState,Spec) 
when is_list(Label) -> 
  add_to_state_map(StateName,Kind,list_to_atom(Label),ToState,Spec).
%%

%% @doc adds timeout of duration Ref and ToState under State
%% note: make sure to name the State before this function
add_to_timeouts(StateName,Ref,ToState,Spec)
when (is_atom(Ref) or is_integer(Ref)) ->
  %% get timeouts 
  OldTimeouts = maps:get(timeouts, Spec),
  %% make sure does not already have timeout
  ?assert(not is_map_key(StateName,OldTimeouts)),
  %% add timeout
  NewTimeouts = maps:put(StateName, {Ref, ToState}, OldTimeouts),
  %% return updated map
  maps:put(timeouts, NewTimeouts, Spec);
%%

add_to_timeouts(StateName,Ref,ToState,Spec) 
when is_list(Ref) ->
  add_to_timeouts(StateName,list_to_atom(Ref),ToState,Spec).
%%

%% @doc adds to timer map a transition from StateName and ToState
%% note: make sure to name the State before this function
add_to_timers(StateName,Timer,ToState,Spec)
when is_atom(Timer) ->
  %% get timers 
  OldTimers = maps:get(timers, Spec),
  OldTimerMap = maps:get(Timer, OldTimers, #{}),
  %% make sure StateName does not already have trigger for this Timer
  ?assert(not is_map_key(StateName,OldTimerMap)),
  %% add 
  NewTimerMap = maps:put(StateName, ToState, OldTimerMap),
  NewTimers = maps:put(Timer, NewTimerMap, OldTimers),
  %% return updated map
  maps:put(timers, NewTimers, Spec);
%%

add_to_timers(StateName,Timer,ToState,Spec) 
when is_list(Timer) ->
  add_to_timeouts(StateName,list_to_atom(Timer),ToState,Spec).
%%

%% @doc adds timer set to map for State Name
%% note: make sure to name the State before this function
add_to_resets(StateName,Timer,Value,Spec) 
when is_atom(Timer) ->
  %% get resets 
  OldResets = maps:get(resets, Spec),
  OldResetMap = maps:get(StateName, OldResets, #{}),
  %% make sure Timer does not have a value in resets from this state
  ?assert(not is_map_key(Timer,OldResetMap)),
  %% add 
  NewResetMap = maps:put(Timer, Value, OldResetMap),
  NewResets = maps:put(StateName, NewResetMap, OldResets),
  %% return updated map
  maps:put(resets, NewResets, Spec);
%%

add_to_resets(StateName,Timer,Value,Spec) 
when is_list(Timer) ->
  add_to_resets(StateName,list_to_atom(Timer),Value,Spec).
%%

