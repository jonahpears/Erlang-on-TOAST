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

  ?SHOW("input fsm...\nEdges:\n\t~p,\nStates:\n\t~p,\nRecMap:\n\t~p.",[to_map(Edges),States,RecMap]),

  InitState = state_name(maps:get(0,States),0),
  InitSpec = #{ init => InitState,
                timeouts => #{},
                resets => #{},
                map => #{} },

  Spec = to_spec(InitState,0,Edges,States, RecMap, InitSpec),

  ?GAP(),?GAP(),?GAP(),?GAP(),?GAP(),?SHOW("input fsm...\nEdges:\n\t~p,\nStates:\n\t~p,\nRecMap:\n\t~p,\nfinished spec:\n\t~p.",[to_map(Edges),States,RecMap,Spec]),

  % timer:sleep(5000),

  {ok, Spec}.

-spec to_spec(atom(), integer(), list(), map(), map(), map()) -> map().

% %% @doc 
% to_spec(State, StateID, Edges, States, Spec) ->
%   pass;

%% @doc generic handler
to_spec(State, StateID, Edges, States, _RecMap, Spec) ->
  %% get relevant edges (and convert to map)
  RelevantEdges = to_map(get_outgoing_edges(StateID,Edges)),
  ?GAP(),?SHOW("entering...\t(~p:~p)\nRelevantEdges:\n\t~p.",[StateID,State,RelevantEdges]),

  %% check if init_state is not ready
  case (State=/=init_state) and ((maps:get(init,Spec)=:=init_state) and is_state_initialisable(State)) of
    true -> %% change init_state to current state
      InitSpec = maps:put(init,state_name(State,StateID),Spec);
      %% change occurence everywhere
    _ -> InitSpec = Spec
  end,

  %% check if recursive
  InitRecursiveVars = get_recursive_vars(StateID, _RecMap),
  %% there can only be zero or one
  ?assert(length(InitRecursiveVars)<2),
  case length(InitRecursiveVars)>0 of
    true -> %% do not explore recursive state more than once
      case maps:get(StateID,_RecMap,false) of
        true -> %% already explored
          % ?SHOW("\n(~p:~p) recursion already explored (is true).",[StateID,State]),
          RecMap = _RecMap;
        _ -> %% not been visited before
          % ?SHOW("\n(~p:~p) is new recursive state.",[StateID,State]),
          %% make sure it wont be visited next time
          RecMap = maps:put(StateID, true, _RecMap)
          % to_spec(ToState, ToStateID, Edges, States, RecMap, NoFutureResolveSpec)
      end;
    _ -> %% is not a recursive state
      % ?SHOW("\n(~p:~p) is not a recursive state,\nRecMap:\n\t~p.",[StateID,State,RecMap]),
      % to_spec(ToState, ToStateID, Edges, States, RecMap, NoFutureResolveSpec)
      RecMap = _RecMap
  end,

  % %% check for stateid in states_to_resolve, and then reverse and add current state
  % InitSpec = check_resolvable_state(State,StateID,Spec),

  %% create new spec from combination of all other specs reachable via edges
  OutSpec = lists:foldl(fun(Edge, InSpec) -> 
    % %% convert to map
    % Edge = to_map(_Edge),
    %% update resolvable states (that havent been resolved by current state) to next edge
    % UpdatedSpec = update_resolvable_states(Edge, InSpec),
    % ?SHOW("\n(~p:~p)\nedge:\n\t~p.\n",[StateID,State,Edge]),
    %% add current state/edges to inspec
    EdgeSpec = edge_spec(Edge, State, StateID, Edges, States, RecMap, InSpec),
    % ?SHOW("\n(~p:~p)\nEdgeSpec:\n\t~p.\n",[StateID,State,EdgeSpec]),
    %% get spec from edges and further
    ToStateID = maps:get(to, Edge),
    ToState = maps:get(ToStateID, States),
    %% remove unresolved so futures dont try and resolve them
    ResolveSpec = maps:get(unresolved, maps:get(resets, EdgeSpec, #{}), #{}),
    NoFutureResolveSpec = maps:put(resets, maps:remove(unresolved, maps:get(resets, EdgeSpec, #{})), EdgeSpec),
    %% do not explore custom_end_state
    StateSpec = case is_end_state(ToState) of
      true -> 
        % ?SHOW("\n(~p:~p) is end state.",[StateID,State]),
        NoFutureResolveSpec;
      _ -> %% do not want to explore error state
        case is_error_state(ToState) of
        true -> 
          % ?SHOW("\n(~p:~p) is error state.",[StateID,State]),
          NoFutureResolveSpec;
        _ -> %% check if recursive
          RecursiveVars = get_recursive_vars(ToStateID, RecMap),
          %% there can only be zero or one
          ?assert(length(RecursiveVars)<2),
          case length(RecursiveVars)>0 of
            true -> %% do not explore recursive state more than once
              case maps:get(ToStateID,RecMap,false) of
                true -> %% already explored
                  ?SHOW("\n(~p:~p) -> (~p,~p) recursion already explored (is true).",[StateID,State,ToStateID,ToState]),
                  NoFutureResolveSpec;
                _ -> %% not been visited before
                  % ?SHOW("\n(~p:~p) is new recursive state.",[StateID,State]),
                  %% make sure it wont be visited next time
                  % RecMap1 = maps:put(StateID, true, RecMap),
                  to_spec(ToState, ToStateID, Edges, States, RecMap, NoFutureResolveSpec)
              end;
            _ -> %% is not a recursive state
              % ?SHOW("\n(~p:~p) is not a recursive state,\nRecMap:\n\t~p.",[StateID,State,RecMap]),
              to_spec(ToState, ToStateID, Edges, States, RecMap, NoFutureResolveSpec)
          end
        end
    end,
    %% add unresolved back
    %% put in resets, under unresolved, the combination of ResolveSpec and what is in unresolved from statespec
    UnresolvedStateSpec = 
      maps:put(resets, 
        maps:put(unresolved, 
          maps:merge(maps:get(unresolved, maps:get(resets, StateSpec, #{}), #{}), ResolveSpec), 
          maps:get(resets, StateSpec, #{})), 
      StateSpec),
    % ?SHOW("\n(~p:~p)\nNoFutureResolveSpec\n\t~p,\nResolveSpec\n\t~p,\nStateSpec:\n\t~p,\nUnresolvedStateSpec:\n\t~p.",[StateID,State,NoFutureResolveSpec,ResolveSpec,StateSpec,UnresolvedStateSpec]),
    %% merge edgespec with inspec
    maps:merge_with(fun(KSpec, EdgeVal, InVal) -> 
      % ?SHOW("\n(~p:~p)\nKSpec:\n\t~p,\nEdgeVal:\n\t~p,\nInVal:\n\t~p.\n",[StateID,State,KSpec,EdgeVal,InVal]),
      case is_map(EdgeVal) of
        true ->
          ?assert(is_map(InVal)),
          %% resolve any lingering resets
          case KSpec=:=resets of
            true -> 
              ResolvedEdge = maps:fold(fun(K,V,Resets) -> 
                case K of 
                  unresolved -> 
                    case maps:size(V)>0 of
                      true ->
                        case is_state_resolvable(State) of
                          true -> Resolved = maps:put(state_name(State,StateID),V,Resets),
                            maps:remove(unresolved,Resolved);
                          _ -> maps:put(K,V,Resets)
                        end;
                      _ -> maps:put(K,V,Resets)
                    end;
                  _ -> maps:put(K,V,Resets)
                end
              end, #{}, EdgeVal),
              % ?SHOW("\n(~p:~p)\nin spec:\n\t~p,\noutspec:\n\t~p.\n",[StateID,State,Spec,ResolvedEdge]),
              %% merge both maps
              maps:merge(InVal, ResolvedEdge);
            _ -> maps:merge(InVal, EdgeVal)
          end;
        _ -> 
          %% should just be init
          ?assert(KSpec=:=init),
          % ?assert(EdgeVal=:=InVal),
          EdgeVal
      end
    end, UnresolvedStateSpec, EdgeSpec)
  end, InitSpec, RelevantEdges),


  % ?SHOW("\n(~p:~p)\nin spec:\n\t~p,\noutspec:\n\t~p.\n",[StateID,State,Spec,OutSpec]),

  %% if init, remove unresolved
  case State of
    init_state -> FinalSpec = maps:put(resets, maps:remove(unresolved, maps:get(resets,OutSpec)), OutSpec);
    _ -> FinalSpec = OutSpec
  end,

  FinalSpec.


-spec edge_spec(map(), atom(), integer(), list(), map(), map(), map()) -> map().

%% @doc for init edge
edge_spec(#{edge_data:=#{event_type:=init}}=_Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) -> Spec;

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
  % {NewSpec, undefined};
  NewSpec;
%%

%% @doc for setting timers in the current state
edge_spec(#{is_timer:=true,is_delay:=false,is_silent:=true,edge_data:=#{timer:=#{name:=Timer,duration:=Value}}}=_Edge, timer_start_state=_State, _StateID, _Edges, _States, _RecMap, Spec) ->
  %% get state name
  % StateName = state_name(State, StateID),
  %% update spec with this reset
  NewSpec = add_to_resets(Timer,Value,Spec),
  % ?GAP(),?SHOW("\n(~p:~p)\nSpec:\n\t~p,\nNewSpec:\n\t~p,\nStates:\n\t~p.\n",[StateID,State,Spec,NewSpec,_States]),
  %% the above name will be unresolvable since it depends on a future state, add next state (To) to states to resolve, to be checked later on
  % NewResolves = maps:put(unresolved, StateName,StateID,unresolved}, maps:get(states_to_resolve, NewSpec, #{})),
  %% return new spec
  % {maps:put(states_to_resolve, NewResolves, NewSpec), StateName};
  NewSpec;
%%

% %% @doc 
% edge_spec(Edge, State, StateID, Edges, States, _RecMap, Spec) ->
%   {Spec, undefined};

% %% @doc 
% edge_spec(Edge, State, StateID, Edges, States, _RecMap, Spec) ->
%   {Spec, undefined};

%% @doc for timeouts
edge_spec(#{to:=To,is_silent:=true,is_timeout:=true,edge_data:=#{timeout:=#{ref:=Timeout}}}=_Edge, State, StateID, _Edges, States, _RecMap, Spec) ->
  %% get next state info
  ToState = maps:get(To, States),
  ToStateName = case is_end_state(ToState) of
    true -> stop_state;
    _ -> state_name(ToState, To)
  end,
  
  StateName = state_name(State, StateID),
  %% update with this edge
  %% update with this edge
  % NewSpec = case is_integer(Timeout) of
  %   true -> add_to_timeouts(StateName,Timeout,ToStateName,Spec);
  %   _ -> add_to_timers(StateName,Timeout,ToState,Spec)
  % end,
  NewSpec = add_to_timeouts(StateName,Timeout,ToStateName,Spec),
  % ?GAP(),?SHOW("\n(~p:~p)\nSpec:\n\t~p,\nNewSpec:\n\t~p,\nStates:\n\t~p.\n",[StateID,State,Spec,NewSpec,States]),
  %% return new spec
  % {NewSpec, undefined};
  NewSpec;
%%

%% @doc for delays
edge_spec(#{to:=To,is_silent:=true,is_delay:=true,edge_data:=#{delay:=#{ref:=Delay}}}=_Edge, State, StateID, _Edges, States, _RecMap, Spec) ->
  %% get next state info
  ToState = maps:get(To, States),
  ToStateName = case is_end_state(ToState) of
    true -> stop_state;
    _ -> state_name(ToState, To)
  end,
  
  StateName = state_name(State, StateID),
  %% update with this edge
  % NewSpec = case is_integer(Delay) of
  %   true -> add_to_timeouts(StateName,Delay,ToStateName,Spec);
  %   _ -> add_to_timers(StateName,Delay,ToState,Spec)
  % end,
  NewSpec = add_to_timeouts(StateName,Delay,ToStateName,Spec),
  % ?GAP(),?SHOW("\n(~p:~p)\nSpec:\n\t~p,\nNewSpec:\n\t~p,\nStates:\n\t~p.\n",[StateID,State,Spec,NewSpec,States]),
  %% return new spec
  % {NewSpec, undefined};
  NewSpec;
%%

% %% @doc for errors
% edge_spec(#{is_error:=true}=_Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) ->
%   Spec;
% %%

% %% @doc 
% edge_spec(Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) ->
%   Spec;
% %%

%% @doc for unhandled edges
edge_spec(Edge, _State, _StateID, _Edges, _States, _RecMap, Spec) ->
  ?GAP(),?SHOW("\n(~p:~p)\nunhandled edge:\nSpec:\n\t~p,\nEdge:\n\t~p,\nStates:\n\t~p.\n",[_StateID,_State,Spec,Edge,_States]),
  Spec.
%%

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
    maps:merge(OldVal,#{Label => ToState})
  end, #{Label => ToState}, OldStateMap),
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
  ?SHOW("\nStateName: ~p,\nToState: ~p,\nOldTimeouts:\n\t~p.\n",[StateName,ToState,OldTimeouts]),
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

% %% @doc adds to timer map a transition from StateName and ToState
% %% note: make sure to name the State before this function
% add_to_timers(StateName,Timer,ToState,Spec)
% when is_atom(Timer) ->
%   %% get timers 
%   OldTimers = maps:get(timers, Spec),
%   OldTimerMap = maps:get(Timer, OldTimers, #{}),
%   %% make sure StateName does not already have trigger for this Timer
%   ?assert(not is_map_key(StateName,OldTimerMap)),
%   %% add 
%   NewTimerMap = maps:put(StateName, ToState, OldTimerMap),
%   NewTimers = maps:put(Timer, NewTimerMap, OldTimers),
%   %% return updated map
%   maps:put(timers, NewTimers, Spec);
% %%

% add_to_timers(StateName,Timer,ToState,Spec) 
% when is_list(Timer) ->
%   add_to_timers(StateName,list_to_atom(Timer),ToState,Spec).
% %%

%% @doc adds timer set to map for State Name
%% note: make sure to name the State before this function
add_to_resets(Timer,Value,Spec) 
when is_atom(Timer) ->
  %% get resets 
  OldResets = maps:get(resets, Spec, #{}),
  OldResetMap = maps:get(unresolved, OldResets, #{}),
  %% make sure Timer does not have a value in resets from this state
  ?assert(not is_map_key(Timer,OldResetMap)),
  %% add 
  NewResetMap = maps:put(Timer, Value, OldResetMap),
  NewResets = maps:put(unresolved, NewResetMap, OldResets),
  %% return updated map
  NewSpec = maps:put(resets, NewResets, Spec),
  ?GAP(),?SHOW("\nOldResetMap:\n\t~p,\nSpec:\n\t~p,\nNewSpec:\n\t~p.\n",[OldResetMap,Spec,NewSpec]),
  NewSpec;
%%

add_to_resets(Timer,Value,Spec) 
when is_list(Timer) ->
  add_to_resets(list_to_atom(Timer),Value,Spec).
%%


is_error_state(State) -> (State=:=error_state).

is_state_resolvable(State) ->
  ResolvableStates = [standard_state,recv_after_state,send_after_state,branch_state,branch_after_state,select_state,select_after_state,if_then_else_state,init_state],
  lists:member(State,ResolvableStates).

is_state_initialisable(State) ->
  InitialisableStates = [standard_state,recv_after_state,send_after_state,branch_state,branch_after_state,select_state,  select_after_state,if_then_else_state],
  lists:member(State,InitialisableStates).
  
  
