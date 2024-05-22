%% @doc Generate Erlang stub program from FSM generated from protocol sturcture
-module(gen_stub).

-export([gen/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").
-include("stub_tools.hrl").


prettypr_options() -> [{paper, 160}].
output_location() -> "tool_output/".

%% @doc Given a Protocol, first generates FSM and then starts building the module
-spec gen(interleave:protocol(), string()) -> none().
gen(Protocol, FileName) ->
  ModuleName = list_to_atom(lists:last(lists:droplast(string:tokens(FileName, "/.")))),
  ?SHOW("Module Name: ~p", [ModuleName]),

  Fsm = build_fsm(Protocol),
  ?SHOW("Build Fsm: Success.", []),

  _MonitorSpec = build_monitor_spec(Fsm, FileName),
  ?SHOW("Build Module Spec: Success.", []),

  {ok, ModuleForms} = build_module_forms(Fsm, ModuleName),
  ?SHOW("Build Module Forms: Success.", []),

  SyntaxTree = erl_syntax:form_list(ModuleForms),
  ?SHOW("Module Syntax Tree: Success.", []),

  Program = erl_prettypr:format(SyntaxTree, prettypr_options()),
  ?SHOW("Format to Program: Success.", []),

  OutputPath = output_location() ++ FileName ++ ".erl",
  file:write_file(OutputPath, Program).

%% @doc Wrapper for build_fsm:to_fsm(Protocol)
-spec build_fsm(interleave:protocol()) -> {list(), map()}.
build_fsm(Protocol) -> build_fsm:to_fsm(Protocol).

%% @doc Wrapper for build_spec:to_monitor_spec(Fsm)
-spec build_monitor_spec({list(), map()}, string()) -> map().
build_monitor_spec(Fsm,FileName) -> 
  {Status, MonitorSpec} = build_spec:to_monitor_spec(Fsm),
  case Status of
    pass -> ok; %% skip while developing tool
    _ ->
      %% write to file
      OutputPath = output_location() ++ FileName ++ "_mon_spec.txt",
      case file:write_file(OutputPath, MonitorSpec) of
        ok -> ?SHOW("file:write_file success: ~p.", [OutputPath]);
        Else -> ?SHOW("file:write_file failed: ~p.", [Else])
      end,
      MonitorSpec
  end.

% -spec build_module_forms({list(), map()}, atom()) -> tree_or_trees().
%% @doc Takes Fsm, returns list for Forms
build_module_forms(Fsm, ModuleName) -> 
  
  %% fsm is composed of list of edges and map of states to edges.
  %% mixed-states have both sending and receiving actions/edges.
  %% states are not mixed.
  %% mixed-states are represented using silent transition between states.
  {Edges, States, RecMap} = Fsm,

  %% for each state, generate corresponding snippet
  % StateFuns = maps:fold(fun(K, V, AccIn) -> AccIn ++ [state_fun(K, V, Edges, States, RecMap)] end, [], States),
  Funs = build_funs(Edges, States, RecMap),

  ?SHOW("funs: \n~p.", [Funs]),

  %% get edges of communication
  _ActionEdges = lists:filter(fun(Edge) -> (Edge#edge.edge_data#edge_data.trans_type==action) and ((not Edge#edge.is_silent) and (not Edge#edge.is_custom_end)) end, Edges),

  %% for each edge, generate corresponding snippet.
  %% function edge_fun 
  % _EdgeFuns = lists:foldl(fun(Edge, AccIn) -> AccIn ++ [edge_fun(Edge, ?MOD)] end, [], ActionEdges),

  %% create basic form for module
  Form = merl_build:init_module(ModuleName),

  Forms = merl_build:add_attribute(define, [merl:var('MONITORED'), false], Form),

  %% add the functions to the module
  AddFuns = fun({X, Name, Cs}, S) -> merl_build:add_function(X, Name, Cs, S) end,
  ModuleForm = merl_build:module_forms(lists:foldl(AddFuns, Forms, Funs)),
  %% return module (list of forms)
  {ok, ModuleForm}.
%%

%% TODO go through each state, add their actions as clauses to the current "scope_fun" IFF they are not in the recmap. otherwise, start a new "scope_fun" that is called at the appropriate time

%% @doc builds all functions corresponding to protocol behaviour.
%% goes through each state in states, starting from 0 and builds them.
%% depending on the state type, a different snippet is used.
%% each outgoing edge is performed within the same function, followed by their next state.
%% if the next state is a point of recursive reentry, then a call to a new function is placed within this scope instead, and a new function created containing all proceeding behaviour described by the protocol.
%% otherwise, all proceeding behaviour remains within the same scope.
%% @returns list of functions 
-spec build_funs(list(), map(), map()) -> list().
build_funs(Edges, States, RecMap) -> build_state_fun(Edges,States,RecMap,0,{-1, none}).

%% @doc states building program functions.
%% if stateID is init then create run() function, which always leads to main() -- this is expected when no clauses or funs currently provided
build_state_fun(Edges, States, RecMap, StateID, {ScopeID, ScopeName}=Scope) ->
  %% using assertions rather than guards for debugging purposes
  ?assert(is_list(Edges)),
  ?assert(is_map(States)),
  ?assert(is_map(RecMap)),
  ?assert(is_atom(ScopeName)),
  ?assert(is_integer(ScopeID)),
  ?assert(is_integer(StateID)),
  %% 
  ?GAP(),?SHOW("scope: ~p.", [Scope]),
  %% get state
  State = maps:get(StateID, States),
  %% if non-standard state, then get special snippet
  case lists:any(fun(Elem) -> Elem=:=State end, special_funs()) of
    true -> %% get snippet, go to next
      {Signal, {_,_FunName,_FunClauses}=Fun, NextState} = gen_snippets:special_state(State, StateID, Edges),
      case Signal of
        next_state -> %% e.g.: state=:=init_state
          {NextStateID, _NextStateFunName} = NextState,
          Funs = [Fun]++build_state_fun(Edges,States,RecMap,NextStateID,NextState);
        none -> %% e.g.: state=:=custom_end_state
          Funs = [Fun];
        _ -> %% unknown/other
          Funs = [Fun]
        end;
    _ -> %% normal state, build snippets,
      {StateFuns, NextStateFuns} = gen_snippets:state(State, StateID, Scope, Edges, States, RecMap),
      %% build remaining state funs needed (corresponding to states reached)
      Funs = lists:foldl(fun({NextStateID, _NextStateFunName}=NextStateScope, AccIn) -> AccIn++build_state_fun(Edges,States,RecMap,NextStateID,NextStateScope) end, StateFuns, NextStateFuns)
  end,
  %% funs contains list of [{true, FunName, FunClauses}, ...]
  Funs.
