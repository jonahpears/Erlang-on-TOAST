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

  % _MonitorSpec = build_monitor_spec(Fsm, FileName),
  % ?SHOW("Build Module Spec: Success.", []),

  ModuleForms = build_module_forms(Fsm, ModuleName),
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
  MonitorSpec = build_spec:to_monitor_spec(Fsm),
  %% write to file
  OutputPath = output_location() ++ FileName ++ "_mon_spec.txt",
  case file:write_file(OutputPath, MonitorSpec) of
    ok -> ?SHOW("file:write_file success: ~p.", [OutputPath]);
    Else -> ?SHOW("file:write_file failed: ~p.", [Else])
  end,
  MonitorSpec.

-spec build_module_forms({list(), map()}, atom()) -> list().
%% @doc Takes Fsm, returns list for Forms
build_module_forms(Fsm, ModuleName) -> 
  % Func = merl:var(list_to_atom("?FUNCTION_NAME")),
  
  %% fsm is composed of list of edges and map of states to edges.
  %% mixed-states have both sending and receiving actions/edges.
  %% states are not mixed.
  %% mixed-states are represented using silent transition between states.
  {Edges, States, RecMap} = Fsm,

  %% for each state, generate corresponding snippet
  % StateFuns = maps:fold(fun(K, V, AccIn) -> AccIn ++ [state_fun(K, V, Edges, States, RecMap)] end, [], States),
  StateFuns = state_funs(Edges, States, RecMap),

  %% get edges of communication
  ActionEdges = lists:filter(fun(Edge) -> (Edge#edge.edge_data#edge_data.trans_type==action) and ((not Edge#edge.is_silent) and (not Edge#edge.is_custom_end)) end, Edges),

  %% for each edge, generate corresponding snippet.
  %% function edge_fun 
  % _EdgeFuns = lists:foldl(fun(Edge, AccIn) -> AccIn ++ [edge_fun(Edge, ?MOD)] end, [], ActionEdges),

  %% add other mandatory functions (following type module_rep())
  Funs = StateFuns,
  % Funs = [ {true, start_link, []},
  %          {true, start_link, []},
  %          {true, send, []},
  %          {true, recv, []},
  %          {true, protocol_violated, []}
  %          | StateFuns ] ++ lists:usort(EdgeFuns) ++ [],

  % lists:foreach(fun(F) -> ?SHOW("...f:  ~p.", [F]) end,StateFuns),

  %% create basic form for module
  Form = merl_build:init_module(ModuleName),

  %% ? build erlang module when behaviour `gen_statem' 
  % Forms = merl_build:add_attribute(behaviour, [merl:term('gen_statem')], Form),

  Forms = merl_build:add_attribute(define, [merl:var('MONITORED'), false], Form),

  %% add the functions to the module
  AddFuns = fun({X, Name, Cs}, S) -> merl_build:add_function(X, Name, Cs, S) end,
  ModuleForm = merl_build:module_forms(lists:foldl(AddFuns, Forms, Funs)),
  %% return module (list of forms)
  ModuleForm.


%% TODO go through each state, add their actions as clauses to the current "scope_fun" IFF they are not in the recmap. otherwise, start a new "scope_fun" that is called at the appropriate time

%% @doc
%% @returns a list of module_rep => [ {true, atom_name, [Clauses]}, ... ]
-spec state_funs(list(), map(), map()) -> list().
state_funs(Edges, States, RecMap) -> 
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
      RecStates = maps:filter(fun(_K, V) -> V=:=StateID end, RecMap),
      IsRec = lists:foldl(fun(Elem, AccIn) -> AccIn and Elem end, true, RecStates),
      case (IsRec and ((length(Clauses)==0) and (StateID=:=ScopeID))) of 
        true -> %% is a recursive state
          %% start new function scope
          {RecFunName, RecEnterClauses} = gen_snippets:state_enter(State, StateID, Edges, State),
          %% reenter function body (with nonempty clauses)
          {RecClauses, RecFuns, RecFunNames} = state_funs(Edges, States, RecMap, StateID, ScopeID, RecEnterClauses, FunNames++[RecFunName]),
          %% return after child 
          {[], RecFuns++[{true,RecFunName,RecClauses}], RecFunNames};
        _ -> %% this is the body of a function
          %% TODO: check how to detect what kind of pattern is this state (send-before-recv, recv-before-send)
          %% go through outgoing edges

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

