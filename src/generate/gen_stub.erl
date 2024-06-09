%% @doc Generate Erlang stub program from FSM generated from protocol sturcture
-module(gen_stub).

-export([gen/3,gen/4]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").
-include("snippets/stub_tools.hrl").


prettypr_options() -> [{paper, 160},{ribbon,160}].
output_location() -> "tool_output/".

%% @doc Given a Protocol, first generates FSM and then starts building the module
-spec gen(atom(), atom(), atom(), string()) -> none().
gen(ProtocolModule, ProtocolFun, ProtocolName, FileName) ->
  %% get protocol
  Protocol = ProtocolModule:ProtocolFun(ProtocolName),
  gen(ProtocolName,Protocol,FileName).
%%

%% @doc Given a Protocol, first generates FSM and then starts building the module
-spec gen(atom(), atom(), string()) -> none().
gen(ProtocolName, Protocol, FileName) ->
  ?GAP(),

  ModuleName = atom_to_list(ProtocolName)++"_"++FileName,
  ?SHOW("Module Name: ~p", [ModuleName]),

  Fsm = build_fsm(Protocol),
  ?SHOW("Build Fsm: Success. (~p)", [ProtocolName]),

  MonitorSpec = build_monitor_spec(Fsm, ModuleName),
  ?SHOW("Build Module Spec: Success.", []),

  ModuleForms = build_module_forms(Protocol, Fsm, list_to_atom(ModuleName), MonitorSpec),
  ?SHOW("Build Module Forms: Success.", []),

  SyntaxTree = erl_syntax:form_list(ModuleForms),
  % ?SHOW("Module Syntax Tree: Success:\n~p.", [SyntaxTree]),
  ?SHOW("Module Syntax Tree: Success.", []),

  Program = erl_prettypr:format(SyntaxTree, prettypr_options()),
  ?SHOW("Format to Program: Success.", []),

  OutputPath = output_location() ++ ModuleName,
  file:write_file(OutputPath, Program),
  
  ?GAP(),
  ?SHOW("Finished. Output path: ~p.",[OutputPath]),
  % timer:sleep(500),
  io:format("\n\n - - - - - - - - - - \n\n"),
  ok.

%% @doc Wrapper for build_fsm:to_fsm(Protocol)
-spec build_fsm(interleave:protocol()) -> {list(), map()}.
build_fsm(Protocol) -> build_fsm:to_fsm(Protocol).

%% @doc Wrapper for build_spec:to_monitor_spec(Fsm)
-spec build_monitor_spec({list(), map()}, string()) -> map().
build_monitor_spec(Fsm,FileName) -> 
  {Status, MonitorSpec} = build_spec:to_monitor_spec(Fsm),
  case Status of
    pass -> MonitorSpec; %% skip while developing tool
    _ ->
      %% write to file
      OutputPath = output_location() ++"gen_mon_spec_"++ FileName ++ ".txt",
      case file:write_file(OutputPath, io_lib:fwrite("~p.\n",[MonitorSpec])) of
        ok -> ?SHOW("file:write_file success: ~p.", [OutputPath]);
        Else -> ?SHOW("file:write_file failed: ~p.", [Else])
      end,
      MonitorSpec
  end.

% -spec build_module_forms({list(), map()}, atom()) -> tree_or_trees().
%% @doc Takes Fsm, returns list for Forms
build_module_forms(Protocol, Fsm, ModuleName,MonitorSpec) -> 
  
  %% fsm is composed of list of edges and map of states to edges.
  %% mixed-states have both sending and receiving actions/edges.
  %% states are not mixed.
  %% mixed-states are represented using silent transition between states.
  {Edges, States, RecMap} = Fsm,

  %% for each state, generate corresponding snippet
  % StateFuns = maps:fold(fun(K, V, AccIn) -> AccIn ++ [state_fun(K, V, Edges, States, RecMap)] end, [], States),
  Funs = build_funs(Edges, States, RecMap, ModuleName),

  % ?SHOW("funs: \n~p.", [Funs]),

  %% get edges of communication
  _ActionEdges = lists:filter(fun(Edge) -> (Edge#edge.edge_data#edge_data.trans_type==action) and ((not Edge#edge.is_silent) and (not Edge#edge.is_custom_end)) end, Edges),

  %% for each edge, generate corresponding snippet.
  %% function edge_fun 
  % _EdgeFuns = lists:foldl(fun(Edge, AccIn) -> AccIn ++ [edge_fun(Edge, ?MOD)] end, [], ActionEdges),

  %% create basic form for module
  Form = merl_build:init_module(ModuleName),

  % ?SHOW("Form: ~p.",[Form]),

  Forms = merl_build:add_attribute(define, [merl:var('MONITORED'), merl:var('false')], Form),

  Forms1 = merl_build:add_attribute(define, [merl:var('MONITOR_SPEC'),merl:term(MonitorSpec)],Forms),
  
  Forms2 = merl_build:add_attribute(define, [merl:var('PROTOCOL_SPEC'),merl:term(Protocol)],Forms1),

  Forms3 = merl_build:add_attribute(include, [merl:term("stub.hrl")],Forms2),

  %% export function found in stub.hrl

  MainForms = Forms3,

  ?SHOW("\nForms:\n\t~p.",[MainForms]),

  %% add the functions to the module
  AddFuns = fun({Exported, Name, Clauses}=_Fun, AccForm) -> 
    % ?SHOW("AddFun:\n\t~p.",[_Fun]),
    _F = merl_build:add_function(Exported, Name, Clauses, AccForm), 
    % ?SHOW("Returning:\n\t~p.",[_F]), 
    _F end,
  _ModuleForm = merl_build:module_forms(lists:foldl(AddFuns, MainForms, Funs)),
  % ?SHOW("_ModuleForm:\n\t~p.", [_ModuleForm]),
  _ModuleForm.
  %% return module (list of forms)
  % {ok, ModuleForm}.
%%

%% TODO go through each state, add their actions as clauses to the current "scope_fun" IFF they are not in the recmap. otherwise, start a new "scope_fun" that is called at the appropriate time

%% @doc builds all functions corresponding to protocol behaviour.
%% goes through each state in states, starting from 0 and builds them.
%% depending on the state type, a different snippet is used.
%% each outgoing edge is performed within the same function, followed by their next state.
%% if the next state is a point of recursive reentry, then a call to a new function is placed within this scope instead, and a new function created containing all proceeding behaviour described by the protocol.
%% otherwise, all proceeding behaviour remains within the same scope.
%% @returns list of functions 
% -spec build_funs(list(), map(), map()) -> list().
build_funs(Edges, States, RecMap, ModuleName) -> 
  build_state_fun(Edges,States,RecMap,0,{main,-1}, ModuleName).

%% @doc states building program functions.
%% if stateID is init then create run() function, which always leads to main() -- this is expected when no clauses or funs currently provided
build_state_fun(Edges, States, RecMap, StateID, {Scope,ScopeID},ModuleName) ->
  %% using assertions rather than guards for debugging purposes
  ?assert(is_list(Edges)),
  ?assert(is_map(States)),
  ?assert(is_map(RecMap)),
  ?assert(is_atom(Scope)),
  ?assert(is_integer(ScopeID)),
  ?assert(is_integer(StateID)),
  %% 
  ?GAP(),?SHOW("scope: ~p.", [Scope]),
  %% get state
  State = maps:get(StateID, States),

  %% add run and main to states
  _StatesEdit = maps:put(-2, run, States),
  StatesEdit = maps:put(-1, main, _StatesEdit),

  %% get map of state funs
  {_StateFunMap, _FunMap} = gen_snippets:state(State, StateID, {Scope,ScopeID},Edges,StatesEdit,RecMap, #{}),

  EmptyFuns = maps:get(-3,_FunMap,[]),
  FunMap = maps:remove(-3,_FunMap),
  StateFunMap = _StateFunMap,

  % ?GAP(),?SHOW("StateFunMap:\n\t~p,,\nEmptyFuns:\n\t~p,\nFunMap:\n\t~p.",[StateFunMap,EmptyFuns,FunMap]),?GAP(),

  ?SHOW("modulename: ~p.\n",[ModuleName]),
  % timer:sleep(500),

  %% convert to list
  _StateFuns = lists:foldl(fun(K, Funs) ->
    Funs ++ lists:foldl(fun(StateFun, AccIn) -> 
      % ?SHOW("StateFun:\n\t~p.",[StateFun]),
      AccIn++[{true,maps:get(K,FunMap),lists:foldl(fun(FunClause, _AccIn) ->
      % ?SHOW("FunClause:\n\t~p.",[FunClause]),
      QClause = ?Q(FunClause),
        if %% remove comments if necessary
          is_list(QClause) -> _AccIn++[lists:nth(1,QClause)];
          is_tuple(QClause) -> _AccIn++[QClause];
          true -> _AccIn++[QClause]
        end
      end, [], StateFun)}]
    end, [], maps:get(K,StateFunMap,[]))
  end, [], maps:keys(StateFunMap)),

  % ?SHOW("_StateFuns:\n\t~p.",[_StateFuns]),

  % StateFuns = lists:foldl(WrapClauses, [], _StateFuns),

  %% add all of the empty stubs
  StateFuns = lists:foldl(fun(FunName, AccIn) ->
    AccIn++[{false,FunName,[?Q(["(Data) -> extend_with_functionality"])]}]
  end, _StateFuns, EmptyFuns),

  ?SHOW("StateFuns:\n\t~p.",[StateFuns]),

  StateFuns.

  
  % %% make each clause of each function ?Q
  % WrapClause = fun(Clause, AccClauses) -> 
  %   % ?SHOW("WrapClause:\n\t~p.",[Clause]),
  %   %% check if ?Q returns list already (if comments are present, then 1st elem is clauses and 2nd is comments)
  %   QClause=?Q(Clause),
  %   if 
  %     is_list(QClause) -> _AccClauses = AccClauses++[lists:nth(1,QClause)],
  %     ?SHOW("QClause, removed comments from clauses to avoid issues:\n\t~p.",[lists:nthtail(1,QClause)]);
  %     is_tuple(QClause) -> _AccClauses = AccClauses++[QClause];
  %     true -> 
  %       ?SHOW("QClause, unexpected return: (not list or tuple):\n\t~p.",[QClause]), 
  %     _AccClauses= AccClauses++[QClause]
  %   end,
  %   % ?SHOW("_AccClauses:\n\t~p.",[_AccClauses]),
  %   _AccClauses
  % end,
  % WrapClauses = fun({Exported, FunName, Clauses}, AccFuns) -> 
  %   % ?GAP(),?SHOW("WrapClauses.",[]),
  %   _AccFuns = AccFuns++[{Exported,FunName,lists:foldl(WrapClause, [], Clauses)}], 
  %   % ?SHOW("_AccFuns:\n\t~p.",[_AccFuns]),
  %   _AccFuns
  % end,


  % %% if non-standard state, then get special snippet
  % case State of
  %   init_state -> %% get snippet, go to next
  %     {Signal, SpecialFuns, NextState} = gen_snippets:special_state(State, StateID, Edges),
  %     case Signal of
  %       next_state -> %% e.g.: state=:=init_state
  %         {NextStateID, NextStateFunName} = NextState,
  %         NextFuns = build_state_fun(Edges,States,RecMap,NextStateID,{NextStateFunName,-1}),
  %         % ?SHOW("NextFuns: ~p.",[NextFuns]),
  %         Funs = SpecialFuns++NextFuns;
  %       none -> %% e.g.: state=:=custom_end_state
  %         Funs = SpecialFuns;
  %       _ -> %% unknown/other
  %         Funs = SpecialFuns
  %       end;
  %   custom_end_state -> %% return nothing
  %     Funs = gen_snippets:state(State, StateID, {main,-1}, Edges, States, RecMap);
  %   _ -> %% normal state, build snippets,
  %     StateFuns = gen_snippets:state(State, StateID, {Scope,ScopeID}, Edges, States, RecMap),
  %     %% make each clause of each function ?Q
  %     WrapClause = fun(Clause, AccClauses) -> 
  %         % ?SHOW("WrapClause:\n\t~p.",[Clause]),
  %         %% check if ?Q returns list already (if comments are present, then 1st elem is clauses and 2nd is comments)
  %         QClause=?Q(Clause),
  %         if 
  %           is_list(QClause) -> _AccClauses = AccClauses++[lists:nth(1,QClause)],
  %           ?SHOW("QClause, removed comments from clauses to avoid issues:\n\t~p.",[lists:nthtail(1,QClause)]);
  %           is_tuple(QClause) -> _AccClauses = AccClauses++[QClause];
  %           true -> 
  %             ?SHOW("QClause, unexpected return: (not list or tuple):\n\t~p.",[QClause]), 
  %           _AccClauses= AccClauses++[QClause]
  %         end,
  %         % ?SHOW("_AccClauses:\n\t~p.",[_AccClauses]),
  %         _AccClauses
  %       end,
  %     WrapClauses = fun({Exported, FunName, Clauses}, AccFuns) -> 
  %         % ?GAP(),?SHOW("WrapClauses.",[]),
  %         _AccFuns = AccFuns++[{Exported,FunName,lists:foldl(WrapClause, [], Clauses)}], 
  %         % ?SHOW("_AccFuns:\n\t~p.",[_AccFuns]),
  %         _AccFuns
  %       end,
  %     Funs = lists:foldl(WrapClauses, [], StateFuns)
  % end,
  %% funs contains list of [{true, FunName, FunClauses}, ...]
  % Funs.
