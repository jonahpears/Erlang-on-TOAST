%% @doc Generate Erlang stub program from FSM generated from protocol sturcture
-module(gen_stub).

-export([gen/3,gen/4]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("reng.hrl").
-include("stub_tools.hrl").


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
  %% make sure output has a folder
  case filelib:ensure_dir(output_location()) of
    ok -> ok;
    _Err -> ?SHOW("Warning, output directory (~p) could not be created.\n\nMaybe try creating it yourself and running the program again.",[output_location()])
  end,

  ModuleName = case FileName of 
    %% if only ext provided
    ".erl" -> ProtocolName;

    %% if no filename provided
    "" -> ProtocolName;

    %% if some  
    _ -> list_to_atom(atom_to_list(ProtocolName)++"_"++lists:last(lists:droplast(string:tokens(FileName, "/."))))

  end,
  ?SHOW("Module Name: ~p", [ModuleName]),

  Fsm = build_fsm(Protocol),
  ?SHOW("Build Fsm: Success. (~p)", [ProtocolName]),

  MonitorSpec = build_monitor_spec(Fsm, atom_to_list(ModuleName)),
  ?SHOW("Build Module Spec: Success.", []),

  ModuleForms = build_module_forms(Protocol, Fsm, ModuleName, MonitorSpec),
  ?SHOW("Build Module Forms: Success.", []),

  SyntaxTree = erl_syntax:form_list(ModuleForms),
  % ?SHOW("Module Syntax Tree: Success:\n~p.", [SyntaxTree]),
  ?SHOW("Module Syntax Tree: Success.", []),

  Program1 = erl_prettypr:format(SyntaxTree, prettypr_options()),
  ?SHOW("Format to Program: Success.", []),

  %% replace each "TempMacroPlaceholder_" with "?"
  Program2 = string:replace(Program1,"TempMacroPlaceholder_","?",all),

  %% make sure "-export" is after "-include("stub.hrl")."
  %% by default, merl_build always places include before export
  %% first remove
  ProgramList1 = string:split(Program2,"-include(\"stub.hrl\").",leading),
  ?assert(length(ProgramList1)==2),
  Preamble = lists:nth(1, ProgramList1),
  ExportAndMainMatter = lists:nth(2, ProgramList1),
  %% then find where "-export" is
  ExportOnwards = string:find(ExportAndMainMatter,"-export",leading),
  %% then find where the next line break is, and put it after that
  ProgramList = string:split(ExportOnwards,".",leading),
  ?assert(length(ProgramList)==2),
  ExportStringOld = lists:nth(1, ProgramList),
  MainMatter = lists:nth(2, ProgramList),

  %% add to export string "start_link/0,start_link/1,stopping/2"
  {ExportStringFront, ExportStringEnd} = lists:split(length(ExportStringOld)-2, ExportStringOld),
  ExportString = ExportStringFront++", start_link/0, start_link/1, stopping/2"++ExportStringEnd,

  %% stitch back together
  Program3 = Preamble ++ "\n" ++ ExportString ++ ".\n\n-include(\"stub.hrl\").\n\n%% @doc \nstart_link() -> start_link([]).\n\n%% @doc \nstart_link(Args) -> stub_start_link(Args).\n\n" ++ MainMatter,
  % Program3 = Preamble ++ "\n" ++ ExportString ++ ".\n\n-include_lib(\"headers/stub.hrl\").\n\n%% @doc \nstart_link() -> start_link([]).\n\n%% @doc \nstart_link(Args) -> stub_start_link(Args).\n\n" ++ MainMatter,

  Program = Program3,

  OutputPath = output_location() ++ atom_to_list(ModuleName) ++ ".erl",
  file:write_file(OutputPath, Program),
  
  ?GAP(),
  ?SHOW("Finished. Output path: ~p.",[OutputPath]),
  io:format("\n\n - - - - - - - - - - \n\n"),
  % timer:sleep(250),
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
      case false of 
        true -> 
          OutputPath = output_location() ++"gen_mon_spec_"++ FileName ++ ".txt",
          case file:write_file(OutputPath, io_lib:fwrite("~p.\n",[MonitorSpec])) of
            ok -> ?SHOW("file:write_file success: ~p.", [OutputPath]);
            Else -> ?SHOW("file:write_file failed: ~p.", [Else])
          end;
        _ -> ok
      end,
      MonitorSpec
  end.

% -spec build_module_forms({list(), map()}, atom()) -> tree_or_trees().
%% @doc Takes Fsm, returns list for Forms
build_module_forms(Protocol, Fsm, ModuleName,MonitorSpec) -> 
  
  %% create basic form for module
  Form = merl_build:init_module(ModuleName),

  % ?SHOW("Form: ~p.",[Form]),

  Forms =  merl_build:add_attribute(define,  [merl:var('MONITORED'),              merl:var('false')], Form),
  Forms1 = merl_build:add_attribute(define,  [merl:var('SHOW_MONITORED'),         merl:var('case ?MONITORED of true -> "(monitored) "; _ -> "" end')], Forms),
  Forms2 = merl_build:add_attribute(define,  [merl:var('SHOW_ENABLED'),           merl:var('true')], Forms1),
  Forms3 = merl_build:add_attribute(define,  [merl:var('SHOW(Str, Args, Data)'),  merl:var('case ?SHOW_ENABLED of true -> printout(Data, ?SHOW_MONITORED++"~p, "++Str, [?FUNCTION_NAME]++Args); _ -> ok end')], Forms2),
  Forms4 = merl_build:add_attribute(define,  [merl:var('SHOW_VERBOSE'),           merl:var('?SHOW_ENABLED and true')], Forms3),
  Forms5 = merl_build:add_attribute(define,  [merl:var('VSHOW(Str, Args, Data)'), merl:var('case ?SHOW_VERBOSE of true -> printout(Data, ?SHOW_MONITORED++"(verbose) ~p, "++Str, [?FUNCTION_NAME]++Args); _ -> ok end')], Forms4),
  Forms6 = merl_build:add_attribute(define,  [merl:var('DO_SHOW(Str, Args, Data)'), merl:var('printout(Data, ?SHOW_MONITORED++"(verbose) ~p, "++Str, [?FUNCTION_NAME]++Args)')], Forms5),
  Forms7 = merl_build:add_attribute(define,  [merl:var('MONITOR_SPEC'),           merl:term(MonitorSpec)], Forms6),
  Forms8 = merl_build:add_attribute(define,  [merl:var('PROTOCOL_SPEC'),          merl:term(Protocol)], Forms7),
  Forms9 = merl_build:add_attribute(include, [merl:term("stub.hrl")],Forms8),

  MainForms = Forms9,

  ?VSHOW("\nForms:\t~p.",[MainForms]),

  %% fsm is composed of list of edges and map of states to edges.
  %% mixed-states have both sending and receiving actions/edges.
  %% states are not mixed.
  %% mixed-states are represented using silent transition between states.
  {Edges, States, RecMap} = Fsm,

  %% for each state, generate corresponding snippet
  % StateFuns = maps:fold(fun(K, V, AccIn) -> AccIn ++ [state_fun(K, V, Edges, States, RecMap)] end, [], States),
  Funs = build_funs(Edges, States, RecMap, ModuleName),
  % ?SHOW("funs: \n~p.", [Funs]),

  %% add the functions to the module
  AddFuns = fun({Exported, Name, Clauses}=_Fun, AccForm) -> 
    % ?SHOW("AddFun:\n\t~p.",[_Fun]),
    _F = merl_build:add_function(Exported, Name, Clauses, AccForm), 
    % ?SHOW("Returning:\n\t~p.",[_F]), 
    _F 
  end,

  ModuleForm = merl_build:module_forms(lists:foldl(AddFuns, MainForms, Funs)),

  ?VSHOW("\nModuleForm:\t~p.", [ModuleForm]),
  
  ModuleForm.
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

  StateID = 0,
  State = maps:get(StateID, States),

  ?VSHOW("\n\nEdges:\t~p,\nStates:\t~p,RecMap:\t~p,\n\ngenerating for \"~p\".\n\n",[Edges,States,RecMap,ModuleName]),

  FunMap = gen_snippets:state(State, StateID, Edges, States, RecMap, #{}),

  ?VSHOW("\nFunMap:\t~p.\n",[FunMap]),

  %% convert to list

  StateFuns = maps:fold(fun(_K, V, Funs) -> 
    Funs ++ V
  end, [], FunMap),

  ?VSHOW("\nStateFuns:\t~p.\n",[StateFuns]),

  StateFuns.



  % build_state_fun(Edges,States,RecMap,0,{main,-1}, ModuleName).

%% @doc states building program functions.
%% if stateID is init then create run() function, which always leads to main() -- this is expected when no clauses or funs currently provided
% build_state_fun(Edges, States, RecMap, StateID, {Scope,ScopeID},ModuleName) ->
%   %% using assertions rather than guards for debugging purposes
%   ?assert(is_list(Edges)),
%   ?assert(is_map(States)),
%   ?assert(is_map(RecMap)),
%   ?assert(is_atom(Scope)),
%   ?assert(is_integer(ScopeID)),
%   ?assert(is_integer(StateID)),
%   %% 
%   ?GAP(),?SHOW("scope: ~p.", [Scope]),
  %% get state
  % State = maps:get(StateID, States),

  % %% add run and main to states
  % _StatesEdit = maps:put(-2, run, States),
  % StatesEdit = maps:put(-1, main, _StatesEdit),

  % %% get map of state funs
  % {_StateFunMap, _FunMap} = gen_snippets:state(State, StateID, {Scope,ScopeID},Edges,StatesEdit,RecMap, #{}),

  % EmptyFuns = maps:get(-3,_FunMap,[]),
  % FunMap = maps:remove(-3,_FunMap),
  % StateFunMap = _StateFunMap,

  % % ?GAP(),?SHOW("StateFunMap:\n\t~p,,\nEmptyFuns:\n\t~p,\nFunMap:\n\t~p.",[StateFunMap,EmptyFuns,FunMap]),?GAP(),

  % ?SHOW("modulename: ~p.\n",[ModuleName]),
  % % timer:sleep(500),

  % %% convert to list
  % _StateFuns = lists:foldl(fun(K, Funs) ->
  %   Funs ++ lists:foldl(fun(StateFun, AccIn) -> 
  %     % ?SHOW("StateFun:\n\t~p.",[StateFun]),
  %     AccIn++[{true,maps:get(K,FunMap),lists:foldl(fun(FunClause, _AccIn) ->
  %     % ?SHOW("FunClause:\n\t~p.",[FunClause]),
  %     QClause = ?Q(FunClause),
  %       if %% remove comments if necessary
  %         is_list(QClause) -> _AccIn++[lists:nth(1,QClause)];
  %         is_tuple(QClause) -> _AccIn++[QClause];
  %         true -> _AccIn++[QClause]
  %       end
  %     end, [], StateFun)}]
  %   end, [], maps:get(K,StateFunMap,[]))
  % end, [], maps:keys(StateFunMap)),

  % % ?SHOW("_StateFuns:\n\t~p.",[_StateFuns]),

  % %% add all of the empty stubs
  % StateFuns = lists:foldl(fun(FunName, AccIn) ->
  %   AccIn++[{false,FunName,[?Q(["(Data) -> extend_with_functionality"])]}]
  % end, _StateFuns, EmptyFuns),

  % ?VSHOW("StateFuns:\n\t~p.",[StateFuns]),

  % StateFuns.
