%% @doc Generate Erlang stub program from FSM generated from protocol sturcture
-module(gen_stub).
-compile({nowarn_unused_function, [ {show,1}, {show,2}]}).
-define(SHOW(String,Args),show("~p"++String,[?FUNCTION_NAME]++Args)).

-export([gen/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").


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
  ?SHOW("Build Module Forms: Success.", []),

  ModuleForms = build_module_forms(Fsm, ModuleName),
  ?SHOW("Build Module Forms: Success.", []),

  SyntaxTree = erl_syntax:form_list(ModuleForms),
  ?SHOW("Module Syntax Tree: Success.", []),

  Program = erl_prettypr:format(SyntaxTree, prettypr_options()),
  ?SHOW("Format to Program: Success.", []),

  OutputPath = string:concat(output_location(), FileName, ".erl"),
  file:write_file(OutputPath, Program).

%% @doc Wrapper for build_fsm:to_fsm(Protocol)
-spec build_fsm(interleave:protocol()) -> {list(), map()}.
build_fsm(Protocol) -> build_fsm:to_fsm(Protocol).

%% @doc Wrapper for build_spec:to_monitor_spec(Fsm)
-spec build_monitor_spec({list(), map()}, string()) -> map().
build_monitor_spec(Fsm,FileName) -> 
  MonitorSpec = build_spec:to_monitor_spec(Fsm),
  %% write to file
  OutputPath = string:concat(output_location(), FileName, "_mon_spec.txt"),
  case file:write_file(OutputPath, MonitorSpec) of
    ok -> ?SHOW("", [OutputPath]);
    Else -> ?SHOW("file:write_file failed: ~p.", [Else])
  end,
  MonitorSpec.

%% @doc Wrapper for io:format output logging
show(String) when is_list(String) -> show(String, []).
show(String, Args) -> 
  % case last(String) of 
  %   "." -> String1 = String;
  %   _ -> String1 = String ++ "."
  % end,
  io:format("~p, "++String, [?MODULE]++Args).


-spec build_module_forms({list(), map()}, atom()) -> list().
%% @doc Takes Fsm, returns list for Forms
build_module_forms(Fsm, ModuleName) -> 
  Module = merl:var(list_to_atom("?MODULE")),
  Func = merl:var(list_to_atom("?FUNCTION_NAME")),
  
  {Edges, States} = Fsm,

  ActionEdges = lists:filter(fun(Edge) -> Edge#edge.edge_data#edge_data.trans_type==action and not Edge#edge.is_silent and not Edge#edge.is_custom_end end, Edges),

  StateFuns = maps:fold(fun(K, V, AccIn) -> AccIn ++ [state_fun(K, V, Edges, Nodes)] end, [], States),

  EdgeFuns = lists:foldl(fun(Edge, AccIn) -> AccIn ++ [edge_fun(Edge, Module)] end, [], ActionEdges),

  Funs = [ {true, start_link, []},
           {true, start_link, []},
           {true, send, []},
           {true, recv, []},
           {true, protocol_violated, []}
           | StateFuns ] ++ lists:usort(EdgeFuns) ++ [],

  Forms = merl_build:add_attribute( behaviour, 
                                    [merl:term('gen_statem')], 
                                    merl_build:init_module(ModuleName)),


  AddFuns = fun( {X, Name, Cs}, S) -> 
    merl_build:add_function(X, Name, Cs, S) end,

  merl_build:module_forms(lists:foldl(AddFuns, Forms, Funs)).

