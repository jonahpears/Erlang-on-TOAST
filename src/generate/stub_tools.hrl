-compile({nowarn_unused_function, [ {show,1}, {show,2}, {merl_commented,2}, {merl_commented,3}, {special_funs,0}, {integer_to_atom,1} ]}).

-define(MOD, merl:var(list_to_atom("?MODULE"))).
-define(FUNC, merl:var(list_to_atom("?FUNCTION_NAME"))).

-define(STUB_MONITORED, merl:var(list_to_atom("?MONITORED"))).
-define(STUB_MONITOR_SPEC, merl:var(list_to_atom("?MONITOR_SPEC"))).

-define(GAP(),io:format("\n\n")).

-define(SHOW(String,Args),show("~p, "++String,[?FUNCTION_NAME]++Args)).

-ifndef(Q).
-include_lib("syntax_tools/include/merl.hrl").
-endif.


%% @doc Wrapper for io:format output logging
show(String) when is_list(String) -> show(String, []).
show(String, Args) -> 
  % case last(String) of 
  %   "." -> String1 = String;
  %   _ -> String1 = String ++ "."
  % end,
  io:format("~p, "++String++"\n", [?MODULE]++Args).


%% {exported?, atom_name, list_clauses} used by merl_build functions.
% -type module_rep() :: {boolean(), atom(), list()}.

%% @doc for commenting ?Q 
%% @returns merl:tree_or_trees()
merl_commented(Comments, Node) -> merl_commented(pre, Comments, Node).
merl_commented(pre, Comments, Node) when is_list(Comments) ->
  erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Comments), Node
  );
merl_commented(post, Comments, Node) when is_list(Comments) ->
  erl_syntax:add_postcomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, Comments), Node
  ).

%% @doc 
integer_to_atom(Integer) -> list_to_atom(integer_to_list(Integer)).

%% @doc
special_funs() -> [init_state,custom_end_state].