-module(gen_snippets).
-compile({nowarn_unused_function,[ {state_name,2} ]}).

-define(EXPORT_DEFAULT, false).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("stdlib/include/assert.hrl").

%% for records
-include("reng.hrl").

%% for helper functions
-include("stub_tools.hrl").
-include("fsm_tools.hrl").

%% specific functionality
-include("edge_snippets.hrl").
-include("get_if_edges.hrl").


%%%%%%%%%%%
%%% state
%%%%%%%%%%%

%% @doc 
%% 
-spec state(atom(), integer(), {atom(), integer()}, list(), map(), map(), map()) -> {map(), map()}.

%% @doc 
state(State, StateID, {Scope, ScopeID}, Edges, States, RecMap, FunMap) ->
  pass;
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

% %% @doc 
% snippet({}, {StateID, ScopeID}, {InData, OutData}) -> 

% %%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% time-sensitive if-statements 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
  Branches = snippet({branching, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData}),
  %% then, insert before the last occurance of "end" in Branches
  ?assert(lists:last(Branches)==" end"),
  Head = lists:droplast(Branches),
  
  %% build total clause
  Clause = Head ++ ["; "] ++ Tail,
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, []};
%%

%% @doc for any other kind of reception. (including single receptions)
%% expects a timeout via a timer to be already wrapped inside the labels and continuations
%% ! note, can not assert label is atom since it may be {timeout, TID, timer} for timeouts
snippet({branching, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and is_map(Continuations) -> 
  pass;
%%


%%%%%%%%%%%%%%%%%%%%%%%%
%%% payload & selection
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc snippet for (potentially) blocking payload (or, for sends without a timeout)
%% requires the name of the variable to be used for the payload to be predefined
snippet({send_blocking_payload, Label}, {StateID, ScopeID}, {InData, OutData}) -> 
  %% make string helpers
  StrFun = tag_state_thing("get_payload",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = [StrPayload++" = "++StrFun++"("++Label++", "++InData++"),",
            "CoParty ! {self(), "++Label++", "++StrPayload++"},",
            OutData++" = save_msg(send, "++Label++", "++StrPayload++", "++InData++")",
            "?SHOW(\"sent "++Label++".\", [], "++OutData++")"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, [{payload_fun,{StrFun,Label}}]};
%%


%% @doc snippet for obtaining non-blocking payloads.
%% Label is an atom.
%% Continuation is the continuation of the Label.
%% Duration is either an integer or atom (referencing a timer) for the selection to complete within.
%% Timeout is the continuation for if the payload takes too long to return.
snippet({send_nonblocking_payload, Label, Continuation, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_atom(Label) and (is_atom(Duration) or is_integer(Duration)) and is_list(Continuation) and is_list(Timeout) -> 
  %% make string helpers
  StrDuration = case is_integer(Duration) of true -> integer_to_list(Duration); _ -> ?assert(is_atom(Duration)), atom_to_list(Duration) end,
  StrFun = tag_state_thing("get_payload",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = ["Await"++StrPayload++" = nonblocking_payload(fun "++StrFun++"/1, {"++Label++", "++InData++"}, self(), "++StrDuration++", "++InData++"),"
            "?VSHOW(\"waiting for payload to be returned.\", [], "++InData++"),",
            "receive {Await"++StrPayload++", ok, {Label, Payload}} -> ",
            "?VSHOW(\"payload obtained: (~p: ~p).\",[Label,Payload], "++InData++"),",
            "CoParty ! {self(), "++Label++", "++StrPayload++"},",
            OutData++"save_msg(send, "++Label++", "++StrPayload++", "++InData++"),"
           ]++Continuation++["; ",
            "{Await"++StrPayload++", ko} -> ",
            "?VSHOW(\"unsuccessful payload. (probably took too long)\", [], "++InData++"),"]++Timeout++[" end"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, [{payload_fun,{StrFun,Label}}]};
%%


%% @doc snippet for non-blocking selection.
%% Labels is a list of atom labels, corresponding to the options in the selection.
%% Continuations is a map between atom labels and their continuation snippets.
%% Duration is either an integer or atom (referencing a timer) for the selection to complete within.
%% Timeout is the continuation of if the selection cannot be made.
snippet({nonblocking_selection, Labels, Continuations, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and (is_atom(Duration) or is_integer(Duration)) and is_map(Continuations) and is_list(Timeout) -> 
  %% make string helpers
  StrLabels = io_lib:fwrite("~p",[Labels]),
  StrDuration = case is_integer(Duration) of true -> integer_to_list(Duration); _ -> ?assert(is_atom(Duration)), atom_to_list(Duration) end,
  StrSelection = tag_state_thing("Selection",StateID),
  StrFun = tag_state_thing("make_selection",StateID),
  StrPayload = tag_state_thing("Payload",StateID),
  %% line by line clause for snippet
  Clause = [StrSelection++" = "++StrLabels++",",
            "Await"++StrSelection++" = nonblocking_selection(fun "++StrFun++"/1, {"++StrSelection++", "++InData++"}, self(), "++StrDuration++", "++InData++"),"
            "?VSHOW(\"waiting for selection to be made (out of: ~p).\", ["++StrSelection++"], "++InData++"),",
            "receive {Await"++StrSelection++", ok, {Label, Payload}} -> ",
            "?VSHOW(\"selection made: ~p.\",[Label], "++InData++"),",
            "case Label of "] ++ lists:foldl(fun(AtomLabel, Cases) ->
              %% check label is as expected
              ?assert(is_atom(AtomLabel)),
              Label = atom_to_list(AtomLabel),
              %% build new case
              Case = [Label++" -> ",
                      "CoParty ! {self(), "++Label++", "++StrPayload++"},",
                      OutData++"save_msg(send, "++Label++", "++StrPayload++", "++InData++"),",
                      "?SHOW(\"sent "++Label++".\", [], "++OutData++")"] ++ maps:get(AtomLabel,Continuations) ++ ["; "],
              %% return new case
              Cases ++ [Case]
            end, [], Labels) ++ [" _Err -> ?SHOW(\"error, unexpected selection: ~p.\", [_Err], "++InData++"),",
            "error(unexpected_label_selected)",
            "end;",
            "{Await"++StrSelection++", ko} -> ",
            "?VSHOW(\"unsuccessful selection. (probably took too long)\", [], "++InData++"),"]++Timeout++[" end"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, [{selection_fun,{StrFun,Labels}}]};
%%


%%%%%%%%%%%
%%% timers
%%%%%%%%%%%

%% @doc snippet for getting timers
snippet({get_timer, Name}, {StateID, ScopeID}, {InData, OutData})
when is_atom(Name) ->
  %% string helpers
  StrName = atom_to_list(Name),
  StrVar = tag_state_thing("TID_"++StrName,StateID),
  %% line by line clause for snippet
  Clause = [StrVar++" = get_timer("++Name++", "++InData++")"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we did not use outdata, return indata)
  {Snippet, InData, []};
%%


%% @doc snippet for setting timers
snippet({set_timer, Name, Duration}, {StateID, ScopeID}, {InData, OutData})
when is_atom(Name) and is_integer(Duration) ->
  %% string helpers
  StrName = atom_to_list(Name),
  StrVar = tag_state_thing("TID_"++StrName,StateID),
  StrDuration = integer_to_list(Duration),
  %% line by line clause for snippet
  Clause = [OutData++" = set_timer("++StrName++", "++StrDuration++", "++InData++"),",
            "?VSHOW(\"set timer "++StrName++" with duration: "++StrDuration++".\", [], "++OutData++")"],
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, []};
%%



%% @doc unhandled snippet requested.
snippet(Unhandled, StateStuff, ScopeStuff) -> 
  ?SHOW("unhandled:\t~p,\nstate stuff:\t~p,\nscope stuff:\t~p.",[Unhandled,StateStuff,ScopeStuff]),
  timer:sleep(5000),
  error(unhandled_snippet).
%%



%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%

%% @doc takes a given Thing with a suffix of the given StateID.
%% @returns a string
tag_state_thing(Thing, StateID) when is_list(Thing) and is_integer(StateID) -> Thing++integer_to_list(StateID);
tag_state_thing(Thing, StateID) when is_atom(Thing) and is_integer(StateID) -> atom_to_list(Thing)++integer_to_list(StateID).



%% @doc returns state data incrememnted
%% second elem of tuple indicates the integer suffix at the end denoting the state within the scope.
%% if second elem is -1, thenresolve_edge_clause this is a fresh scope
next_state_data(StateID, ScopeID, _ScopeData) 
when ScopeID==-1 -> 
  tag_state_thing("Data",StateID);
%%

next_state_data(StateID, _ScopeID, ScopeData) -> 
  tag_state_thing(ScopeData,StateID).
%%


%% @doc returns timer name
timer_name(Name) when is_list(Name) -> "timer_"++Name;
%%
timer_name(Name) when is_atom(Name) -> "timer_"++atom_to_list(Name).
%%

