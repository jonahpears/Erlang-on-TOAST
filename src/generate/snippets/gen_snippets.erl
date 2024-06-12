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
-spec state(atom(), integer(), {atom(), integer()}, {list(), list()}, list(), map(), map(), map()) -> {list(), map()}.

%% @doc 
state(State, StateID, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap) ->
  %% string helpers

  ?SHOW("building new state.\nstate:\t~p => ~p,\nscope:\t~p => ~p,\ndata:\t~p => ~p.",[StateID,State,ScopeID,Scope,PrevData,StateData]),
  
  ?VSHOW("\nedges:\t~p,\nstates:\t~p,\nrecmap:\t~p,\nfunmap:\t~p.",[Edges,States,RecMap,FunMap]),

  %% check if recursive state, and if already explored
  case maps:get(StateID,RecMap,not_recursive_state)=RecState of 
    {already_unfolded, _Var, StrFun} -> %% just insert callback
      %% return call to function
      {[StrFun++"(CoParty, "++PrevData++")"], #{}};
      
    _ -> %% may be, but explore children first and see if any of them define it


      %% get edges relevant to current state
      RelevantEdges = get_outgoing_edges(StateID,Edges),

      %% get relevant string edges
      StrEdges = #{},



      Snippet = case State of 
        if_then_else_state -> 
          Condition = maps:get(condition,StrEdges),
          True = maps:get(true,StrEdges),
          False = maps:get(false,StrEdges),
          snippet({if_then, Condition, True, False}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        delay_state -> 
          Duration = maps:get(duration,StrEdges),
          Continuation = maps:get(continuation,StrEdges),
          snippet({delay, Duration, Continuation}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        standard_state -> 
          %% depends on case of single direction kind
          Edge = lists:nth(1,RelevantEdges),
          ?assert(length(RelevantEdges)==1),
          %% get edge 
          #{to:=ToStateID,edge_data:=#{trans_type:=Kind}} = Edge,
          %% get continuation
          #{ToStateID:=ToState} = States,
          NextData = state_data(ToStateID,ScopeData),
          ContinuationSnippet = states(ToState, ToStateID, {Scope, ScopeID}, {StateData, NextStateData}, Edges, States, RecMap, FunMap),
          %% unpack 
           = ContinuationSnippet,
          %% string helpers
          Label = get_msg_label(Edge),
          %% depending on direction
          case Kind of 
            send -> 
              snippet({send_blocking_payload, Label, Continuation}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

            recv -> pass
          end,

        branch_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          snippet({branching, Labels, Continuations}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        select_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          snippet({blocking_selection, Labels, Continuations}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        recv_after_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          Duration = maps:get(duration,StrEdges),
          Timeout = maps:get(timeout,StrEdges),
          snippet({timeout, Labels, Continuations, Duration, Timeout}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        branch_after_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          Duration = maps:get(duration,StrEdges),
          Timeout = maps:get(timeout,StrEdges),
          snippet({timeout, Labels, Continuations, Duration, Timeout}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        send_after_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          Duration = maps:get(duration,StrEdges),
          Timeout = maps:get(timeout,StrEdges),
          snippet({nonblocking_payload, Label, Continuation, Duration, Timeout}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        select_after_state -> 
          Labels = maps:get(labels,StrEdges),
          Continuations = maps:get(continuations,StrEdges),
          Duration = maps:get(duration,StrEdges),
          Timeout = maps:get(timeout,StrEdges),
          snippet({nonblocking_selection, Labels, Continuations, Duration, Timeout}, {Scope, ScopeID}, {PrevData, StateData}, Edges, States, RecMap, FunMap);

        error_state -> 
          ["error()"];

        custom_end_state -> 
          pass;

        end_state -> 
          pass;

        init_state -> 
          pass

      end,

      %% unpack snippet
      {} = Snippet,


    not_recursive_state -> %% proceed 
    _ -> %% this is an un-explored recursive state
      %% update to be foumd next time
      %% !! check this on thw way back up !!
      % RecMap1 = maps:put(State,{already_unfolded,RecState,})

  end.
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
  StrTimerVar = tag_state_thing("TID_"++StrDuration++"_",StateID),

  %% build clause
  Clause = ["receive {timeout, "++StrTimerVar++", "++StrTimer++"} -> ",
            "?VSHOW(\"took branch for timer (~p) completing.\", ["++StrTimer++"], "++InData++"),"
            ]++Timer++["after 0 -> ",
                       "?VSHOW(\"took branch for timer (~p) still running.\", ["++StrTimer++"], "++InData++"),"]++False++[" end"],

  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we did not use outdata, return indata)
  {Snippet, InData, []};
%%


%% @doc delays
snippet({delay, Duration, Continuation}, {StateID, ScopeID}, {InData, _OutData})
when (is_atom(Duration) or is_integer(Duration)) and is_list(Continuation) -> 
  %% string helpers
  StrDuration = string_wrap(Duration),

  %% build clause
  Clause = case is_integer(Duration) of 
    true -> 
      ["?VSHOW(\"delay (~p).\", ["++StrDuration++"], "++InData++"),",
       "timer:sleep("++StrDuration++"),"];
    _ -> 
      ?assert(is_atom(Duration)), 
      StrTimer = tag_state_thing("TID_"++StrDuration++"_",StateID),
      %% get timer from data, wait until receive from it
      ["receive {timeout, "++StrTimer++", "++StrDuration++"} -> ",
      %% actually, just assert that the timer matches
       "?assert("++StrTimer++"=:=get_timer("++StrDuration++", "++InData++")),",
       "?VSHOW(\"timer (~p) finished delay.\", ["++StrDuration++"], "++InData++"),"]++Continuation++[" end"]
  end,
  
  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we did not use outdata, return indata)
  {Snippet, InData, []};
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
snippet({branching, Labels, Continuations}, {StateID, ScopeID}, {InData, OutData})
when is_list(Labels) and is_map(Continuations) -> 
  %% make string helpers
  StrPayload = tag_state_thing("Payload",StateID),
  StrLabel = tag_state_thing("Label",StateID),
  %% line by line clause for snippet
  Clause = ["?SHOW(\"waiting to recv.\", [], "++InData++"),",
            "receive "]++lists:foldl(fun(Label, Cases) -> 
              Head = case length(Cases)>0 of true -> ["; "]; _ -> [] end,
              Cases ++ Head ++ ["{CoParty, "++Label++"="++StrLabel++", "++StrPayload++"} -> ",
                                OutData++" = save_msg(recv, "++Label++", "++StrPayload++", "++InData++"),",
                                "?SHOW(\"recv ~p: ~p.\"., ["++StrLabel++", "++StrPayload++"], "++OutData++")"] ++ maps:get(Label, Continuations)
            end, [], Labels)++[" end"],

  %% add comments
  _Commented = erl_syntax:comment(["% ."]),
  %% package as map
  Snippet = Clause,%#{clause=>Clause,comment=>Comment},
  %% return with comments (since we used outdata, return that too)
  {Snippet, OutData, []};
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
%% Label is an string.
%% Continuation is the continuation of the Label.
%% Duration is either an integer or atom (referencing a timer) for the selection to complete within.
%% Timeout is the continuation for if the payload takes too long to return.
snippet({send_nonblocking_payload, Label, Continuation, Duration, Timeout}, {StateID, ScopeID}, {InData, OutData})
when is_list(Label) and (is_atom(Duration) or is_integer(Duration)) and is_list(Continuation) and is_list(Timeout) -> 
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
            OutData++" = save_msg(send, "++Label++", "++StrPayload++", "++InData++"),"
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
%% Labels is a list of string labels, corresponding to the options in the selection.
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
            "case Label of "] ++ lists:foldl(fun(Label, Cases) ->
              %% check label is as expected
              ?assert(is_list(Label)),
              %% build new case
              Case = [Label++" -> ",
                      "CoParty ! {self(), "++Label++", "++StrPayload++"},",
                      OutData++" = save_msg(send, "++Label++", "++StrPayload++", "++InData++"),",
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
snippet({get_timer, Name}, {StateID, ScopeID}, {InData, _OutData})
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


%% @doc wraps as a string
string_wrap(Thing) when is_list(Thing) and is_integer(StateID) -> Thing;
string_wrap(Thing) when is_integer(Thing) and is_integer(StateID) -> integer_to_list(Thing);
string_wrap(Thing) when is_atom(Thing) and is_integer(StateID) -> atom_to_list(Thing).


%% @doc returns the current scope data
scope_data(StateID, ScopeID) 
when ScopeID==-1 -> "Data"++integer_to_list(StateID);
%%

scope_data(StateID, ScopeID, ScopeData) -> "Data"++integer_to_list(StateID)++"_"++integer_to_list(ScopeID).
%%


%% @doc returns next state data incrememnted
state_data(StateID, ScopeID) 
when ScopeID==-1 -> "Data"++integer_to_list(StateID);
%%

state_data(StateID, ScopeID, ScopeData) -> "Data"++integer_to_list(StateID)++"_"++integer_to_list(ScopeID).
%%


%% @doc returns timer name
timer_name(Name) when is_list(Name) -> "timer_"++Name;
%%
timer_name(Name) when is_atom(Name) -> "timer_"++atom_to_list(Name).
%%

