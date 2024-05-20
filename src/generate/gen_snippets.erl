%% @doc Erlang snippets used in stub generation
-module(gen_snippets).
-compile({nowarn_unused_function, [ {get_state_name,2}, {get_next_state_trans,2} ]}).

-export([ special_state/4,
          state_enter/4,
          state_clauses/4,
          edge_clauses/0
        ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").
-include("stub_tools.hrl").

%% @doc 
get_state_name(State, StateID) -> 
  case State of 
    end_state -> end_state;
    custom_end_state -> custom_end_state;
    standard_state -> list_to_atom("state" ++ StateID ++ "_std");
    choice_state -> list_to_atom("state" ++ StateID ++ "choice_after");
    recv_after_state -> list_to_atom("state" ++ StateID ++ "_recv_after");
    branch_after_state -> list_to_atom("state" ++ StateID ++ "_branch_after");
    send_after_state -> list_to_atom("state" ++ StateID ++ "_send_after");
    select_after_state -> list_to_atom("state" ++ StateID ++ "_select_after");
    after_state -> list_to_atom("state" ++ StateID ++ "_after");
    fatal_timeout_state -> list_to_atom("state" ++ StateID ++ "_fatal_timeout");
    _Else -> list_to_atom("state" ++ StateID ++ "_unexpected_" ++ atom_to_list(State))
  end.

%% @doc 
get_next_state_trans(To, NextID) ->
  case To of 
    end_state -> {stop, normal};
    _ ->
      NextState = get_state_name(To, NextID),
      case To of 
        custom_end_state -> {next_state, NextState};
        standard_state -> {next_state, NextState};
        choice_state -> {next_state, NextState};
        recv_after_state -> {next_state, NextState};
        branch_after_state -> {next_state, NextState};
        send_after_state -> {next_state, NextState};
        select_after_state -> {next_state, NextState};
        after_state -> {next_state, NextState};
        fatal_timeout_state -> {next_state, NextState};
        _Else -> {next_state, NextState}
      end
  end.

%% TODO: process beginning and end first, so that the rest of the functions/events/actions are able to occur within main()
%% TODO: recursion: add more data too states/nodes, to signify if part of loop

%% @doc 
edge_clauses() -> ok.

state_enter(State, StateID, [_Edge|_Edges], _States) ->
  %% get next state after the transition
  % To = maps:get(Edge#edge.to, States),
  % NextID = integer_to_list(Edge#edge.to),
  % NextState = get_state_name(To, NextID),

  Name = get_state_name(State, StateID),

  % Clause = ?Q(["(CoParty) -> ","'@Name@'(CoParty, [])"])
  Clause = ["(CoParty, Data) -> "],

  Clauses = [Clause],

  {Name, Clauses}.

%% @doc state with a single outgoing transition
state_clauses(standard_state=State, StateID, [Edge|_Edges], States) ->
  %% get next state after the transition
  To = maps:get(Edge#edge.to, States),
  NextID = integer_to_list(Edge#edge.to),
  NextState = get_state_name(To, NextID),

  ?SHOW("nextstate: ~p.", [NextState]),
  ?SHOW("edge: ~p.", [Edge]),
  ?SHOW("edge_data: ~p.", [Edge#edge.edge_data]),
  reng_show(edge_data, Edge#edge.edge_data),

  {Act, Var} = Edge#edge.edge_data#edge_data.event,
  Event = Edge#edge.edge_data#edge_data.event_type,
  Dir = Edge#edge.edge_data#edge_data.trans_type,

  ?SHOW("nextstate: ~p.", [NextState]),
  ?SHOW("act: ~p.", [Act]),
  ?SHOW("var: ~p.", [Var]),
  ?SHOW("event: ~p.", [Event]),
  ?SHOW("dir: ~p.", [Dir]),

  case Dir of
    send ->
      Clause = ok;
    recv -> 
      Clause = ok;
    _ ->
      Clause = ok
  end,

  Name = get_state_name(State, StateID),
  Clauses = [Clause],

  Clauses.


%% @doc 
special_state(init_state=State, _StateID, _Edges, _States) ->
  % To = maps:get(Edge#edge.to, States),
  % Num = integer_to_list(Edge#edge.to),
  % NextState = get_next_state(To, Num),
  % Func = ?FUNCNAME,

  Name = run,
  Main = main,


  Clause1 = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc Adds default empty list for Data.",
      "% @see '@Name@'/2."
    ]),
    ?Q(["(CoParty) -> ",
        "'@Name@'(CoParty, [])"
    ])
  ),

  Clause2 = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc Called immediately after a successful initialisation.",
      "% Add any setup functionality here, such as for the contents of Data.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a list to store data inside to be used throughout the program."
    ]),
    ?Q(["(CoParty, Data) -> %% add any init/start preperations below, before entering '@Main@'",
        "",
        "'@Main@'(CoParty, Data)"
    ])
  ),

  Clauses = [Clause1, Clause2],

  % {{true, Name, Clauses}, [Main]};
  {{true, Name, Clauses}, Main};

%% @doc 
%% @see state(custom_end_state, ...)
special_state(end_state=State, StateID, Edges, States) -> state(custom_end_state, StateID, Edges, States);

%% @doc 
special_state(custom_end_state=State, _StateID, _Edges, _States) ->
  Name = stopping,

  %%
  ClauseDefault = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc Adds default reason 'normal' for stopping.",
      "% @see '@Name@'/3."
    ]),
    ?Q(["(CoParty, Data) -> ",
        "'@Name@'(normal, CoParty, Data)"])
  ),

  %%
  ClauseNormal = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc Adds default reason 'normal' for stopping.",
      "% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}."
    ]),
    ?Q(["(normal=Reason, _CoParty, _Data) -> ",
        "exit(normal)"])
  ),

  %%
  ClauseError = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc stopping with error.",
      "% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.",
      "% @param CoParty is the process ID of the other party in this binary session.",
      "% @param Data is a list to store data inside to be used throughout the program."
    ]),
    ?Q(["({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> ",
        "erlang:error(Reason, Details)"])
  ),

  %%
  ClausePartialError = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc Adds default Details to error."
    ]),
    ?Q(["({error, Reason}, CoParty, Data) when is_atom(Reason) -> ",
        "'@Name@'({error, Reason, []}, CoParty, Data)"])
  ),

  %%
  ClauseUnknown = erl_syntax:add_precomments(
    lists:map(fun(Com) -> erl_syntax:comment([Com]) end, [
      "% @doc stopping with Unexpected Reason."
    ]),
    ?Q(["(Reason, _CoParty, _Data) when is_atom(Reason) -> ",
        "exit(Reason)"])
  ),

  Clauses = [ClauseDefault, ClauseNormal, ClauseError, ClausePartialError, ClauseUnknown],

  {true, Name, Clauses};
  

special_state(Kind, StateID, Edges, States) when is_atom(Kind) ->
  ?SHOW("unexpected kind: ~p.", [Kind]),
  ?SHOW("stateid: ~p.", [StateID]),
  ?SHOW("edges: ~p.", [Edges]),
  ?SHOW("states: ~p.", [States]),
  {true, Kind, []}.

