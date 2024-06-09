-compile({nowarn_unused_function, [ {get_msg_label,1}, {get_if_label,1}, {state_name,2}, {get_outgoing_edges,2}, {to_map,1}, {pop_map,2}, {pop_map,3}, {is_edge_silent,1}, {is_edge_not_silent,1}, {is_state_recursive,2}, {get_recursive_vars,2}, {is_end_state,1} ]}).


%% @doc returns true if edge is silent
is_edge_silent(Edge) -> Edge#edge.is_silent.
is_edge_not_silent(Edge) -> not is_edge_silent(Edge).
%%

%% @doc
is_end_state(State) -> (State=:=custom_end_state) or (State=:=end_state).
%%


%% @doc for use in boolean conditions 
%% @returns true if StateID appears in RecMap
is_state_recursive(StateID, RecMap) -> length(get_recursive_vars(StateID, RecMap)) > 0.
%%

%% @doc retrieves all recursive variables bound to StateID.
%% @returns list of recursive variable names bound to StateID
get_recursive_vars(StateID, RecMap) ->
  RecStates = maps:filter(fun(_K, V) -> (V=:=StateID) end, RecMap),
  maps:keys(RecStates).
%%



%% @doc strip away prefix to get edge (msg) label
%% for sending actions
get_msg_label(#edge{edge_data=#edge_data{event = {Act, _Var},trans_type = send}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "send_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end;
%%
%% for receiving actions
get_msg_label(#edge{edge_data=#edge_data{event = {Act, _Var},trans_type = recv}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "receive_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end;
%%
%% otherwise, neither send or receive
get_msg_label(#edge{edge_data=#edge_data{event = {Act, _Var}}}=_Edge) -> 
  ?SHOW("unexpected, not send or receive:\n\t~p.",[_Edge]),
  atom_to_list(Act);
%%


%% @doc strip away prefix to get edge (msg) label
%% for sending actions
get_msg_label(#{edge_data:=#{event := {Act, _Var},trans_type := send}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "send_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end;
%%
%% for receiving actions
get_msg_label(#{edge_data:=#{event := {Act, _Var},trans_type := recv}}=_Edge) -> 
  case Label=string:prefix(atom_to_list(Act), "receive_") of
    nomatch -> atom_to_list(Act);
    _ -> Label end;
%%
%% otherwise, neither send or receive
get_msg_label(#{edge_data:=#{event := {Act, _Var}}}=_Edge) -> 
  ?SHOW("unexpected, not send or receive:\n\t~p.",[_Edge]),
  atom_to_list(Act).
%%


%% @doc strip away prefix to get edge (msg) label
%% for if 
get_if_label(#edge{edge_data=#edge_data{if_stmt=#{is_not:=false}},is_if=true,is_else=false}=_Edge) -> if_edge;
%%
%% for if not
get_if_label(#edge{edge_data=#edge_data{if_stmt=#{is_not:=true}},is_if=true,is_else=false}=_Edge) -> if_not_edge;
%%
%% for else 
get_if_label(#edge{edge_data=#edge_data{if_stmt=#{is_not:=false}},is_if=false,is_else=true}=_Edge) -> else_edge;
%%
%% for else not
get_if_label(#edge{edge_data=#edge_data{if_stmt=#{is_not:=true}},is_if=false,is_else=true}=_Edge) -> else_not_edge.
%%


%% @doc returns name of state
state_name(State, _StateID) 
when is_atom(State) 
and is_integer(_StateID) -> 
  StateID = integer_to_list(_StateID),
  case State of 
    init_state -> init_state;
    end_state -> end_state;
    custom_end_state -> custom_end_state;
    standard_state -> list_to_atom("state" ++ StateID ++ "_std");
    choice_state -> list_to_atom("state" ++ StateID ++ "choice_after");
    recv_after_state -> list_to_atom("state" ++ StateID ++ "_recv_after");
    branch_after_state -> list_to_atom("state" ++ StateID ++ "_branch_after");
    send_after_state -> list_to_atom("state" ++ StateID ++ "_send_after");
    select_after_state -> list_to_atom("state" ++ StateID ++ "_select_after");
    after_state -> list_to_atom("state" ++ StateID ++ "_after");
    if_state -> list_to_atom("state" ++ StateID ++ "_if");
    if_then_else_state -> list_to_atom("state" ++ StateID ++ "_if_else");
    fatal_timeout_state -> list_to_atom("state" ++ StateID ++ "_fatal_timeout");
    _Else -> list_to_atom("state" ++ StateID ++ "_unexpected_" ++ atom_to_list(State))
  end.
%%


%% @doc returns all of the outgoing edges from StateID
get_outgoing_edges(StateID, Edges) -> 
  %% get outgoing edges from stateID
  IsRelevant = fun(Edge) -> Edge#edge.from =:= StateID end, 
  RelevantEdges = lists:filter(IsRelevant, Edges),
  RelevantEdges.
%%


%% @doc takes record and returns as map
to_map(R) when is_record(R,edge) ->
  #{ from => R#edge.from,
     to => R#edge.to,
     is_silent => R#edge.is_silent,
     is_choice => R#edge.is_choice,
     is_if => R#edge.is_if ,
     is_else => R#edge.is_else ,
     is_timer => R#edge.is_timer ,
     is_delay => R#edge.is_delay ,
     is_error => R#edge.is_error ,
     is_internal_timeout_to_supervisor => R#edge.is_internal_timeout_to_supervisor,
     is_custom_end => R#edge.is_custom_end,
     edge_data => to_map(R#edge.edge_data) };
%%


%% @doc takes record and returns as map
to_map(R) when is_record(R,edge_data) ->
  #{ args => R#edge_data.args,
     attributes => R#edge_data.attributes,
     code => R#edge_data.code,
     comments => R#edge_data.comments,
     delay => R#edge_data.delay,
     error_reason => R#edge_data.error_reason,
     event => R#edge_data.event,
     event_type => R#edge_data.event_type,
     guard => R#edge_data.guard,
     if_stmt => R#edge_data.if_stmt,
     pattern => R#edge_data.pattern,
     timeout => R#edge_data.timeout,
     timer => R#edge_data.timer,
     trans_type => R#edge_data.trans_type };
%%

to_map([]=L) when is_list(L) -> [];

to_map([H|_]=L) when is_record(H,edge) and is_list(L) ->
  lists:foldl(fun(E, AccIn) -> AccIn++[to_map(E)] end, [], L).


pop_map(Key, Map, Default) when is_map(Map) and is_map_key(Key, Map) ->
  Value = maps:get(Key, Map, Default),
  Map1 = maps:remove(Key,Map),
  {Map1, Value}.

pop_map(Key, Map) when is_map(Map) and is_map_key(Key, Map) ->
  Value = maps:get(Key, Map),
  Map1 = maps:remove(Key,Map),
  {Map1, Value}.

