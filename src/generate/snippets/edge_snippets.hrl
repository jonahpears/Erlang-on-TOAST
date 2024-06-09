
%% @doc generates clause for actions/outgoing edges
%% @returns tuple {[EdgeClauses], [PostActionClauses]}
%% Note: PostActionClauses is only used by receiving actions (to add "end") to end




%% @doc if state (timer, not negated)
edge(#edge{edge_data=#edge_data{if_stmt=#{is_timer:=true,ref:=Timer,is_not:=false}},is_if=true,is_else=false}, {_StateData,_NextStateData}) ->
  [" receive {timeout, _TID_"++Timer++", "++timer_name(Timer)++"} -> "];
%%


%% @doc else state (timer, not negated)
edge(#edge{edge_data=#edge_data{if_stmt=#{is_timer:=true,ref:=Timer,is_not:=false}},is_if=false,is_else=true}, {_StateData,_NextStateData}) ->
  [" after 0 -> "];
%%


%% @doc if not state (timer)
edge(#edge{edge_data=#edge_data{if_stmt=#{is_timer:=true,ref:=Timer,is_not:=true}},is_if=true,is_else=false}, {_StateData,_NextStateData}) ->
  [" after 0 -> "];
%%


%% @doc else not state (timer)
edge(#edge{edge_data=#edge_data{if_stmt=#{is_timer:=true,ref:=Timer,is_not:=true}},is_if=false,is_else=true}, {_StateData,_NextStateData}) ->
  [" receive {timeout, _TID_"++Timer++", timer_"++Timer++"} -> "];
%%




%% @doc error state
edge(#edge{edge_data=#edge_data{error_reason=ErrorReason},is_error=true}, {_StateData,_NextStateData}) ->
  [" error("++atom_to_list(ErrorReason)++"), "];
%%
%% @doc for defining new timers and adding them to data
edge(#edge{edge_data=#edge_data{timer=#{duration:=Duration,name:=Name}},is_timer=true,is_delay=false}, {StateData,NextStateData}) ->
  ["{"++NextStateData++",_TID_"++Name++"} = set_timer("++Name++", "++integer_to_list(Duration)++", "++StateData++"),"];
%%
%% @doc for delay (via timer)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Timer}},is_delay=true,is_timer=false}, {StateData,_NextStateData}) 
when is_list(Timer) ->
  % ?SHOW("timer ref: ~p.",[Timer]),
  ["% (delay until timer "++Timer++" completes, error if does not exist) \n case get_timer("++Timer++", "++StateData++") of {ok, TID_"++Timer++"} -> receive {timeout, TID_"++Timer++", "++timer_name(Timer)++"} -> ok end; {ko, no_such_timer} -> error(no_such_timer) end,"];
%%
%% @doc for delay (via delay)
edge(#edge{edge_data=#edge_data{delay=#{ref:=Delay}},is_delay=true,is_timer=false}, {_StateData,_NextStateData}) 
when is_number(Delay) ->
  ["timer:sleep("++integer_to_list(floor(Delay))++"),"];
%%
%% @doc for timeout edge using timer
%% adds sneaky ';' before to allow the joining to be easier, since there must always be some other receiving action before this
edge(#edge{edge_data=#edge_data{timeout=#{ref:=Timer}},is_silent=true}=_Edge, {_StateData,_NextStateData}) 
when is_list(Timer) -> 
  ["; {timeout, _TID_"++Timer++", "++timer_name(Timer)++"} -> "];
%%
%% @doc for timeout edge using duration
edge(#edge{edge_data=#edge_data{timeout=#{ref:=Duration}},is_silent=true}=_Edge, {_StateData,_NextStateData}) 
when is_number(Duration) -> 
  ["after "++integer_to_list(floor(Duration))++" -> "];
%%
%% @doc for edge within choice
edge(#edge{edge_data=#edge_data{event = {_Act, Var} ,trans_type = TransType},is_silent=false}=Edge, {StateData,NextStateData}) -> 
  Label = get_msg_label(Edge),
  StrVar = atom_to_list(Var),
  StrPayload = "Payload_"++StrVar,
  case TransType of
    send ->["CoParty ! {self(), "++Label++", Payload},"];%,[]};
    recv -> 
      RecvClause = [%"%% recv \n",
      "{CoParty, "++Label++", "++StrPayload++"} -> "],
      DataClause = [ NextStateData++" = save_msg("++Label++", "++StrPayload++", "++StateData++"), " ],
      Clauses = RecvClause++DataClause,
      Clauses;% {Clauses, []};
    _ ->
      []%{[], []}
  end.
%%
% %% @doc for edge within choice
% edge(#edge{edge_data=#edge_data{event = {_Act, Var} ,trans_type = TransType},is_silent=false,is_choice=true}=Edge, {StateData,NextStateData}) -> 
%   Label = get_msg_label(Edge),
%   StrVar = atom_to_list(Var),
%   StrPayload = "Payload_"++StrVar,
%   case TransType of
%     send ->{["CoParty ! {self(), "++Label++", Payload},"],[]};
%     recv -> 
%       RecvClause = [%"%% recv \n",
%       "{CoParty, "++Label++", "++StrPayload++"} -> "],
%       DataClause = [ NextStateData++" = save_msg("++Label++", "++StrPayload++", "++StateData++"), " ],
%       Clauses = RecvClause++DataClause,
%       {Clauses, []};
%     _ ->
%       {[], []}
%   end;
% %%
% %% @doc for lone action (prewrap in recv)
% edge(#edge{edge_data=#edge_data{event = {_Act, Var} ,trans_type = TransType},is_silent=false,is_choice=false}=Edge, {StateData,NextStateData}) -> 
%   case TransType of
%     send -> %{["CoParty ! {self(), "++get_msg_label(Edge)++", Payload},"],[]}; % edge(Edge#edge{is_choice=true},{StateData,NextStateData});
%       Label = get_msg_label(Edge),
%       StrVar = atom_to_list(Var),
%       StrPayload = "Payload_"++StrVar,
%       PayloadClause = [StrPayload++" = \'payload\',"],
%       SendClause = ["CoParty ! {self(), "++Label++", Payload_"++StrVar++"},"],
%       Clauses = PayloadClause++SendClause,
%       {Clauses, []};
%     recv -> 
%       {Clauses, PostClause} = edge(Edge#edge{is_choice=true},{StateData,NextStateData}),
%       {["receive "]++Clauses, PostClause++["end"]};
%     _ ->
%       {[], []}
%   end;
% %%
% %% @doc other case
% edge(_Edge, {_StateData,_NextStateData}) -> {[],[]}.
%%
