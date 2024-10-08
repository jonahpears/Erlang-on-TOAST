-module(toast_process).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).

-include_lib("stdlib/include/assert.hrl").

-type party () :: atom().

-type payload () :: undefined | any().

-type label () :: atom().

-type msg() :: {label(), payload()}.

-type dir() :: send | recv.

-type timer() :: string().

-type upper_bound () :: {leq, integer()} | {les, integer()} | 'infinity' | 0.

-type equalities() :: eq | neq.
-type greaters() :: geq | gtr.
-type lessers() :: leq | les.
-type dbc() :: equalities() | greaters() | lessers().

-type simple_range() :: {string(), dbc(), integer()}.

-type nondet_delay () :: integer() | {t, dbc(), integer()} 
                      %  | {integer(), greaters(), t, lessers(), integer()} %% ! <- this is experimental, not in theory!
                       .

-type queue_contents () :: empty | {msg(), queue_contents()}.

-type rec_var () :: {string(), {[msg()],[party()]}}.

%% toast processes (unsupported, outside scope of tool)
out_of_scope_processes() -> [queue_process, parl_process, session_process].


%% {from, to}
-type queue_process () :: {party(), party(), queue_contents()}.

%% {left, right}
-type parl_process () :: {toast_process(), toast_process()}.

%% {scope, {p,q}, session}
-type session_process () :: {'scope', {party(), party()}, toast_process()}.

%% toast processes (supported by tool)
supported_processes() -> [error_process, termination_process, send_process, branch_process, timeout_process, set_process, delay_process, if_process, def_process, call_process].

-type error_process () :: error.
-type termination_process () :: term.

%% {to, msg, next}
-type send_process () :: {party(), '<-', msg(), toast_process()}.

%% {from, within, [msg, next]}
-type recv_process () :: {party(), '->', upper_bound(), msg(), toast_process()}.
-type branch_process () :: {party(), '->', upper_bound(), [{msg(), toast_process()}]}.

%% {from, within, [msg, next], after, other}
-type timeout_process () :: {branch_process(), 'after', toast_process()}.
% -type cotimeout_process () :: {send_process(), 'after', toast_process()}.

%% {set, timer, next}
-type set_process () :: {'set', timer(), toast_process()}.

%% {delay, duration, next}
-type delay_process () :: {'delay', nondet_delay(), toast_process()}.

%% {if, {timer, dbc, constant}, then, P, else, Q}
-type if_process () :: {'if', simple_range(), 'then', toast_process(), 'else', toast_process()}.

%% {def, process_to_unfold, as, {name, [msgs], [timers]}}
%% {def, process_to_unfold, as, {name, [msgs], [timers]}, in, unfolding} %% <- is unfolded into (from above)
-type def_process () :: {'def', toast_process(), 'as', rec_var()}
                      | {'def', toast_process(), 'as', rec_var(), 'in', toast_process()}.

%% {call, {name, [msgs], [timers]}}
-type call_process () :: {'call', rec_var()}.


-type toast_process () :: termination_process() 
                        | send_process()
                        | recv_process()
                        | branch_process()
                        | timeout_process()
                        | set_process()
                        | if_process()
                        | delay_process() %% only delays of integer values should be directly honoured in protocol
                        | def_process()
                        | call_process()
                        | parl_process()
                        | session_process()
                        | error_process()
                      .


%% @doc takes toast process and returns input protocol
-spec to_protocol({atom(),toast_process()}) -> interleave:protocol().
to_protocol({Name, Process}) -> 
  Map = #{name=>Name,rec_vars=>#{},pending_timers=>#{},timers_to_set=>[],flags=>#{}},
  % Map = #{name=>Name,rec_vars=>#{},timers_to_set=>[],trace=>#{head=>undefined,body=>#{},next_action=>undefined,prev_action=>undefined}},
  {Protocol, Map1} = to_protocol(Process, Map),
  %% check if Map3 contains any requirements for Timer
  {Protocol1, _Map2} = resolve_pending_timers(all,Protocol,Map1),
  %% catch any pending timers
  Protocol1.
%%

-spec to_protocol(toast_process(),map()) -> {interleave:protocol(),map()}.

%% @doc termination process
to_protocol(term, Map) -> {'endP', Map};

%% @doc error process
to_protocol(error, Map) -> {'error', Map};

%% @doc recursive call process
to_protocol({'call', {Name, {Msgs, Roles}}}, #{rec_vars:=RecVars}=Map)
when is_list(Msgs) and is_list(Roles) -> 
  %% make sure is defined
  ?assert(is_map_key(Name,RecVars)),
  %% make sure each msg/role required to callback is defined
  #{Name:={RecMsgs,RecRoles}} = RecVars,
  ?assert(lists:foldl(fun(Msg,MsgIn) -> MsgIn and lists:member(Msg,Msgs) end, true, RecMsgs)),
  ?assert(lists:foldl(fun(Role,RoleIn) -> RoleIn and lists:member(Role,Roles) end, true, RecRoles)),
  %% return call
  {{'rvar', Name}, Map};
%%

%% @doc recursive definition process
to_protocol({'def', P, 'as', {Name, {Msgs, Roles}}}, #{rec_vars:=RecVars}=Map)
when is_map(Map) and is_list(Msgs) and is_list(Roles) -> 
  %% make sure not already defined
  ?assert(not is_map_key(Name,RecVars)),
  %% add new def to map
  Map1 = maps:put(rec_vars,maps:put(Name,{Msgs,Roles},RecVars),Map),
  %% clear flags
  Map2 = maps:put(flags,#{},Map1),
  %% begin unfolding
  to_protocol({'def', P, 'as', {Name, {Msgs,Roles}}, 'in', P}, Map2);
%%

%% @doc recursive definition process unfolding
to_protocol({'def', _P, 'as', {Name, {Msgs, Roles}}, 'in', Q}, #{rec_vars:=RecVars}=Map)
when is_map(Map) and is_map_key(Name,RecVars) and is_list(Msgs) and is_list(Roles) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% continue unfolding
  {Protocol, Map2} = to_protocol(Q, Map1),
  %% return protocol
  {{rec, Name, Protocol}, Map2};
%%

%% @doc set process timer
to_protocol({'set', Timer, P}, Map) 
when is_map(Map) and is_list(Timer) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% add to timers_to_set map
  Map2 = maps:update_with(timers_to_set, fun(V) -> lists:uniq(V++[Timer]) end, [Timer], Map1),
  %% continue mapping
  {Protocol, Map3} = to_protocol(P, Map2),
  % io:format("\nmap2:\t~p.\n",[Map2]),
  %% remove current timer from timers_to_set
  Map4 = maps:update_with(timers_to_set, fun(V) -> lists:delete(Timer,V) end, [], Map3),
  %% check if Map3 contains any requirements for Timer 
  %% and go through continuation and update timer name to match
  resolve_pending_timers(Timer,Protocol,Map4);
%%

%% @doc non-deterministic delay process
to_protocol({'delay', {t, DBC, Const}=Delay, P}, #{flags:=Flags}=Map) 
when is_map(Map) and is_atom(DBC) and is_integer(Const) -> 
  %% unsure of value to pick, choose const
  % io:format("\nFound non-deterministic delay:\t~p,\nuncertain how to proceed, using constant (~p) for delay value.\n(raised flag to allow potential for non-blocking send after snippet.)\n",[Delay,Const]),
  Value = Const,
  %% resolve to value and continue
  % to_protocol({'delay', Value, P}, maps:put(flags,maps:merge(Flags,#{non_det_delay=>true}),Map));
  {Protocol1, Map1} = to_protocol(P, maps:put(flags,maps:merge(Flags,#{non_det_delay=>true}),Map)),
  %% check if send after
  case is_map_key(send_after,maps:get(flags,Map1)) of

    %% if true, then Protocol1 
    true -> Protocol2 = Protocol1,
      io:format("\nFound non-deterministic delay:\t~p,\nraised flag and successfully converted to send_after structure.\n",[Delay]);

    
    %% wrap in delay and return
    _ -> Protocol2 = {'delay', Value, Protocol1},
      io:format("\nFound non-deterministic delay:\t~p,\nuncertain how to proceed, using constant (~p) for delay value.\n",[Delay,Const])

  end,
  %% return protocol
  {Protocol2, Map1};
%%

%% @doc deterministic delay process
to_protocol({'delay', Delay, P}, Map) 
when is_map(Map) and is_integer(Delay) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% continue mapping
  {Protocol1, Map2} = to_protocol(P, maps:put(flags,#{},Map1)),
  %% wrap in delay and return
  {{'delay', Delay, Protocol1}, Map2};
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% single send/recv actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc send process that has nonblocking dependency
to_protocol({To, '<-', {Label,_Payload},P}, #{flags:=#{non_det_delay:=true,if_statement:=true}}=Map) 
when is_map(Map) and is_atom(To) and is_atom(Label) -> 
  %% continue mapping
  {Protocol1, Map1} = to_protocol(P, Map),
  %% wrap in act s_ and return
  ActLabel = list_to_atom("s_"++atom_to_list(Label)),
  {{'act', ActLabel, Protocol1}, maps:put(flags,#{send_after=>true},Map1)};
%%

%% @doc send process
to_protocol({To, '<-', {Label,_Payload},P}, Map) 
when is_map(Map) and is_atom(To) and is_atom(Label) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% continue mapping
  {Protocol1, Map2} = to_protocol(P, Map1),
  %% wrap in act s_ and return
  ActLabel = list_to_atom("s_"++atom_to_list(Label)),
  {{'act', ActLabel, Protocol1}, Map2};
%%

%% @doc recv process
to_protocol({From, '->', UpperBound, {Label,_Payload}, P}, Map) 
when is_map(Map) and is_atom(From) and is_atom(Label) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% continue mapping
  {Protocol1, Map2} = to_protocol(P, Map1),
  %% wrap in act r_ and return
  ActLabel = list_to_atom("r_"++atom_to_list(Label)),
  %% warn against having non-infinite durations without timeout
  case UpperBound of 
    infinity ->
      Protocol2 = {'act', ActLabel, Protocol1};
    _ -> 
      {_DBC, Duration} = UpperBound,
      ?assert(is_integer(Duration)),
      io:format("\nWarning, non-infinite upper bound specified (~p), but no timeout present.\nSuch behaviour may get stuck in cases where communication is not reliable.\nIn order to enforce such behaviour, the protocol specifies an error state has been reached if the upper bound is exceeded.\n",[Duration]),
      %% add error process to timeout
      Protocol2 = {'act', ActLabel, Protocol1, 'aft', Duration, error}
  end,
  %% return protocol
  {Protocol2, Map2};
%%

%% @doc recv process (wrapped in list)
to_protocol({From, '->', Duration, [{{Label,Payload},P}]=Branches}, Map) 
when is_map(Map) and is_atom(From) and is_atom(Label) and is_list(Branches) -> 
  %% make sure only one branch
  ?assert(length(Branches)==1),
  %% return protocol via unwrapped mapping
  to_protocol({From, '->', Duration, {Label,Payload}, P}, Map);
%%

%%%%%%%%%%%%%%%%%%%
%%% branch process
%%%%%%%%%%%%%%%%%%%

%% @doc branch process
to_protocol({From, '->', UpperBound, [{{_Label,_Payload},_P}|_T]=Branches}, Map) 
when is_map(Map) and is_atom(From) and is_atom(_Label) and is_list(Branches) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% make sure more than one branch
  ?assert(length(Branches)>1),
  %% create branches list
  {Branches1,Map2} = lists:foldl(fun({{Label,__Payload},P},{InBranches,InMap}) -> 
    %% continue mapping 
    {BranchProtocol, BranchMap} = to_protocol(P,InMap),
    %% add to branches, wrapped in label (without unnecessary prefix r_)
    {InBranches++[{Label, BranchProtocol}], BranchMap}
  end, {[],Map1}, Branches),
  %% warn against having non-infinite durations without timeout
  case UpperBound of 
    infinity ->%% return protocol
      Protocol = {'branch', Branches1};
    _ -> 
      {_DBC, Duration} = UpperBound,
      ?assert(is_integer(Duration)),
      io:format("\nWarning, non-infinite upper bound specified (~p), but no timeout present.\nSuch behaviour may get stuck in cases where communication is not reliable.\nIn order to enforce such behaviour, the protocol specifies an error state has been reached if the upper bound is exceeded.\n",[Duration]),
      %% add error process to timeout
      Protocol = {'branch', Branches1, 'aft', Duration, error}
  end,  
  %% return protocol
  {Protocol, Map2};
%%

%%%%%%%%%%%%%%%%%%%%
%%% timeout process
%%%%%%%%%%%%%%%%%%%%

%% @doc recv-after process
to_protocol({From, '->', UpperBound, {{Label,Payload},P}, 'after', Timeout}, Map)
when is_map(Map) and is_atom(From) ->
  %% clear flags
  Map1 = maps:put(flags,#{},Map), 
  %% utilise previous methods, but ensure no timeout is added to end
  {Protocol, Map2} = to_protocol({From, '->', infinity, {Label,Payload}, P}, Map1),
  %% unpack
  {'act', LabelP, NextP} = Protocol,
  %% warn about having infinite timeout
  case UpperBound of 
    infinity -> 
      io:format("\nWarning, infinite upper bound specified for timeout.\nSince the timeout branch will never be reached, and erlang timers cannot be set to infinity, the timeout branch has been removed.\n"),
      %% set protocol and map
      Protocol2 = Protocol,
      Map3 = Map2;
    _ -> 
      {_DBC, Duration} = UpperBound,
      ?assert(is_integer(Duration)),
      %% continue mapping 
      {Protocol1, Map3} = to_protocol(Timeout, Map2),
      %% set protocol
      Protocol2 = {'act', LabelP, NextP, 'aft', Duration, Protocol1}
  end,  
  %% return protocol
  {Protocol2, Map3};
%%

%% @doc recv-after process (wrapped in list)
to_protocol({From, '->', UpperBound, [{{Label,Payload},P}]=Branches, 'after', Timeout}, Map) 
when is_map(Map) and is_atom(From) and is_atom(Label) and is_list(Branches) -> 
  %% make sure only one branch
  ?assert(length(Branches)==1),
  %% return protocol via unwrapped mapping
  to_protocol({From, '->', UpperBound, {{Label,Payload},P}, 'after', Timeout}, Map);
%%

%% @doc timeout process
to_protocol({From, '->', UpperBound, [{{_Label,_Payload},_P}|_T]=Branches, 'after', Timeout}, Map)
when is_map(Map) and is_atom(From) and is_list(Branches) -> 
  %% clear flags
  Map1 = maps:put(flags,#{},Map),
  %% utilise previous methods, but ensure no timeout is added to end
  {Protocol, Map2} = to_protocol({From, '->', infinity, Branches}, Map1),
  %% unpack
  {'branch', Branches1} = Protocol,
  %% warn about having infinite timeout
  case UpperBound of 
    infinity -> 
      io:format("\nWarning, infinite upper bound specified for timeout.\nSince the timeout branch will never be reached, and erlang timers cannot be set to infinity, the timeout branch has been removed.\n"),
      %% set protocol and map
      Protocol2 = Protocol,
      Map3 = Map2;
    _ -> 
      {_DBC, Duration} = UpperBound,
      ?assert(is_integer(Duration)),
      %% continue mapping 
      {Protocol1, Map3} = to_protocol(Timeout, Map2),
      %% set protocol
      Protocol2 = {'branch', Branches1, 'aft', Duration, Protocol1}
  end,  
  %% return protocol
  {Protocol2, Map3};
%%


%%%%%%%%%%%%%%%%%%%
%%% select process
%%%%%%%%%%%%%%%%%%%
to_protocol({'if', {Name, DBC, Const}=_Constraint, 'then', P, 'else', Q}, #{timers_to_set:=Timers,pending_timers:=Pending,flags:=Flags}=Map)
when is_map(Map) and is_list(Name) and is_integer(Const) -> 
  %% check if timer has been defined previously, warn if not and arrange for it to be set at the beginning of the protocol
  case lists:member(Name,Timers) of true -> ok; _ -> io:format("\nWarning, an 'if' conditional uses timer (~p) which has not yet been set.\nThis is not a valid process and has been corrected by specifying to set the timer at the very beginning of the protocol.\n",[Name]) end,
  Map0 = maps:put(flags,maps:merge(Flags,#{if_statement=>true}),Map),
  %% continue mapping P
  {ProtocolP, MapP} = to_protocol(P, Map0),
  %% continue mapping Q
  {ProtocolQ, MapQ} = to_protocol(Q, Map0),

  %% merge maps p and q
  Map1 = maps:merge_with(fun(K, VP, VQ) -> 
    case K of 
      rec_vars -> maps:merge(VP,VQ);

      pending_timers -> maps:merge(VP,VQ);

      timers_to_set -> lists:uniq(VP++VQ);

      flags -> maps:merge(VP,VQ);

      _ -> %% by default, just use old value passed in
        maps:get(K,Map)
  end end, MapP, MapQ),

  % io:format("\n\nif,\n\nMap0:\t~p,\n\nProtocolP:\t~p,\n\nProtocolQ:\t~p,\n\nMapP:\t~p,\n\nMapQ:\t~p.\n",[Map0,ProtocolP,ProtocolQ,MapP,MapQ]),
  % timer:sleep(2000),

  %% check if MapP contains flag for changing this to send after
  case is_map_key(send_after,maps:get(flags,MapP)) of

    %% if true, then ProtocolP is just the beginning, of {act, s_, P, ...}
    true -> 

      %% map does not change, as timer not being added 
      Map2 = Map1,

      %% determine if this condition should be inverted
      Invert = case DBC of 
        geq -> true;
        gtr -> true;
        leq -> false;
        les -> false;
        eq -> true,
          %% TODO :: figure out how to implement this consistently, in the monitors and stubs
          io:format("\nWarning, constraint uses (eq) which is currently not fully supported.\nThis is due to difficulties in ensuring a timer has finished recently enough to be considered to adhere to `equal to' constraints.\n")
      end,

      %% construct the rest of the protocol
      case Invert of 

        true -> 
          {act, MsgQ, NextQ} = ProtocolQ,
          Protocol = {act, MsgQ, NextQ, 'aft', Const, ProtocolP};


        _ -> 
          {act, MsgP, NextP} = ProtocolP,
          Protocol = {act, MsgP, NextP, 'aft', Const, ProtocolQ}


      end;

    %% otherwise, continue as normal
    _ -> 

      %% add to pending timers to be passed back out 
      Map2 = maps:put(pending_timers, maps:update_with(Name, fun(V) -> lists:uniq(V++[Const]) end, [Const], Pending), Map1),
      %% create name for timer
      TimerName = Name++integer_to_list(Const),

      %% check if MapQ contains flag for changing this to send after
      case is_map_key(send_after,maps:get(flags,MapQ)) of

        %% if true, then ProtocolQ is just the beginning, of {act, s_, Q, ...}
        true -> 

          %% determine if this condition should be inverted
          Invert = case DBC of 
            geq -> true;
            gtr -> true;
            leq -> false;
            les -> false;
            eq -> true,
              %% TODO :: figure out how to implement this consistently, in the monitors and stubs
              io:format("\nWarning, constraint uses (eq) which is currently not fully supported.\nThis is due to difficulties in ensuring a timer has finished recently enough to be considered to adhere to `equal to' constraints.\n")
          end,

          %% construct the rest of the protocol
          case Invert of 

            true -> 
              {act, MsgP, NextP} = ProtocolP,
              Protocol = {act, MsgP, NextP, 'aft', Const, ProtocolQ};


            _ -> 
              {act, MsgQ, NextQ} = ProtocolQ,
              Protocol = {act, MsgQ, NextQ, 'aft', Const, ProtocolP}


          end;

        %% otherwise, continue as normal
        _ -> 

        %% determine if this condition should be inverted
        case DBC of 
          geq -> Head = 'if_timer';
          gtr -> Head = 'if_timer';
          leq -> Head = 'if_not_timer';
          les -> Head = 'if_not_timer';
          eq -> Head = 'if_timer',
            %% TODO :: figure out how to implement this consistently, in the monitors and stubs
            io:format("\nWarning, constraint uses (eq) which is currently not fully supported.\nThis is due to difficulties in ensuring a timer has finished recently enough to be considered to adhere to `equal to' constraints.\n")
        end,

        Protocol = {Head, TimerName, ProtocolP, 'else', ProtocolQ}

      end
  end,
  %% return protocol
  {Protocol, Map2};
%%

%% @doc forgot to add upperbound to timeout process
to_protocol({From, '->', Branches}, Map)
when is_map(Map) and is_atom(From) and is_list(Branches) ->
  io:format("\nWarning, it looks like you forgot to set an upper-bound for a timeout process.\nBy default, an upper-bound of `infinity` has been set.\n"),
  to_protocol({From, '->', infinity, Branches}, Map);
%%

%% @doc forgot to add upperbound to recv-after process
to_protocol({From, '->', Branch, P}, Map)
when is_map(Map) and is_atom(From) ->
  io:format("\nWarning, it looks like you forgot to set an upper-bound for a receive-after process.\nBy default, an upper-bound of `infinity` has been set.\n"),
  to_protocol({From, '->', infinity, Branch, P}, Map);
%%

%% @doc unhandled processes
to_protocol(_TOAST, Map) 
when is_map(Map) -> 
  io:format("\nname:\t\t~p,\n\nunhandled:\t~p,\n\nmap:\t~p\n\n",[maps:get(name,Map,no_name_given),_TOAST,Map]),
  timer:sleep(2000),
  {term, Map}.
%%

%% @doc for updating on the way in or out the last action
% %% @doc on way out, tell the level above what the next action is
% update_trace_actions(out, {_Direction, _Label}=Action, #{trace:=#{next_action:=_NextAction,prev_action:=_PrevAction}=Trace}=Map)
% when is_map(Map) ->
%   Trace1 = maps:put(next_action,Action,Trace),
%   % Trace2 = maps:put(prev_action,undefined,Trace1),
%   maps:put(trace,Trace1,Map);
% %%

% %% @doc on way in, tell the level below what the previous action is
% update_trace_actions(in, {_Direction, _Label}=Action, #{trace:=#{next_action:=_NextAction,prev_action:=_PrevAction}=Trace}=Map)
% when is_map(Map) ->
%   Trace1 = maps:put(prev_action,Action,Trace),
%   % Trace2 = maps:put(next_action,undefined,Trace1),
%   maps:put(trace,Trace1,Map).
% %%

%% @doc for validating structure of process
% validate()

%% @doc helper function for resolving all pending timers.
resolve_pending_timers(all,Protocol,#{pending_timers:=PendingTimers}=Map) ->
  %% for each in pending timers, resolve
  {Protocol1, Map1} = maps:fold(fun(Timer,_Value,{InProtocol,InMap}) -> 
    resolve_pending_timers(Timer,InProtocol,InMap)
  end, {Protocol,Map}, PendingTimers),
  %% return
  {Protocol1, Map1};
%%

%% @doc helper function for resolving pending timers at the current level.
%% wraps a given protocol in a series of set timer protocols.
resolve_pending_timers(Timer,Protocol,Map) ->
  %% check if Map3 contains any requirements for Timer
  #{pending_timers:=PendingTimers} = Map,
  case is_map_key(Timer,PendingTimers) of 
    true -> %% get pending values
      #{Timer:=PendingValues} = PendingTimers,
      %% for each value, add reset to before protocol
      Protocol1 = lists:foldl(fun(Pend,InSet) -> 
        ?assert(is_integer(Pend)),
        %% create timer specific to this requirement, wrap around outside of inner protocol
        Name = Timer++integer_to_list(Pend),
        {'timer', Name, Pend, InSet}
      end, Protocol, PendingValues),
      %% remove from pending timers
      Map1 = maps:put(pending_timers, maps:remove(Timer,PendingTimers), Map),
      %% return protocol
      {Protocol1, Map1};
    _ -> %% return as is
      {Protocol, Map}
  end.
%%

output_location() -> "tool_output/mappings/".


%% @doc automatically generates protocol from input,
process_to_stub(Name)
when is_atom(Name) ->
  %% get protocol
  {ok, _Process, Protocol} = map_process_to_protocol(Name),
  %% pass to generator
  gen_stub:gen(Name,Protocol,"_process_to_stub.erl"),
  ok.
%%

%% @doc given a name corresponding to some maptest process, writes and returns the corresponding protocol the process maps to.
map_process_to_protocol(Name) ->
  %% get process
  Process = maptest(Name),
  io:format("\n+ + + + + + + + + + +\n\nbeginning new mapping of process to protocol...\n\nname:\t\t~p,\n\nprocess:\t~p.\n\n- - - - - - - - - - -\n",[Name,Process]),
  %% get protocol via mapping function
  Protocol = to_protocol({Name,Process}),
  io:format("\nname:\t\t~p,\n\nprocess:\t~p,\n\nprotocol:\t~p.\n",[Name,Process,Protocol]),
  %% save to file
  case file:write_file(output_location()++atom_to_list(Name)++".erl", io_lib:fwrite("process_~p() -> ~p.\nprotocol_~p() -> ~p.\n",[Name,Process,Name,Protocol])) of
    ok -> ok;
    Else -> io:format("\n> > file:write_file failed: ~p.\n", [Else])
  end,
  {ok, Process, Protocol}.
%%


all() -> run_all_tests().

run_all_tests() ->
  %% make sure output has a folder
  case filelib:ensure_dir(output_location()) of
    ok -> ok;
    _Err -> io:format("\nWarning, output directory (~p) could not be created.\n\nMaybe try creating it yourself and running the program again.\n",[output_location()])
  end,
  io:format("\n\nbeginning tests...\n"),
  %% basic communication
  ok=run_tests(header, send),
  ok=run_tests(header, recv),
  %% branching receptions & timeouts
  ok=run_tests(header, branch),
  ok=run_tests(header, basic_timeout),
  ok=run_tests(header, recv_after),
  ok=run_tests(header, nested_timeout),
  ok=run_tests(header, mix_timeout),
  %% selections & cotimeouts
  ok=run_tests(header, select),
  ok=run_tests(header, basic_cotimeout),
  ok=run_tests(header, send_after),
  ok=run_tests(header, nested_cotimeout),
  ok=run_tests(header, mix_cotimeout),
  %% recursion
  ok=run_tests(header, linear_recursion),
  ok=run_tests(header, nonlinear_recursion),
  %% other tests
  ok=run_tests(header, other),
  %%
  io:format("\nfinished all tests.\n"),
  ok.

run_test(Name) -> {ok, _, _} = map_process_to_protocol(Name), ok.

run_tests(header, Name) ->
  io:format("\n= = = = = = = = = = = = = = = =\n\nrunning tests:\t~p.\n",[Name]),
  run_tests(Name).

run_tests(send) ->
  %% send
  ok=run_test(send_only),
  ok=run_test(send_twice),
  ok=run_test(send_thrice),
  ok;
  
run_tests(recv) ->
  %% recv
  ok=run_test(recv_only),
  ok=run_test(recv_twice),
  ok=run_test(recv_thrice),
  ok;

run_tests(branch) ->
  %% branch
  ok=run_test(branch_only),
  ok=run_test(branch_twice),
  ok=run_test(branch_thrice),
  ok;

run_tests(basic_timeout) ->
  %% basic timeout
  ok=run_test(timeout_send_only),
  ok=run_test(timeout_recv_only),
  ok;

run_tests(recv_after) ->
  %% recv after
  ok=run_test(recv_after_send),
  ok=run_test(recv_after_recv),
  ok;

run_tests(nested_timeout) ->
  %% nested timeouts
  ok=run_test(timeout_send_twice),
  ok=run_test(timeout_send_thrice),
  ok=run_test(timeout_recv_twice),
  ok=run_test(timeout_recv_thrice),
  ok;

run_tests(mix_timeout) ->
  %% nested alternating timeouts
  ok=run_test(timeout_send_recv),
  ok=run_test(timeout_recv_send),
  ok;

run_tests(select) ->
  %% select (leq)
  ok=run_test(select_one_from_two_with_one_timer_leq),
  ok=run_test(select_one_from_three_with_one_timer_leq),
  ok=run_test(select_one_from_four_with_two_timers_leq),
  ok=run_test(select_with_resets_leq),
  %% select (geq)
  ok=run_test(select_one_from_two_with_one_timer_geq),
  ok=run_test(select_one_from_three_with_one_timer_geq),
  ok=run_test(select_one_from_four_with_two_timers_geq),
  ok=run_test(select_with_resets_geq),
  ok;

run_tests(basic_cotimeout) ->
  %% basic cotimeout
  ok=run_test(cotimeout_send_only),
  ok=run_test(cotimeout_recv_only),
  ok;

run_tests(send_after) ->
  %% send after
  ok=run_test(send_after_send),
  ok=run_test(send_after_recv),
  ok;

run_tests(nested_cotimeout) ->
  %% nested cotimeouts
  ok=run_test(cotimeout_send_twice),
  ok=run_test(cotimeout_send_thrice),
  ok=run_test(cotimeout_recv_twice),
  ok=run_test(cotimeout_recv_thrice),
  ok;

run_tests(mix_cotimeout) ->
  %% nested alternating timeouts
  ok=run_test(cotimeout_send_recv),
  ok=run_test(cotimeout_recv_send),
  ok;

run_tests(linear_recursion) ->
  %% linear recursion
  ok=run_test(linear_recursion_send_recv),
  ok=run_test(linear_recursion_recv_send),
  ok;

run_tests(nonlinear_recursion) ->
  %% nonlinear recursion
  ok=run_test(nonlinear_recursion_send_recv),
  ok=run_test(nonlinear_recursion_recv_send),
  ok;

run_tests(other) ->
  %% recv-after/if-statement equivalence testing
  ok=run_test(nonlinear_recursion_recv_send_using_if),
  ok=run_test(nonlinear_recursion_recv_send_using_branch),
  %% check timer qol catch works
  ok=run_test(misc_catch_unset_timers),
  ok;

run_tests(_) -> run_test(undefined).



%% @doc tests for the above mapping
-spec maptest(atom()) -> toast_process().

%%%%%%%%%%%%%%
%%% send only
%%%%%%%%%%%%%%
maptest(send_only) ->
  {p, '<-', {msg_A, undefined}, term};

maptest(send_twice) ->
  {p, '<-', {msg_A, undefined}, {p, '<-', {msg_B, undefined}, term}};

maptest(send_thrice) ->
  {p, '<-', {msg_A, undefined}, {p, '<-', {msg_B, undefined}, {p, '<-', {msg_C, undefined}, term}}};


%%%%%%%%%%%%%%
%%% recv only
%%%%%%%%%%%%%%
maptest(recv_only) ->
  {p, '->', infinity, [{{msg_A, undefined}, term}]};

maptest(recv_twice) ->
  {p, '->', infinity, {msg_A, undefined}, 
    {p, '->', infinity, {msg_B, undefined}, term}};

maptest(recv_thrice) ->
  {p, '->', infinity, {msg_A, undefined}, 
    {p, '->', infinity, {msg_B, undefined}, 
      {p, '->', infinity, {msg_C, undefined}, term}}};


%%%%%%%%%%%%%%%%
%%% branch only
%%%%%%%%%%%%%%%%
maptest(branch_only) ->
  {p, '->', infinity, 
      [ {{msg_A, undefined}, term},
        {{msg_B, undefined}, term} ]};

maptest(branch_twice) ->
  {p, '->', infinity, 
      [ {{msg_A, undefined},  {p, '->', infinity, 
                                  [ {{msg_C, undefined}, term},
                                    {{msg_D, undefined}, term} ]}},
        {{msg_B, undefined},  {p, '->', infinity, 
                                  [ {{msg_E, undefined}, term},
                                    {{msg_F, undefined}, term} ]}} ]};

maptest(branch_thrice) ->
  {p, '->', infinity, 
      [ {{msg_A, undefined},  {p, '->', infinity, 
                                  [ {{msg_C, undefined},  {p, '->', infinity, 
                                                              [ {{msg_G, undefined}, term},
                                                                {{msg_H, undefined}, term} ]}},
                                    {{msg_D, undefined},  {p, '->', infinity, 
                                                              [ {{msg_I, undefined}, term},
                                                                {{msg_J, undefined}, term} ]}} ]}},
        {{msg_B, undefined},  {p, '->', infinity, 
                                  [ {{msg_E, undefined},  {p, '->', infinity, [ {{msg_K, undefined}, term},
                                                                                {{msg_L, undefined}, term} ]}},
                                    {{msg_F, undefined},  {p, '->', infinity, [ {{msg_M, undefined}, term},
                                                                                {{msg_N, undefined}, term} ]}} ]}} ]};

%%%%%%%%%%%%%%%%%%%%%%%
%%% timeout send only
%%%%%%%%%%%%%%%%%%%%%%%
maptest(timeout_send_only) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined}, term},
        {{msg_B, undefined}, term} ],
      'after', {p, '<-', {msg_C, undefined}, term}};

maptest(timeout_send_twice) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_D, undefined}, term},
                                    {{msg_E, undefined}, term} ],
                                  'after', {p, '<-', {s_MsgF, undefined}, term}}},
        {{msg_B, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_G, undefined}, term},
                                    {{msg_H, undefined}, term} ],
                                  'after', {p, '<-', {s_MsgI, undefined}, term}}} ],
      'after', {p, '<-', {msg_C, undefined}, term}};

%%%%%%%%%%%%%%%%%%%%%%%
%%% timeout recv only
%%%%%%%%%%%%%%%%%%%%%%%
maptest(timeout_recv_only) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined}, term},
        {{msg_B, undefined}, term} ],
      'after', {p, '->', infinity, {msg_C, undefined}, term}};

maptest(timeout_recv_twice) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_D, undefined}, term},
                                    {{msg_E, undefined}, term} ],
                                  'after', {p, '->', {s_MsgF, undefined}, term}}},
        {{msg_B, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_G, undefined}, term},
                                    {{msg_H, undefined}, term} ],
                                  'after', {p, '->', {s_MsgI, undefined}, term}}} ],
      'after', {p, '->', infinity, {msg_C, undefined}, term}};


%%%%%%%%%%%%%%%%%%%%%%%
%%% timeout mix only
%%%%%%%%%%%%%%%%%%%%%%%
maptest(timeout_send_recv) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_D, undefined}, term},
                                    {{msg_E, undefined}, term} ],
                                  'after', {p, '->', {s_MsgF, undefined}, term}}},
        {{msg_B, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_G, undefined}, term},
                                    {{msg_H, undefined}, term} ],
                                  'after', {p, '->', {s_MsgI, undefined}, term}}} ],
      'after', {p, '<-', {msg_C, undefined}, term}};

maptest(timeout_recv_send) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_D, undefined}, term},
                                    {{msg_E, undefined}, term} ],
                                  'after', {p, '<-', {s_MsgF, undefined}, term}}},
        {{msg_B, undefined},  {p, '->', {leq, 5}, 
                                  [ {{msg_G, undefined}, term},
                                    {{msg_H, undefined}, term} ],
                                  'after', {p, '<-', {s_MsgI, undefined}, term}}} ],
      'after', {p, '->', infinity, {msg_C, undefined}, term}};


%%%%%%%%%%%%%%%%%%%%%%
%%% select only (leq)
%%%%%%%%%%%%%%%%%%%%%%
maptest(select_one_from_two_with_one_timer_leq) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        p, '<-', {msg_B, undefined}, term
      }
    }
  }};

maptest(select_one_from_three_with_one_timer_leq) ->
  {set, "x", {
    delay, {t, leq, 10}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        'if', {"x", leq, 10}, 'then', {
          p, '<-', {msg_B, undefined}, term
        }, 'else', {
          p, '<-', {msg_C, undefined}, term
        }
      }
    }
  }};

maptest(select_one_from_four_with_two_timers_leq) ->
  {set, "x", {delay, {t, leq, 5}, {
    set, "y", {delay, {t, geq, 5}, {
      'if', {"x", leq, 10}, 'then', {
        'if', {"y", leq, 5}, 'then', {
          p, '<-', {msg_A, undefined}, term
        }, 'else', {
          p, '<-', {msg_B, undefined}, term
        }
      }, 'else', {
        'if', {"y", leq, 5}, 'then', {
          p, '<-', {msg_C, undefined}, term
        }, 'else', {
          p, '<-', {msg_D, undefined}, term
        }
      }
    }
  }}}};

maptest(select_with_resets_leq) ->
  {set, "x", {delay, {t, leq, 5}, {
    set, "y", {delay, {t, geq, 5}, {
      'if', {"x", leq, 10}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {delay, {t, leq, 5}, {
            'if', {"y", leq, 5}, 'then', {
              p, '<-', {msg_B, undefined}, term
            }, 'else', {
              'if', {"x", leq, 3}, 'then', {
                p, '<-', {msg_C, undefined}, term
              }, 'else', {
                p, '<-', {msg_D, undefined}, term
              }
            }
          }}
        }
      }, 'else', {
        p, '<-', {msg_E, undefined}, term
      }
    }
  }}}};


%%%%%%%%%%%%%%%%%%%%%%
%%% select only (geq)
%%%%%%%%%%%%%%%%%%%%%%
maptest(select_one_from_two_with_one_timer_geq) ->
  {set, "x", {
    delay, {t, geq, 5}, {
      'if', {"x", geq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        p, '<-', {msg_B, undefined}, term
      }
    }
  }};

maptest(select_one_from_three_with_one_timer_geq) ->
  {set, "x", {
    delay, {t, geq, 10}, {
      'if', {"x", geq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        'if', {"x", geq, 10}, 'then', {
          p, '<-', {msg_B, undefined}, term
        }, 'else', {
          p, '<-', {msg_C, undefined}, term
        }
      }
    }
  }};

maptest(select_one_from_four_with_two_timers_geq) ->
  {set, "x", {delay, {t, geq, 5}, {
    set, "y", {delay, {t, geq, 5}, {
      'if', {"x", geq, 10}, 'then', {
        'if', {"y", geq, 5}, 'then', {
          p, '<-', {msg_A, undefined}, term
        }, 'else', {
          p, '<-', {msg_B, undefined}, term
        }
      }, 'else', {
        'if', {"y", geq, 5}, 'then', {
          p, '<-', {msg_C, undefined}, term
        }, 'else', {
          p, '<-', {msg_D, undefined}, term
        }
      }
    }
  }}}};

maptest(select_with_resets_geq) ->
  {set, "x", {delay, {t, geq, 5}, {
    set, "y", {delay, {t, geq, 5}, {
      'if', {"x", geq, 10}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {delay, {t, geq, 5}, {
            'if', {"y", geq, 5}, 'then', {
              p, '<-', {msg_B, undefined}, term
            }, 'else', {
              'if', {"x", geq, 3}, 'then', {
                p, '<-', {msg_C, undefined}, term
              }, 'else', {
                p, '<-', {msg_D, undefined}, term
              }
            }
          }}
        }
      }, 'else', {
        p, '<-', {msg_E, undefined}, term
      }
    }
  }}}};

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cotimeout send only
%%%%%%%%%%%%%%%%%%%%%%%%%
maptest(cotimeout_send_only) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        p, '<-', {msg_C, undefined}, term
      }
    }
  }};

maptest(cotimeout_send_twice) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {
            delay, {t, leq, 5}, {
              'if', {"x", leq, 5}, 'then', {
                p, '<-', {msg_D, undefined}, term
              }, 'else', {
                p, '<-', {msg_F, undefined}, term
              }
            }
          }
        }
      }, 'else', {
        p, '<-', {msg_C, undefined}, term
      }
    }
  }};

%%%%%%%%%%%%%%%%%%%%%%%%
%%% cotimeout recv only
%%%%%%%%%%%%%%%%%%%%%%%%
maptest(cotimeout_recv_only) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        p, '->', {msg_C, undefined}, term
      }
    }
  }};

maptest(cotimeout_recv_twice) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {
            delay, {t, leq, 5}, {
              'if', {"x", leq, 5}, 'then', {
                p, '<-', {msg_D, undefined}, term
              }, 'else', {
                p, '->', {msg_F, undefined}, term
              }
            }
          }
        }
      }, 'else', {
        p, '->', {msg_C, undefined}, term
      }
    }
  }};


%%%%%%%%%%%%%%%%%%%%%%%
%%% cotimeout mix only
%%%%%%%%%%%%%%%%%%%%%%%
maptest(cotimeout_send_recv) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {
            delay, {t, leq, 5}, {
              'if', {"x", leq, 5}, 'then', {
                p, '<-', {msg_D, undefined}, term
              }, 'else', {
                p, '->', {msg_F, undefined}, term
              }
            }
          }
        }
      }, 'else', {
        p, '<-', {msg_C, undefined}, term
      }
    }
  }};

maptest(cotimeout_recv_send) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, {
          set, "x", {
            delay, {t, leq, 5}, {
              'if', {"x", leq, 5}, 'then', {
                p, '<-', {msg_D, undefined}, term
              }, 'else', {
                p, '<-', {msg_F, undefined}, term
              }
            }
          }
        }
      }, 'else', {
        p, '->', {msg_C, undefined}, term
      }
    }
  }};



%%%%%%%%%%%%%%
%%% recursion 
%%%%%%%%%%%%%%
maptest(linear_recursion_send_recv) -> 
  {set, "x", {%% test if "x" is extracted into protocol, since it isnt used
    'def', {
      p, '<-', {msg_A, undefined}, {
        p, '->', {msg_B, undefined}, {
          'call', {"r1", {[{msg_A, undefined}, {msg_B, undefined}], ["x"]}}
        }
      }
    }, 'as', {"r1", {[], ["x"]}}
  }};

maptest(linear_recursion_recv_send) -> 
  {set, "y", {%% test if "y" is extracted into protocol, since it isnt used
    'def', {
      p, '->', {msg_A, undefined}, {
        p, '<-', {msg_B, undefined}, {
          'call', {"r1", {[{msg_A, undefined}, {msg_B, undefined}], []}}
        }
      }
    }, 'as', {"r1", {[], []}}
  }};

%% producer-consumer (producer)
maptest(nonlinear_recursion_send_recv) -> 
  {set, "x", {
    'def', {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, {
          'call', {"r1", {[{msg_A, undefined}], []}}
        }
      }, 'else', {
        p, '->', {msg_B, undefined}, term
      }
    }, 'as', {"r1", {[], []}}
  }};

%% producer-consumer (consumer)
maptest(nonlinear_recursion_recv_send) -> maptest(nonlinear_recursion_recv_send_using_if);

%% producer-consumer (consumer)
maptest(nonlinear_recursion_recv_send_using_if) -> 
  {set, "y", {
    'def', {
      'if', {"y", leq, 5}, 'then', {
        p, '->', {msg_A, undefined}, {
          'call', {"r1", {[{msg_A, undefined}], []}}
        }
      }, 'else', {
        p, '<-', {msg_B, undefined}, term
      }
    }, 'as', {"r1", {[], []}}
  }};


%% producer-consumer (consumer)
maptest(nonlinear_recursion_recv_send_using_branch) -> 
  {'def', {p, '->', {leq, 5}, 
              [ {{msg_A, undefined}, {
                'call', {"r1", [{msg_A, undefined}], []}
                }} ],
              'after', { p, '<-', {msg_B, undefined}, term}
          }, 'as', {"r1", {[], []}}};


%%%%%%%%%%%%%%%
%%% recv after 
%%%%%%%%%%%%%%%
maptest(recv_after_send) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined}, term} ],
      'after', {p, '<-', {msg_B, undefined}, term}};

maptest(recv_after_recv) ->
  {p, '->', {leq, 5}, 
      [ {{msg_A, undefined}, term} ],
      'after', {p, '<-', {msg_B, undefined}, term}};

%%%%%%%%%%%%%%%
%%% send after 
%%%%%%%%%%%%%%%
maptest(send_after_send) -> maptest(cotimeout_send_only);

maptest(send_after_recv) ->
  {set, "x", {
    delay, {t, leq, 5}, {
      'if', {"x", leq, 5}, 'then', {
        p, '<-', {msg_A, undefined}, term
      }, 'else', {
        p, '->', {msg_B, undefined}, term
      }
    }
  }};

%%%%%%%%%%%%%%%
%%% misc tests
%%%%%%%%%%%%%%%
maptest(misc_catch_unset_timers) ->
  {delay, {t, leq, 5}, {
    'if', {"x", leq, 5}, 'then', {
      p, '<-', {msg_A, undefined}, term
    }, 'else', {
      p, '->', {msg_B, undefined}, term
    }
  }};


%%%% catch all
maptest(_) -> term.
