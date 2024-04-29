-module(role_tmp).
-file("role_tmp.erl", 1).

-behaviour(gen_statem).

-include("tpri_data_records.hrl").

%% gen_statem
-export([ start_link/0,
          callback_mode/0,
          init/1,
          stop/0,
          terminate/3 ]).

%% custom wrappers for gen_statem
-export([ start_link/1 ]).

%% callbacks
-export([ handle_event/4 ]).

%% better printing
% -export([ format_status/1 ]).

%% generic callbacks
-export([ send/2, recv/1 ]).

-include("printout.hrl").
-include("role_mon_show.hrl").
-include("list_to_map_builder.hrl").
-include("list_to_map_getter.hrl").
-include("list_to_map_setter.hrl").
-include("role_mon_data.hrl").
-include("role_mon_options.hrl").

-define(MSG_AGE_LIMIT, 4).


start_link(List) when is_list(List) -> 
  %% get data-map from list
  Data = data(List),
  show({"~p.", [?FUNCTION_NAME], Data}),

  #{name := Name} = Data,
  #{role := Role} = Data,
  show({Name, "~p, role name: ~p.", [?FUNCTION_NAME, Role], Data}),

  RegID = maps:get(reg_id,Data,err_no_role_id),
  show({Name, "~p, reg_id: ~p.", [?FUNCTION_NAME, RegID], Data}),

  case RegID of
    err_no_role_id -> 
      show({Name, "~p, ~p, registering locally.", [?FUNCTION_NAME, RegID], Data}),
      {ok,PID} = gen_statem:start_link({local, ?MODULE}, ?MODULE, [Data], []);
    _ ->
      show({Name, "~p, registering globally as: ~p.", [?FUNCTION_NAME, Name], Data}),
      {ok,PID} = gen_statem:start_link({global, Name}, ?MODULE, [Data], [])
  end,
  show({Name, "leaving ~p as ~p.", [?FUNCTION_NAME, PID], Data}),
  {ok, PID}.




start_link() -> 
  printout("~p.", [self()]),
  ?MODULE:start_link([]).




callback_mode() -> [handle_event_function, state_enter].




init([Data]) when is_map(Data) -> 
  %% get name and role from data
  #{name := Name} = Data,
  #{role := Role} = Data,
  show({Name, "~p, finished process params.", [?FUNCTION_NAME], Data}),
  show({Name, "~p, Data: ~p.", [?FUNCTION_NAME, Data], Data}),

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {role, Role, mon, self()},
  show({Name, "~p, registered with app.", [?FUNCTION_NAME], Data}),

  {ok, init_setup_state, Data}.




stop() -> 
  printout("~p.", [?FUNCTION_NAME]),
  gen_statem:stop(?MODULE).




terminate(Reason, issue_timeout=_State, #{name:=Name}=Data) ->
  show({Name, "~p, (timeout)...\n\treason: ~p,\n\tdata: ~p.", [?FUNCTION_NAME, Reason, Data], Data});

terminate(Reason, State, #{name:=Name}=Data) ->
  show({Name, "~p, (~p)...\n\treason: ~p,\n\tdata: ~p.", [?FUNCTION_NAME, State, Reason, Data], Data}).




%% generic callbacks
send(Label, Msg) -> gen_statem:cast(?MODULE, {send, Label, Msg}).
recv(Label) -> gen_statem:call(?MODULE, {recv, Label}).






%% % % % % % %
%% handling instructions from user/implementation/controller
%% % % % % % %
handle_event(info, {act, send, Label, Msg, Meta}, _State, #{name:=Name}=Data) ->
  io:format("\n"),
  show({Name, "~p, {act, send, ~p, ~p, ~p}...", [?FUNCTION_NAME, Label, Msg, Meta], Data}),
  {keep_state_and_data, [{next_event, cast, {send, Label, Msg, Meta}}]};

handle_event(info, {act, recv, Label}, _State, #{name:=Name}=Data) ->
  io:format("\n"),
  show({Name, "~p, {recv, ~p},\n\n\tThis is a mistake! To retrieve messages use:\n\t\t\"gen_statem(?THIS_PID, {recv, ~p}).\"", [?FUNCTION_NAME, Label, Label], Data}),
  {keep_state_and_data, [{next_event, cast, {recv, Label}}]};






%% % % % % % %
%% custom init/stop wrappers
%% % % % % % %

%% catch before start (to receive the CoPartyID)
handle_event(enter, _OldState, init_setup_state=State, #{name:=Name,coparty_id:=undefined}=Data) -> 
  show({Name, "(->) ~p.", [State], Data}),
  {keep_state, Data, [{state_timeout, 0, wait_to_finish}]};

handle_event(state_timeout, wait_to_finish, init_setup_state=State, 
#{name:=Name,coparty_id:=undefined,fsm:=#{init:=Init},options:=Options}=Data) ->
  show({Name, "~p, waiting to finish setup.", [State], Data}),
  receive
    {_SupID, sup_init, CoPartyID} ->
      show({Name, "~p, received coparty ID (~p).", [State,CoPartyID], Data}),
      show(verbose, {Name, "~p, options: ~p.", [State,Options], Data}),
      Data1 = Data#{coparty_id => CoPartyID, trace => [Init]},
      show(verbose, {Name, "leaving ~p, entering ~p.", [State,Init], Data1}),
      {next_state, Init, Data1}
  end;

%% catch before stop
handle_event(enter, _OldState, stop_state=State, #{data:=#{name:=Name}=Data}=StopData) -> 
  show({Name, "(->) ~p.", [State], Data}),
  {keep_state, StopData, [{state_timeout, 0, exit_deferral}]};

handle_event(state_timeout, exit_deferral, stop_state=State, #{reason:=Reason,data:=#{name:=Name}=Data}=_StopData) -> 
  show({Name, "~p, ~p, Data: \n\t~p.", [State, Reason, Data], Data}),
  {stop, Reason, Data};






%% % % % % % %
%% states enter
%% % % % % % %

%% state enter, issue timeout (stop and cause supervisor to notice)
handle_event(enter, _OldState, issue_timeout=State, #{name:=Name}=Data) ->
  show({Name, "(->) ~p.", [State], Data}),
  StopData = stop_data([{reason, error_exceeded_throttling_capacity}, {data, Data}]),
  {keep_state, StopData, [{state_timeout, 0, goto_stop}]};

handle_event(state_timeout, goto_stop, issue_timeout=_State, Data) ->
  {next_state, stop_state, Data};




%% state enter, mixed choice (and no queued actions)
handle_event(enter, _OldState, State, #{name:=Name,fsm:=#{timeouts:=Timeouts,map:=Map},queue:=#{on:=[],off:=[]}}=Data)
when is_map_key(State, Timeouts) ->
  %% get timeout
  {TimeoutDuration, TimeoutState} = maps:get(State, Timeouts),
  %% get current actions
  Actions = maps:get(State, Map, none),
  %% display available actions if verbose
  #{options:=#{printout:=#{verbose:=Verbose}}} = Data,
  case Verbose of 
    true -> 
      %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
      Action = lists:nth(1, maps:keys(Actions)),
      %% get outgoing edges
      #{Action:=Edges} = Actions,
      EdgeStr = lists:foldl(fun({Label, Succ}, Acc) -> Str = io_lib:format("\n\t~p: <~p> -> ~p",[Action, Label, Succ]), Acc ++ [Str] end, [], maps:to_list(Edges)),
      show(verbose, {Name, "(->) ~p [t:~p], actions:~s.", [State, TimeoutDuration, EdgeStr], Data});
    _ ->
      show({Name, "(->) ~p [t:~p].", [State, TimeoutDuration], Data})
  end,
  %% if no actions, this is unusual (and likely unintended)
  case Actions==none of
    true -> 
      show({Name, "~p, unusual, no actions from this state:\n\t~p.", [State, Map], Data});
    _ -> ok
  end,
  {keep_state, Data, [{state_timeout, TimeoutDuration, TimeoutState}]};




%% state enter, no queued actions and no mixed-choice
handle_event(enter, _OldState, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{on:=[],off:=[]}}=Data) ->
  %% get current actions
  Actions = maps:get(State, Map, none),
  %% display available actions if verbose
  #{options:=#{printout:=#{verbose:=Verbose}}} = Data,
  case Verbose of 
    true -> 
      %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
      Action = lists:nth(1, maps:keys(Actions)),
      %% get outgoing edges
      #{Action:=Edges} = Actions,
      EdgeStr = lists:foldl(fun({Label, Succ}, Acc) -> Str = io_lib:format("\n\t~p: <~p> -> ~p",[Action, Label, Succ]), Acc ++ [Str] end, [], maps:to_list(Edges)),
      show(verbose, {Name, "(->) ~p, actions:~s.", [State, EdgeStr], Data});
    _ ->
      show({Name, "(->) ~p.", [State], Data})
  end,
  %% if no actions, this is unusual (and likely unintended)
  case Actions==none of
    true -> 
      show({Name, "~p, unusual, no actions from this state: ~p.", [State, Map], Data});
    _ -> ok
  end,
  keep_state_and_data;




%% state enter, begin handling queued actions
handle_event(enter, _OldState, State, #{name:=Name,queue:=#{on:=[],off:=Off}}=Data) when is_list(Off) ->
  show({Name, "(->) ~p, with (~p) queued actions.", [State, length(Off)], Data}),
  {keep_state_and_data, [{state_timeout,0,begin_process_queue}]};

handle_event(state_timeout, begin_process_queue, _State, #{queue:=#{on:=[],off:=Off}}=Data) ->
  %% move off-queue to (active) on-queue
  Data1 = Data#{queue=>#{on=>Off,off=>[]}},
  {repeat_state, Data1};




%% state enter, continue handling queued actions (list Off may be non-empty, containing those already tried this state)
handle_event(enter, _OldState, State, #{name:=Name,queue:=#{on:=On}}=Data) when is_list(On) ->
  show({Name, "(->) ~p (handling queue).", [State], Data}),
  {keep_state_and_data, [{state_timeout,0,continue_process_queue}]};

handle_event(state_timeout, continue_process_queue, State, 
#{ name:=Name, 
   queue:=#{on:=[#{label:=Label,payload:=Payload,aging:=#{enabled:=LocalAgingEnabled,age:=Age}=Aging}=H|T]},
   options:=#{queue:=#{enabled:=true,aging:=#{enabled:=GlobalAgingEnabled,max_age:=MaxAge}}} }=Data) ->
  show({Name, "(->) ~p, queued action: ~p.", [State, H]}),
  %% update next data
  Data1 = Data#{queue=>#{on=>T}},
  Aging1 = Aging#{age=>Age+1},
  %% check if aging enabled
  case MaxAge of
    -1 -> %% overrides both global and local aging, disabling them both
      {repeat_state, Data1, [{next_event, cast, {send, Label, Payload, Aging1}}]};
    _ -> %% need to check if message is aged and valid to send
      case GlobalAgingEnabled or LocalAgingEnabled of
        true -> %% aging applies to this action
          case Age>=MaxAge of
            true -> %% message is young enough to try again
              {repeat_state, Data1, [{next_event, cast, {send, Label, Payload, Aging1}}]};
            _ -> %% message is too old to try again
              {repeat_state, Data1}
          end;
        _ -> %% message cannot be aged
          {repeat_state, Data1, [{next_event, cast, {send, Label, Payload, Aging1}}]}
      end
  end;






%% % % % % % %
%% sending actions
%% % % % % % %

%% from correct states
handle_event(cast, {send, Label, Payload, Meta}, State, #{name:=Name,coparty_id:=CoPartyID,fsm:=#{map:=Map},trace:=Trace}=Data) 
when is_map_key(State, Map) and is_atom(map_get(Label, map_get(send, map_get(State, Map)))) and is_list(Meta) ->
  %% send message to co-party
  CoPartyID ! {self(), Label, Payload},
  %% get next state
  #{State:=#{send:=#{Label:=NextState}}} = Map,
  % NextState = maps:get(Label, maps:get(send, maps:get(State, StateMap))),
  %% update trace
  Data1 = Data#{trace=>[NextState] ++ Trace},
  show({Name, "~p, send (~p: ~p).", [State, Label, Payload], Data1}),
  {next_state, NextState, Data1};

%% from wrong states, and queue enabled 
handle_event(cast, {send, Label, Payload, Meta}, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{on:=On,off:=Off},options:=#{queue:=#{enabled:=true}}}=Data) 
when is_map_key(State, Map) and is_list(Meta) ->
  %% make sure 
  Action = new_queued_action(Label, Payload, Meta),
  %% check if this message is flagged to be queued (default: true)
  #{meta:=#{queue:=#{enabled:=ActionQueueEnabled}}} = Action,
  case ActionQueueEnabled of
    true -> %% this message is flagged to be queued 
      Off1 = Off ++ [Action];
    _ -> %% this message has been specifically flagged to not be queued
      Off1 = Off,
      %% verbose explanation (?)
      #{options:=#{printout:=#{verbose:=Verbose}}} = Data,
      case Verbose of 
        true -> 
          %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
          show(verbose, {Name, "~p, wrong state to send (~p: ~p),\n\tand global-queue option set to true,\n\tbut -- message explicitly flagged to not be queue-able:\n\t~p.", [State, Label, Payload, Action], Data});
        _ ->
          show({Name, "~p, unusual, cannot add to queue: ~p.", [State, {Label, Payload}], Data})
      end
  end,
  %% next data has updated off-queue 
  Data1 = Data#{queue:=#{on=>On,off=>Off1}},
  {keep_state, Data1};

%% from wrong states, and queue explicitly enabled 
handle_event(cast, {send, Label, Payload, #{queue:=true}=Meta}, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{off:=Off}=Queue,options:=#{queue:=#{enabled:=false}}}=Data) 
when is_map_key(State, Map) and is_list(Meta) ->
  %% assimulate into map-format with full meta
  Action = new_queued_action(Label, Payload, Meta),
  %% verbose explanation (?)
  #{options:=#{printout:=#{verbose:=Verbose}}} = Data,
  case Verbose of 
    true -> 
      %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
      show(verbose, {Name, "~p, wrong state to send (~p: ~p),\n\tand global-queue option set to false,\n\tbut -- message flagged to be queue-able:\n\t~p.", [State, Label, Payload, Action], Data});
    _ ->
      show({Name, "~p, added to queue: ~p.", [State, {Label, Payload}], Data})
  end,
  %% add to queue
  Off1 = Off ++ [Action],
  %% next data has updated off-queue 
  Data1 = Data#{queue=>Queue#{off=>Off1}},
  {keep_state, Data1};






%% % % % % % %
%% receiving actions, 
%% % % % % % %

%% from correct states
handle_event(info, {CoPartyID, Label, Payload}, State, #{name:=Name,coparty_id:=CoPartyID,fsm:=#{map:=Map},msgs:=Msgs,queue:=Queue,options:=#{forward_receptions:=#{enabled:=ForwardingEnabled,to:=ForwardTo,any:=ForwardAny,labels:=ForwardLabels},queue:=#{flush_after_recv:=#{enabled:=FlushingEnabled,after_any:=FlushAfterAny,after_labels:=FlushAfterLabels}}}}=Data) 
when is_map_key(State, Map) and is_atom(map_get(Label, map_get(recv, map_get(State, Map)))) ->  
  %% forward if necessary
  case ForwardingEnabled of
    true ->
      case ForwardAny or lists:member(Label, ForwardLabels) of
        true ->
          show(verbose, {Name, "~p, forwarding msg to ~p.", [State, ForwardTo], Data}),
          ForwardTo ! {self(), Label, Payload};
        _ -> ok
      end;
    _ -> ok
  end,
  %% flush queue if necessasry
  case FlushingEnabled of
    true -> %% check when to flush
      case FlushAfterAny or lists:member(Label, FlushAfterLabels) of
        true -> 
          show(verbose, {Name, "~p, flushing queue after receiving.", [State], Data}),
          Queue1 = Queue#{on=>[],off=>[]};
        _ -> Queue1 = Queue
      end; 
    _ -> Queue1 = Queue
  end,
  %% add to list of receives to check next time we enter state
  #{check_recvs:=CheckRecvs} = Queue1,
  Queue2 = Queue#{check_recvs=>CheckRecvs ++ [Label]},
  %% add to front of list of messages received under this label
  Payloads = maps:get(Label, Msgs, []),
  Payloads1 = [Payload] ++ Payloads,
  Msgs1 = Msgs#{Label => Payloads1},
  %% get next state
  #{State:=#{recv:=#{Label:=NextState}}} = Map,
  %% update data
  Data1 = Data#{msgs=>Msgs1,queue=>Queue2},
  show({Name, "~p, recv (~p: ~p) -> ~p.", [State, Label, Payload, NextState], Data1}),
  {next_state, NextState, Data1};

%% from wrong states 
handle_event(info, {CoPartyID, Label, Payload}, State, #{name:=Name,coparty_id:=CoPartyID}=Data) ->
  show({Name, "~p, wrong state to recv (~p: ~p), postponing.", [State, Label, Payload], Data}),
  {keep_state_and_data, [postpone]};






%% % % % % % %
%% mixed-choice (timeouts)
%% % % % % % %
handle_event(state_timeout, NextState, State, #{name:=Name}=Data) ->
  show({Name, "~p, internal timeout (~p).", [State, NextState], Data}),
  {next_state, NextState, Data};






%% % % % % % %
%% retreive latest message of given label
%% % % % % % %
handle_event({call, From}, {recv, Label}, State, #{name:=Name,msgs:=Msgs}=Data) -> 
  show({Name, "~p, looking for msg with label (~p).", [State, Label], Data}),
  case maps:get(Label, Msgs, no_msg_found_under_label) of
    no_msg_found_under_label=Err -> 
      show({"~p, no msgs with label (~p) found.", [State, Label], Data}),
      ReplyMsg = {error, Err};
    Matches -> 
      NumMsgs = lists:length(Matches),
      H = lists:nth(1, Matches),
      show({"~p, found msg (~p: ~p) out of ~p.", [State, Label, H, NumMsgs], Data}),
      ReplyMsg = {ok, #{  label => Label, matches => Matches }}
  end,
  {keep_state_and_data, [{reply, From, ReplyMsg}]};






%% % % % % % %
%% handle external requests to 
%% % % % % % %

%% change options (using list-depth-key val)
handle_event({call, From}, {options, [H|T]=Keys, Val}, State, #{name:=Name,options:=Options}=Data) 
when is_list(Keys) ->    
  show(verbose, {Name, "~p, changing option: ~p => ~p.", [State, Keys, Val], Data}),
  Options1 = list_to_map_setter(H, T, Options),
  Data1 = Data#{options=>Options1},
  {keep_state, Data1, [{reply, From, ok}]};

%% change options (single surface level key-val)
handle_event({call, From}, {options, Key, Val}, State, #{name:=Name,options:=Options}=Data) 
when is_map_key(Key, Options) ->    
  show(verbose, {Name, "~p, changing option: ~p => ~p.", [State, Key, Val], Data}),
  Options1 = maps:put(Key, Val, Options),
  Data1 = Data#{options=>Options1},
  {keep_state, Data1, [{reply, From, ok}]};
    
%% request copy of options
handle_event({call, From}, get_options, State, #{name:=Name,options:=Options}=Data) ->    
  show(verbose, {Name, "~p, sharing options with ~p,\n\tOptions: ~p.", [State, From, Options], Data}),
  {keep_state, Data, [{reply, From, {ok, Options}}]};

%% request fsm terminate
handle_event({call, _From}, {terminate, Reason}, State, #{name:=Name}=Data) ->
  show({Name, "~p, termination, reason: ~p.", [State, Reason], Data}),
  Data1 = stop_data([{reason = Reason}, {data = Data}]), 
  {next_state, stop_state, Data1};






%% % % % % % %
%% anything else
%% % % % % % %
handle_event(EventType, EventContent, State, #{name:=Name}=Data) ->
  show({Name, "~p, error, reached unhandled event...\n\tEventType: ~p,\n\tEventContent: ~p,\n\tData: ~p.", [State, EventType, EventContent, Data], Data}),
  Data1 = stop_data([{reason, unhandled_event}, {data, Data}]), 
  {next_state, stop_state, Data1}.

