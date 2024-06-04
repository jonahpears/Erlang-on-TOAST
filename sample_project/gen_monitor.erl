-module(gen_monitor).
-file("gen_monitor.erl", 1).

-behaviour(gen_statem).

-include("toast_data_records.hrl").

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
-include("nested_map_merger.hrl").
-include("list_to_map_builder.hrl").
-include("list_to_map_getter.hrl").
-include("list_to_map_setter.hrl").
-include("role_mon_data.hrl").
-include("role_mon_options.hrl").



start_link(List) when is_list(List) -> 
  %% get data-map from list
  Data = data(List),
  show({"~p.", [?FUNCTION_NAME], Data}),

  #{name := Name} = Data,
  #{role := Role} = Data,
  show({Name, "~p, role:\n\t~p.", [?FUNCTION_NAME, Role], Data}),

  RegID = maps:get(reg_id,Data,err_no_role_id),
  show({Name, "~p,\n\treg_id: ~p.", [?FUNCTION_NAME, RegID], Data}),

  case RegID of
    err_no_role_id -> 
      show({Name, "~p,\n\t~p, registering locally.", [?FUNCTION_NAME, RegID], Data}),
      {ok,PID} = gen_statem:start_link({local, ?MODULE}, ?MODULE, [Data], []);
    _ ->
      show({Name, "~p,\n\tregistering globally as: ~p.", [?FUNCTION_NAME, Name], Data}),
      {ok,PID} = gen_statem:start_link({global, Name}, ?MODULE, [Data], [])
  end,
  show({Name, "leaving ~p as ~p.", [?FUNCTION_NAME, PID], Data}),
  {ok, PID}.




start_link() -> 
  printout("~p.", [self()]),
  start_link([]).




callback_mode() -> [handle_event_function, state_enter].




init([Data]) when is_map(Data) -> 
  %% get name and role from data
  #{name := Name} = Data,
  #{role := Role} = Data,
  show({Name, "~p,\n\tfinished process params.", [?FUNCTION_NAME], Data}),
  show({Name, "~p,\n\tdata: ~p.", [?FUNCTION_NAME, Data], Data}),

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(toast,app_id),
  AppID ! {role, Role, mon, self()},
  show({Name, "~p,\n\tregistered with app.", [?FUNCTION_NAME], Data}),

  {ok, init_setup_state, Data}.




stop() -> 
  printout("~p.", [?FUNCTION_NAME]),
  gen_statem:stop(?MODULE).




terminate(Reason, issue_timeout=_State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\t(timeout)...\n\treason: ~p,\n\tdata: ~p.", [?FUNCTION_NAME, Reason, Data], Data});

terminate(Reason, State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\t(~p)...\n\treason: ~p,\n\tdata: ~p.", [?FUNCTION_NAME, State, Reason, Data], Data}).




%% generic callbacks
send(Label, Msg) -> gen_statem:cast(?MODULE, {send, Label, Msg}).
recv(Label) -> gen_statem:call(?MODULE, {recv, Label}).


%% receive real ID of system being monitored
handle_event({call,From},{StartID,setup,SusID},_State,#{start_id:=StartID}=Data) ->
  printout("~p,\n\tstart_id: ~p,\n\tsus_id: ~p.",[?FUNCTION_NAME,StartID,SusID]),
  Data1 = maps:put(sus_id,SusID,Data),
  {keep_state,Data1,[{reply,From,ok}]};




%% % % % % % %
%% handling instructions from user/implementation/controller
%% % % % % % %

%% normal sending actions (when in a proper state)
handle_event(info, {act, send, Label, Payload, Meta}, State, #{name:=Name,fsm:=#{map:=Map}}=Data) 
when is_map_key(State, Map) ->
  show({Name, "~p,\n\tinstruction, send:\n\t{~p, ~p, ~p}.", [State, Label, Payload, Meta], Data}),
  Meta1 = maps:from_list(Meta),
  %% make sure special "from_queue" is not inserted
  Meta2 = maps:remove(from_queue, Meta1),
  {keep_state_and_data, [{next_event, cast, {send, Label, Payload, Meta2}}]};

%% normal sending actions (but caught when other actions are being processed) -- postpone
handle_event(info, {act, send, _Label, _Payload, _Meta}, State, #{name:=Name}=Data) ->
  show(verbose, {Name, "~p,\n\tpostponing send instruction...", [State], Data}),
  {keep_state_and_data, [postpone]};


%% mistakingly try to request message received under label
handle_event(info, {act, recv, Label}, State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\t{recv, ~p},\n\n\tThis is a mistake! To retrieve messages use:\n\t\t\"gen_statem:call(?THIS_PID, {recv, ~p}).\"", [State, Label, Label], Data}),
  keep_state_and_data;






%% % % % % % %
%% custom init/stop wrappers
%% % % % % % %

%% catch before start (to receive the CoPartyID)
handle_event(enter, _OldState, init_setup_state=State, #{name:=Name,coparty_id:=undefined}=Data) -> 
  Data1 = maps:put(state_to_return_to, undefined, Data),
  show({Name, "(->) ~p.", [State], Data1}),
  {keep_state, Data1, [{state_timeout, 0, wait_to_finish}]};

handle_event(state_timeout, wait_to_finish, init_setup_state=State, 
#{name:=Name,coparty_id:=undefined,fsm:=#{init:=Init},options:=Options}=Data) ->
  show({Name, "~p,\n\twaiting to finish setup.", [State], Data}),
  receive
    {_SupID, sup_init, CoPartyID} ->
      show({Name, "~p,\n\treceived coparty ID (~p).", [State,CoPartyID], Data}),
      show(verbose, {Name, "~p,\n\toptions:\n\t~p.", [State,Options], Data}),
      Data1 = Data#{coparty_id => CoPartyID, trace => [Init]},
      show(verbose, {Name, "leaving ~p, entering ~p.", [State,Init], Data1}),
      {next_state, Init, Data1}
  end;

%% catch before stop
handle_event(enter, _OldState, stop_state=State, #{data:=#{name:=Name}=Data}=StopData) -> 
  show({Name, "(->) ~p.", [State], Data}),
  {keep_state, StopData, [{state_timeout, 0, exit_deferral}]};

handle_event(state_timeout, exit_deferral, stop_state=_State, #{reason:=Reason,data:=#{name:=_Name}=Data}=_StopData) -> 
  % show({Name, "~p,\n\t~p, Data: \n\t~p.", [State, Reason, Data], Data}),
  {stop, Reason, Data};






%% % % % % % %
%% states enter
%% % % % % % %

%% state enter, issue timeout (stop and cause supervisor to notice)
handle_event(enter, _OldState, issue_timeout=State, #{name:=Name}=Data) ->
  show({Name, "(->) ~p.", [State], Data}),
  StopData = stop_data([{reason, protocol_issued_timeout}, {data, Data}]),
  {keep_state, StopData, [{state_timeout, 0, goto_stop}]};

handle_event(state_timeout, goto_stop, issue_timeout=_State, Data) ->
  {next_state, stop_state, Data};




%% state enter, mixed choice (and no queued actions)
handle_event(enter, _OldState, State, #{name:=Name,fsm:=#{timeouts:=Timeouts,map:=Map},queue:=#{state_to_return_to:=undefined}}=Data)
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
      show({Name, "~p,\n\tunusual, no actions from this state:\n\t~p.", [State, Map], Data});
    _ -> ok
  end,
  {keep_state, Data, [{state_timeout, TimeoutDuration, TimeoutState}]};




%% state enter, no queued actions and no mixed-choice
handle_event(enter, _OldState, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{state_to_return_to:=undefined}=Queue}=Data) ->
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
      show({Name, "~p,\n\tunusual, no actions from this state: ~p.", [State, Map], Data});
    _ -> ok
  end,
  %% since any queued actions have been processed, clear 'check_recvs'
  Queue1 = Queue#{check_recvs => []},
  Data1 = Data#{queue => Queue1},
  {keep_state, Data1};




%% state enter, begin handling queued actions
handle_event(enter, _OldState, State, #{name:=Name,queue:=#{on:=[],off:=Off,state_to_return_to:=undefined}}=Data) when is_list(Off) ->
  show({Name, "(->) ~p, with (~p) queued actions.", [State, length(Off)], Data}),
  {keep_state_and_data, [{state_timeout,0,begin_process_queue}]};

handle_event(state_timeout, begin_process_queue, State, #{name:=Name,queue:=#{on:=[],off:=Off,state_to_return_to:=undefined}=Queue}=Data) ->
  show(verbose, {Name, "(->) ~p, (state_timeout).", [State], Data}),
  %% move off-queue to (active) on-queue
  Queue1 = Queue#{on=>Off,off=>[],state_to_return_to=>State},
  Data1 = Data#{queue=>Queue1},
  {repeat_state, Data1};




%% state enter, continue handling queued actions (list Off may be non-empty, containing those already tried this state)
handle_event(enter, _OldState, State, #{name:=Name,queue:=#{on:=On,state_to_return_to:=StateToReturnTo}}=Data) when is_list(On) and StateToReturnTo=/=undefined ->
  show( verbose, {"(->) ~p (handling queue),\n\tdata: ~p.", [State,Data]},
        else, {"(->) ~p (handling queue).", [State]}, {Name, Data}),
  {keep_state_and_data, [{state_timeout,0,continue_process_queue}]};

handle_event(state_timeout, continue_process_queue, State, 
#{ name:=Name, 
   queue:=#{on:=[#{label:=Label,payload:=Payload,meta:=#{aging:=#{enabled:=LocalAgingEnabled,age:=Age}=Aging,drop:=#{after_recv:=DropAfterRecv,after_labels:=DropAfterLabels}}=Meta}=H|T], check_recvs:=CheckRecvs, state_to_return_to:=StateToReturnTo}=Queue,
   options:=#{queue:=#{enabled:=true,aging:=#{enabled:=GlobalAgingEnabled,max_age:=MaxAge}}}}=Data) when is_map(Meta) ->
  show({Name, "(->) ~p, queued action: ~p.", [State, H]}),
  %% update next data
  Queue1 = Queue#{on=>T},
  Data1 = Data#{queue=>Queue1},
  %% check if this should be dropped due to contents of check_recvs
  DoNotDrop = lists:foldl(fun(Elem, Acc) -> Acc and lists:member(Elem, CheckRecvs) end, true, DropAfterLabels),
  case DoNotDrop or DropAfterRecv of
    true ->
      Aging1 = Aging#{age=>Age+1},
      Meta1 = maps:put(aging, Aging1, Meta),
      Meta2 = maps:put(from_queue, true, Meta1),
      %% check if aging enabled
      case MaxAge of
        -1 -> %% overrides both global and local aging, disabling them both
          {next_state, StateToReturnTo, Data1, [{next_event, cast, {send, Label, Payload, Meta2}}]};
        _ -> %% need to check if message is aged and valid to send
          case GlobalAgingEnabled or LocalAgingEnabled of
            true -> %% aging applies to this action
              case Age>=MaxAge of
                true -> %% message is young enough to try again
                  {next_state, StateToReturnTo, Data1, [{next_event, cast, {send, Label, Payload, Meta2}}]};
                _ -> %% message is too old to try again
                  {next_state, StateToReturnTo, Data1}
              end;
            _ -> %% message cannot be aged
              {next_state, StateToReturnTo, Data1, [{next_event, cast, {send, Label, Payload, Meta2}}]}
          end
      end;
    _ -> %% this queued action has flagged itself to be dropped after a recent receive
      show( verbose, {"~p, dropped queued action (~p: ~p) since:\n\tdrop_after_recv: ~p,\n\tdrop_after_labels: ~p,\n\tcheck_recvs: ~p.", [State, Label, Payload, DropAfterRecv, DropAfterLabels, CheckRecvs]},
            else, {"~p, dropped queued action (~p: ~p).", [State, Label, Payload]}, 
            {Name, Data1} ),
      {next_state, StateToReturnTo, Data1}
  end;






%% % % % % % %
%% sending actions
%% % % % % % %

%% auto-label -- try and find label and then send. if no label found, add to queue
handle_event(cast, {send, dont_care=Label, Payload, Meta}, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{off:=Off}=Queue,options:=#{queue:=#{enabled:=QueueEnabled}}}=Data) 
when is_map_key(State, Map) and is_map(Meta) and is_map_key(auto_label, Meta) and map_get(enabled, map_get(auto_label, Meta))
 ->
  show(verbose, {Name, "~p,\n\tattempting to auto-label.", [State], Data}),
  #{State:=Directions} = Map,
  case lists:member(send,maps:keys(Directions)) of
    true -> %% this should correspond, therefore use this label
      %% get label from current available sending actions, which we assume to be one
      #{send:=Actions} = Directions,
      Label1 = lists:nth(1, maps:keys(Actions)),
      % io:format("\n"),
      show({Name, "~p,\n\tauto-labelled:\n\t(~p, ~p), ~p.", [State, Label1, Payload, Meta], Data}),
      {keep_state_and_data, [{next_event, cast, {send, Label1, Payload, Meta}}]};
    _ -> %% then this is a recv? add to queue
      show(verbose, {Name, "~p,\n\tcannot auto-label, adding to queue.", [State], Data}),
      %% if no queue in meta, use global option
      case lists:member(queue, maps:keys(Meta)) of
        true -> Meta1 = Meta;
        _ -> Meta1 = maps:put(queue, #{enabled=>QueueEnabled}, Meta)
      end,
      Action = new_queued_action(Label, Payload, maps:to_list(Meta1)),
      %% check if this message is flagged to be queued (default: true)
      #{meta:=#{queue:=#{enabled:=ActionQueueEnabled}}} = Action,
      case ActionQueueEnabled of
        true -> %% this message is flagged to be queued 
          Off1 = Off ++ [Action];
        _ -> %% this message has been specifically flagged to not be queued
          Off1 = Off,
          %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
          show( verbose, {"~p, wrong state to send (~p: ~p),\n\tand global-queue option set to true,\n\tbut -- message explicitly flagged to not be queue-able:\n\t~p.", [State, Label, Payload, Action]},
                else, {"~p, unusual, cannot add to queue: ~p.", [State, {Label, Payload}]}, {Name, Data})
      end,
      Queue1 = Queue#{off=>Off1},
      Data1 = Data#{queue=>Queue1},
      show(verbose, {Name, "~p,\n\tsuccessfully added to queue:\n\t~p.", [State,maps:get(queue,Data1)], Data}),
      {keep_state, Data1}
  end;

%% from correct states
handle_event(cast, {send, Label, Payload, Meta}, State, #{name:=Name,coparty_id:=CoPartyID,fsm:=#{map:=Map},trace:=Trace}=Data) 
when is_map_key(State, Map) and is_atom(map_get(Label, map_get(send, map_get(State, Map)))) and is_map(Meta) ->
  %% send message to co-party
  CoPartyID ! {self(), Label, Payload},
  %% get next state
  #{State:=#{send:=#{Label:=NextState}}} = Map,
  %% update trace
  Trace1 = [NextState] ++ Trace,
  Data1 = maps:put(trace,Trace1,Data),
  show({Name, "~p,\n\tsend (~p: ~p),\n\ttrace: ~p.", [State, Label, Payload, Trace1], Data1}),
  {next_state, NextState, Data1};

%% from wrong states, and queue enabled 
handle_event(cast, {send, Label, Payload, Meta}, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{off:=Off}=Queue,options:=#{queue:=#{enabled:=true=QueueEnabled}}}=Data) 
when is_map_key(State, Map) and is_map(Meta) ->
  %% if no queue in meta, use global option
  case lists:member(queue, maps:keys(Meta)) of
    true -> Meta1 = Meta;
    _ -> Meta1 = maps:put(queue, #{enabled=>QueueEnabled}, Meta)
  end,
  Action = new_queued_action(Label, Payload, maps:to_list(Meta1)),
  %% check if this message is flagged to be queued (default: true)
  #{meta:=#{queue:=#{enabled:=ActionQueueEnabled}}} = Action,
  case ActionQueueEnabled of
    true -> %% this message is flagged to be queued 
      show(verbose, {Name, "~p,\n\twrong state to send (~p: ~p),\n\tand global-queue option set to true,\n\tand message can be queued,\n\taction: ~p.", [State, Label, Payload, Action], Data}),
      Off1 = Off ++ [Action];
    _ -> %% this message has been specifically flagged to not be queued
      Off1 = Off,
      %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
      show( verbose, {"~p, wrong state to send (~p: ~p),\n\tand global-queue option set to true,\n\tbut -- message explicitly flagged to not be queue-able:\n\t~p.", [State, Label, Payload, Action]},
            else, {"~p, unusual, cannot add to queue: ~p.", [State, {Label, Payload}]}, {Name, Data})
  end,
  %% next data has updated off-queue 
  Queue1 = Queue#{off=>Off1},
  Data1 = Data#{queue=>Queue1},
  %% check if meta says from_queue
  FromQueue = lists:member(from_queue, maps:keys(Meta)),
  case FromQueue of true -> IsFromQueue = maps:get(from_queue, Meta); _ -> IsFromQueue = false end,
  case IsFromQueue of
    true -> %% then state allowed to repeat
      {repeat_state, Data1};
    _ ->
      {keep_state, Data1}
  end;

%% from wrong states, and queue explicitly enabled (although global queue is off)
handle_event(cast, {send, Label, Payload, #{queue:=true}=Meta}, State, #{name:=Name,fsm:=#{map:=Map},queue:=#{off:=Off}=Queue,options:=#{queue:=#{enabled:=false}}}=Data) 
when is_map_key(State, Map) and is_map(Meta) ->
  %% assimulate into map-format with full meta
  Action = new_queued_action(Label, Payload, Meta),
  %% assuming mixed-states are modelled as two (with silent/timeout edge between them)
  show( verbose, {"~p, wrong state to send (~p: ~p),\n\tand global-queue option set to false,\n\tbut -- message flagged to be queue-able:\n\t~p.", [State, Label, Payload, Action]}, 
        else, {"~p, added to queue: ~p.", [State, {Label, Payload}]}, {Name, Data}),
  %% add to queue
  Off1 = Off ++ [Action],
  %% next data has updated off-queue 
  Queue1 = Queue#{off=>Off1},
  Data1 = Data#{queue=>Queue1},
  {keep_state, Data1};






%% % % % % % %
%% receiving actions, 
%% % % % % % %

%% from correct states
handle_event(info, {CoPartyID, Label, Payload}, State, #{name:=Name,coparty_id:=CoPartyID,fsm:=#{map:=Map},msgs:=Msgs,trace:=Trace,queue:=Queue,options:=#{forward_receptions:=#{enabled:=ForwardingEnabled,to:=ForwardTo,any:=ForwardAny,labels:=ForwardLabels},queue:=#{flush_after_recv:=#{enabled:=FlushingEnabled,after_any:=FlushAfterAny,after_labels:=FlushAfterLabels}}}}=Data) 
when is_map_key(State, Map) and is_atom(map_get(Label, map_get(recv, map_get(State, Map)))) ->  
  %% forward if necessary
  case ForwardingEnabled of
    true ->
      case ForwardAny or lists:member(Label, ForwardLabels) of
        true ->
          show(verbose, {Name, "~p,\n\tforwarding msg to ~p.", [State, ForwardTo], Data}),
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
          show(verbose, {Name, "~p,\n\tflushing queue after receiving.", [State], Data}),
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
  %% update trace
  Trace1 = [NextState] ++ Trace,
  %% update data
  Data1 = Data#{msgs=>Msgs1,queue=>Queue2,trace=>Trace1},
  show({Name, "~p,\n\trecv (~p: ~p) -> ~p.", [State, Label, Payload, NextState], Data1}),
  {next_state, NextState, Data1};

%% from wrong states 
handle_event(info, {CoPartyID, Label, Payload}, State, #{name:=Name,coparty_id:=CoPartyID}=Data) ->
  show(verbose, {"~p, wrong state to recv (~p: ~p), postponing,\n\tdata: ~p.", [State, Label, Payload, Data]}, else, {"~p, wrong state to recv (~p: ~p), postponing.", [State, Label, Payload]}, {Name, Data}),
  {keep_state_and_data, [postpone]};






%% % % % % % %
%% timeouts
%% % % % % % %

%% during processing
handle_event(state_timeout, NextState, State, #{name:=Name}=Data) when NextState=:=State ->
  show(verbose, {Name, "~p,\n\tinternal timeout (~p),\n\tdata: ~p.", [State, NextState, Data], Data}),
  {next_state, NextState, Data};

%% mixed-choice
handle_event(state_timeout, NextState, State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\tinternal timeout (~p).", [State, NextState], Data}),
  {next_state, NextState, Data};






%% % % % % % %
%% retreive latest message of given label
%% % % % % % %
handle_event({call, From}, {recv, Label}, State, #{name:=Name,msgs:=Msgs}=Data) -> 
  show({Name, "~p,\n\tlooking for msg with label (~p).", [State, Label], Data}),
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

%% change options (single surface level key-val)
handle_event({call, From}, {options, OptionKey, Map}, State, #{name:=Name,options:=Options}=Data) 
when is_map_key(OptionKey, Options) and is_map(Map) ->    
  show({Name, "~p,\n\tchanging option:\n\t~p => ~p.", [State, OptionKey, Map], Data}),
  CurrentOption = maps:get(OptionKey, Options),
  NewOption = nested_map_merger(CurrentOption, Map),
  Options1 = maps:put(OptionKey, NewOption, Options),
  Data1 = Data#{options=>Options1},
  % show(verbose, {Name, "~p,\n\tupdated: ~p => ~p,\n\told: ~p\n\tnew: ~p.", [State,OptionKey, Map,Options,Options1], Data1}),
  {keep_state, Data1, [{reply, From, ok}]};
    
%% request copy of options
handle_event({call, From}, get_options, State, #{name:=Name,options:=Options}=Data) ->    
  show(verbose, {Name, "~p,\n\tsharing options with ~p,\n\tOptions: ~p.", [State, From, Options], Data}),
  {keep_state, Data, [{reply, From, {ok, Options}}]};

%% request copy of data
handle_event({call, From}, get_data, State, #{name:=Name}=Data) ->    
  show(verbose, {Name, "~p,\n\tsharing data with ~p,\n\tData: ~p.", [State, From, Data], Data}),
  {keep_state, Data, [{reply, From, {ok, Data}}]};

%% request fsm terminate
handle_event({call, _From}, {terminate, Reason}, State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\tinstructed to terminate,\n\treason: ~p.", [State, Reason], Data}),
  StopData = stop_data([{reason, Reason}, {data, Data}]), 
  {next_state, stop_state, StopData};






%% % % % % % %
%% anything else
%% % % % % % %
handle_event(EventType, EventContent, State, #{name:=Name}=Data) ->
  show({Name, "~p,\n\terror, reached unhandled event...\n\tEventType: ~p,\n\tEventContent: ~p,\n\tData: ~p.", [State, EventType, EventContent, Data], Data}),
  StopData = stop_data([{reason, unhandled_event}, {data, Data}]), 
  {next_state, stop_state, StopData}.

