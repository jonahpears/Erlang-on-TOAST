-compile({nowarn_unused_function, [ {data,0},
                                    {data,1},
                                    {default_data,0},
                                    {stop_data,0},
                                    {stop_data,1},
                                    {default_stop_data,0},
                                    {new_queued_action,3},
                                    {default_queued_action,0},
                                    {default_action_meta,0},
                                    {default_options,0},
                                    {default_options,1},
                                    {reset_return_state,1} ]}).

reset_return_state(#{queue:=#{state_to_return_to:=To}=Queue}=Data) ->
  ?VSHOW("resetting from (~p) to undefined.",[To],Data),
  maps:put(queue,maps:put(state_to_return_to,undefined,Queue),Data).




%%%%%%%%%%%%
%% data functions
%%%%%%%%%%%%

data() -> data([]).

data(List) -> list_to_map_builder(List, default_data()).

default_data() ->
  #{ session_id => undefined,
     sus_init_id => undefined,
     sus_id => undefined,
     role => undefined,
    %  name => undefined, 
     coparty_id => undefined, 
     process_timers => #{},
     fsm => #{ init => undefined, %% initial state
               timeouts => #{}, %% outgoing silent edges from states
               timers => #{}, %% timers and maps to states they can trigger in and then lead to
               resets => #{}, %% states to reset certain timers in to what value
               enter_flags => #{}, %% signify which things have been done on enter current state, reset each new state
               map => #{} }, %% outgoing edges from states
     trace => [], %% list of state-names reached
     msgs => #{}, %% received messages (maps labels to lists of payloads)
     queue => #{ on => [], 
                 off => [], 
                 check_recvs => [],
                 state_to_return_to => undefined },
     options => default_options() }.


default_options() -> 
  #{ printout => default_options(printout)
    %  delayable_sends => default_options(delayable_sends),
    %  queue => default_options(queue),
    %  forward_receptions => default_options(forward_receptions),
    %  support_auto_label => default_options(support_auto_label) %% only relevant to sending actions
    }.

default_options(printout) -> #{ enabled => true, verbose => true, termination => true };

default_options(delayable_sends) -> #{ enabled => false };
default_options(queue) -> 
  #{ enabled => false, 
     flush_after_recv => #{ enabled => false, 
                            after_any => false, 
                            after_labels => [] },
     aging => #{ enabled => false,
                 max_age => -1 } %% ignore local ages too
    };
default_options(forward_receptions) -> #{ enabled => true, any => true, labels => [] };
default_options(support_auto_label) -> #{ enabled => false }.


new_queued_action(Label, Payload, Meta) when is_list(Meta) -> 
  Action = default_queued_action(),
  Action#{ label=>Label,
           payload=> Payload,
           meta => list_to_map_builder(Meta, default_action_meta()) }.


default_queued_action() ->
  #{ event_type => cast,
     label => undefined,
     payload => undefined }.

default_action_meta() ->
  #{ queue => #{ enabled => false },
     aging => #{ enabled => false, age => 0 },
     drop => #{ after_recv => false, after_labels => []},
     auto_label => #{ enabled => false } }.

% queued_action_to_event(#{label:=Label,payload:=Payload,age:=Age}=_Action) -> {next_event, cast, {send, Label, Msg, Age}}


stop_data() -> stop_data([]).

stop_data(List) -> list_to_map_builder(List, default_stop_data()).

default_stop_data() -> 
  #{ reason => undefined,
     data => default_data() }.




