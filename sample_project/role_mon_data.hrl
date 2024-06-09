-compile({nowarn_unused_function, [ {data,0},
                                    {data,1},
                                    {default_data,0},
                                    {stop_data,0},
                                    {stop_data,1},
                                    {default_stop_data,0},
                                    {new_queued_action,3},
                                    {default_queued_action,0},
                                    {default_action_meta,0} ]}).

data() -> data([]).

data(List) -> list_to_map_builder(List, default_data()).

default_data() ->
  #{ session_id => undefined,
     start_id => undefined,
     sus_id => undefined,
     role => undefined,
     name => undefined, 
     coparty_id => undefined, 
     fsm => #{ init => undefined, %% initial state
               timeouts => #{}, %% outgoing silent edges from states
               map => #{} }, %% outgoing edges from states
     trace => [], %% list of state-names reached
     msgs => #{}, %% received messages (maps labels to lists of payloads)
     queue => #{ on => [], 
                 off => [], 
                 check_recvs => [],
                 state_to_return_to => undefined },
     options => default_options() }.


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




