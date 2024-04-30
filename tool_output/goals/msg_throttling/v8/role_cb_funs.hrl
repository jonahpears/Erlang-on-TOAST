-compile({nowarn_unused_function, [ {mon_send,2},
                                    {mon_send,3},
                                    {mon_send,4},
                                    {mon_recv,2},
                                    {mon_recv,3},
                                    {mon_terminate,1},
                                    {mon_terminate,2},
                                    {special_request,2} ]}).

%% callback functions (intended for user to monitor)

%% for full message, send to fsm
mon_send(To, Label, Payload, Meta) -> To ! {act, send, Label, Payload, Meta}.

%% for label and payload, add empty meta
mon_send(To, Label, Payload) when is_atom(Label) -> mon_send(To, Label, Payload, []);

%% for meta and no label
mon_send(To, Payload, Meta) when is_list(Meta) -> 
  %% ensure auto_label is at front
  Map = maps:from_list(Meta),
  Map1 = maps:remove(auto_label, Map),
  Meta1 = maps:to_list(Map1),
  Meta2 = [{auto_label, #{enabled=>true}}] ++ Meta1,
  mon_send(To, dont_care, Payload, Meta2).

%% if only label, send empty payload
mon_send(To, Label) when is_atom(Label) -> mon_send(To, Label, "");

%% if only payload, dont_care label and ask for auto_label
mon_send(To, Payload) -> mon_send(To, dont_care, Payload, [{auto_label, #{enabled=>true}}]).



mon_recv(all, From, Label) -> 
  {ok, #{ label:=_Label, msg:=_Msg, total:=_Total, matches:=Matches}} = gen_statem:call(From, {recv, Label}),
  Matches.
mon_recv(From, Label) -> [H|_T] = mon_recv(all, From, Label), H.



special_request(From, {options, Key, Val}) -> gen_statem:call(From, {options, Key, Val}).


mon_terminate(To, Reason) -> gen_statem:call(To, {terminate, Reason}).
mon_terminate(To) -> mon_terminate(To, normal).

