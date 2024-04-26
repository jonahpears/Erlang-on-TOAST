-compile({nowarn_unused_function, [ {mon_send,2},
                                    {mon_send,3},
                                    {mon_recv,2},
                                    {mon_recv,3},
                                    {mon_terminate,1},
                                    {mon_terminate,2},
                                    {special_request,2} ]}).

%% callback functions (intended for user to monitor)
mon_send(To, Label, Payload) -> To ! {act, send, Label, Payload}.
mon_send(To, Label) -> mon_send(To, Label, "").

mon_recv(all, From, Label) -> 
  {ok, #{ label:=_Label, msg:=_Msg, total:=_Total, matches:=Matches}} = gen_statem:call(From, {recv, Label}),
  Matches.
mon_recv(From, Label) -> [H|_T] = mon_recv(all, From, Label), H.

special_request(From, {options, Key, Val}) -> gen_statem:call(From, {options, Key, Val}).


mon_terminate(To, Reason) -> gen_statem:call(To, {terminate, Reason}).
mon_terminate(To) -> mon_terminate(To, normal).

