
# running v3 (tool generated state-enter statem)

```erl
c(msg_msger),c(msg_acker),c(msg_supervisor),c(msg_server),msg_server:start_link().
```

```erl
msg_msger:send(msg1,"A"),msg_msger:send(msg2,"B"),msg_acker:send(ack1,"Z").
```

## generate v3 (state-enter statem)
```erl
generate:gen(msg_throttling_v3:role_msger(),"_msg_throttling_msger_v3.erl"),generate:gen(msg_throttling_v3:role_acker(),"_msg_throttling_acker_v3.erl").
```


# running v6 (master template)

```erl
c(msg_worker), c(msg_supervisor), c(msg_server), {ok, ServerID} = msg_server:start_link().
```

```erl
{{_, MsgerID}, {_, AckerID}} = {msg_supervisor:get_child(msger,msg_worker), msg_supervisor:get_child(acker,msg_worker)}.
```

```erl
MsgerID ! {act, send, msg1, "A"}, AckerID ! {act, send, ack1, "B"}.
```

```erl
MsgerID ! {act, send, msg1, "A"}, MsgerID ! {act, send, msg1, "A"}, AckerID ! {act, send, ack1, "B"}.
```
