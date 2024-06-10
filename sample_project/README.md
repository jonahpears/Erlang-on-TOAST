
# to run a session with two parties

- one named `basic_recv_send__ali_test`
- another named `basic_send_recv__ali_test`


```erl
c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup), c(toast_app), toast_app:start([{role, #{module=>basic_recv_send__ali_test,name=>rs_ali}},{role, #{module=>basic_send_recv__ali_test,name=>sr_ali}}]),toast_app:run().
```

## run either with monitor

- set macro `MONITORED` to `true` in either module file

then, when spawned the file will create a transparent monitor within the same node, which it will then proceed to communicate through as part of the session.

- either party can do this, and nothing else needs to change


## other scraps (for dev)

```erl
c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup).
```

```erl
toast_sup:start_link([{role, #{module=>basic_recv_send__ali_test,name=>rs_ali}},{role, #{module=>basic_send_recv__ali_test,name>sr_ali}}]).
```

```erl
c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup), toast_sup:start_link([{role, #{module=>basic_recv_send__ali_test,name=>rs_ali}},{role, #{module=>basic_send_recv__ali_test,name=>sr_ali}}]).
```

```erl
c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup), basic_recv_send__ali_test:start_link().
```
