
# to run 1 with monitors 


```erl
c(gen_monitor), c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup), c(toast_app), toast_app:start([{role, #{module=>basic_recv_send__ali_test,name=>rs_ali}},{role, #{module=>basic_send_recv__ali_test,name=>sr_ali}}]),toast_app:run().
```

# to run 2 without monitors 


```erl
c(basic_recv_send__ali_test), c(basic_send_recv__ali_test), c(toast_sup), c(toast_app), toast_app:start([{role, #{module=>basic_recv_send__ali_test,name=>rs_ali}},{role, #{module=>basic_send_recv__ali_test,name=>sr_ali}}]),toast_app:run().
```


## other scraps

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
