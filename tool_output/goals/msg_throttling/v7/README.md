
# Preparing Application
Inside the `Erlang` shell environment:
```erl
c(tpri_app), c(tpri_server), c(tpri_supervisor), c(msg_worker), c(msg_protocol), c(data_records), c(msger), c(acker).
```

# Running Application
Inside the `Erlang` shell environment:
```erl
tpri:start().
```
or
```erl
tpri:start(Params).
```
where `Params` is a list.

## As `.app`
Adapted from [Erlang/OTP docs](https://www.erlang.org/doc/design_principles/applications#starting-and-stopping-applications).
Inside the `Erlang` shell environment:

First, we must load the application:
```erl
application:load(tpri_app).
```

Next, we may start the application:
```erl
application:start(tpri_app).
```

To stop the application:
```erl
application:stop(tpri_app).
```

