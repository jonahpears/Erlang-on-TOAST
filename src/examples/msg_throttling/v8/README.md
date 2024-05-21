


# Compiling Application

```erl
c(tpri_app), c(tpri_server), c(tpri_sup), c(role_sup), c(role_tmp).
```

```erl
c(role_fsm_ali), c(role_imp_ali).
```

```erl
c(role_fsm_bob), c(role_imp_bob).
```

## All Together
```erl
c(tpri_app), c(tpri_server), c(tpri_sup), c(role_sup), c(role_tmp), c(role_fsm_ali), c(role_imp_ali), c(role_fsm_bob), c(role_imp_bob).
```


# Running Application
Inside the `Erlang` shell environment:
```erl
tpri_app:start().
```
or
```erl
tpri_app:start(Params).
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

# Execution Examples

```erl
c(tpri_app), c(tpri_server), c(tpri_sup), c(role_sup), c(role_tmp), c(role_fsm_ali), c(role_imp_ali), c(role_fsm_bob), c(role_imp_bob), tpri_app:start().
```
