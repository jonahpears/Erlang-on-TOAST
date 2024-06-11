# (this file is still a work-in-progress)
# to run a session with two parties
firstly, we provide an overview of the stubs content.


## run either with monitor

- set macro `MONITORED` to `true` in either module file

then, when spawned the file will create a transparent monitor within the same node, which it will then proceed to communicate through as part of the session.

- either party can do this, and nothing else needs to change

## terminal outputs

- macro `SHOW_ENABLED` set to `true` enables *general* messages to be output to the terminal at runtime.
- macro `SHOW_VERBOSE` set to `true` enables more *verbose* messages to be output to the terminal at runtime. (useful for debugging purposes.)


- macros `SHOW` is for general output messages, `VSHOW` for verbose and, `DO_SHOW` is an override and will always be shown.

## `stub.hrl`

this header file contains all of the helper-functions used by our stubs to seemlessly integrate inline monitoring (or not), configurable by the `MONITORED` macro.

### functions 

#### `set_timer(Name, Duration, Data)` 
- starts a timer via `erlang:start_timer` and stores the resulting `PID` under `Name` within `Data`, and returns the updated `Data` and the `PID`.
- if, `?MONITORED` returns `true` then the stub asks the monitor to start the timer for them, and the message will be forwarded automatically from the monitor to the process.
- otherwise, if `?MONITORED` is set to `false` then the stub will start the timer themselves.

# implementation examples

## basic ping-pong implementation

- file `exa_pingpong_recv_send` will wait until it has received `ping` from `exa_pingpong_send_recv`, and then respond with `pong`.
- since these protocols are *dual*, `exa_pingpong_send_recv` first sends `ping` and then waits to receive `pong`.

use the code below to *only* compile:
```erl
c(exa_pingpong_recv_send), c(exa_pingpong_send_recv), c(gen_monitor), c(toast_sup), c(toast_app).
```

the code below will run the implementation using `toast_app` and `toast_sup`:
```erl
c(exa_pingpong_recv_send), 
c(exa_pingpong_send_recv), 
c(toast_sup), c(toast_app), 
c(gen_monitor),
toast_app:start([
  {role, #{module=>exa_pingpong_recv_send,name=>rs_pong}},
  {role, #{module=>exa_pingpong_send_recv,name=>sr_ping}}]),
toast_app:run().
```

in the command above, we illustrate the kinds of parameters our "session application" takes to start the sessions. 
- `toast_app` takes a list of participant role maps (2 total)
- below is the role `rs_pong` corresponding to `basic_recv_send`:
```erl
RsPong = {role, #{module=>exa_pingpong_recv_send,name=>rs_pong}}.
```

the above specifies the module/file to use, and the name the process should go by.

### remember
> setting `?MONITORED` to `true` or `false` in the respective file (i.e., `exa_pingpong_recv_send` or `exa_pingpong_send_recv`) will determine which stubs will be spawned with inlined runtime monitors. it is possible for both, one, or none to be spawned with monitors.



## using process timers for timeouts & branching/selection implementation

- file `exa_timer_timeout_selection` must make a selection beofore timer `t` reaches 0 (starting from 5000ms). 
it uses some function `select_state/1` to make the selection, via the helper-function named `nonblocking_selection` (in `stub.hrl`) which spawns this potentially-blocking behaviour as a new process. 
- the `nonblocking_selection` function requires that the `select_state/` function returns both the label of the selected message, but also a function to obtain the payload of the message, which are then all returned to the main process, if they complete before the timeout via timer `t`.
- otherwise, if timer `t` completes before both the label and payload are ready, a timeout is triggered.

compile with the following command:
```erl
c(exa_timer_timeout_selection), c(exa_timer_timeout_branching), c(toast_sup), c(toast_app).
```

or, compile and run with the following command:
```erl
c(exa_timer_timeout_selection), 
c(exa_timer_timeout_branching), 
c(toast_sup), c(toast_app), 
c(gen_monitor),
toast_app:start([
  {role, #{module=>exa_timer_timeout_selection,name=>selector}},
  {role, #{module=>exa_timer_timeout_branching,name=>brancher}}]),
toast_app:run().
```



```erl
c(exa_timer_timeout_selection), 
c(exa_timer_timeout_branching), 
c(toast_sup), c(toast_app), 
c(gen_monitor),
toast_app:start([
  {role, #{module=>exa_timer_timeout_branching,name=>brancher}},
  {role, #{module=>exa_timer_timeout_selection,name=>selector}}]),
toast_app:run().
```













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
