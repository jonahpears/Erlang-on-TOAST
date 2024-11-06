Erlang on TOAST tool,

GitHub <https://github.com/jonahpears/Erlang-on-TOAST>

An extension (fork) of:
Protocol re-engineering artifact,
GitHub  <https://github.com/LauraVoinea/protocol-reengineering-implementation>

# 1. Building Guide

## Getting Started

This is a tool for composing protocols, generating state machines based on
protocol specification, and extracting protocols from existing code.

The protocol type is defined within interleave.erl. This file also contains a few
examples together with the algorithm for protocol composition.

### Building

The tool can be built using rebar3:
```erl
rebar3 compile
```

## Step-by-Step Instructions

### Stub Generation (via toolchain)

Compile and enter the rebar3 shell:
```erl
rebar3 compile; rebar3 shell
```

Run the following command:
```erl
gen_stub:gen(ProtocolName,Protocol,FileNameSuffix).
```

For example:
```erl
gen_stub:gen(basic_send_recv,ali:spec(basic_send_recv),"_ali_test.erl").
```
Will generate a stub named `basic_send_recv_ali_test.erl` to `/tool_output/`, using the protocol retrned by `ali:spec(basic_send_recv)`.

Alternatively, the above is the same as writing: (via a helper function)
```erl
gen_stub:gen(ali,spec,basic_send_recv,"_ali_test.erl").
```

Additionally, a file named `mon_spec_basic_send_recv_ali_test.erl` will be created in `/tool_output/` containing the FSM map used by the monitor.
This file is already included in the generated stubs automatically, to allow more convenient enabling/disabling of the runtime monitors.




# 2. Sample Project
Please see the [`sample app`](https://github.com/jonahpears/Erlang-on-TOAST-sample-app) repo (moved here).

## 2.1 Monitoring Template
Please see [`/sample_app/src/monitor template/gen_monitor.erl`](https://github.com/jonahpears/Erlang-on-TOAST-sample-app/blob/main/src/monitor%20template/gen_monitor.erl).

## 2.2 Generated Stub Utility Functions
Please see [`/sample_app/include/stub.hrl`](https://github.com/jonahpears/Erlang-on-TOAST-sample-app/blob/main/include/stub.hrl).





# 3. Erlang API
Defined as `-type protocol()` in [`/src/interleave/interleave.erl`](https://github.com/jonahpears/Erlang-on-TOAST/tree/main/src/interleave/interleave.erl).



# 4. Stub Generation




---

# need to update

> The examples can be found under src/examples/
most of these examples will not work since we have removed some features from the original tool


# todo
- Find a way to include macros in clauses when generating using `?Q([])` from `merl_build`, currently had to substitute macros in the stubs generated with `TempMacroPlaceholder_` instead of `?`. The `?` itself seemed to be causing issues when using `?Q([])`. Currently thinking about trying to use parse_transform to fix this, along with adding comments inside bodies of functions (as this is another issue I have encountered when using `merl_build`).

current work around: after writing to file, replace each occurance of `TempMacroPlaceholder_` with `?`. Not ideal.

## general todo
- update readme properly
- improve `sample_app` directory tutorial/example
- add sample `configuration presets` for monitors (verify only, enforce light, enforce strong) - (currently, these have to be configured manually over several options. adding presets would essentially bundle sets of options together.)
- clean up repo (alot of remnants from the original project still exist that are no longer relevant.)

## monitors todo
In the current example, we have not showcased the enforcement monitoring tested in an earlier version.
- create an example that uses the enforcement monitoring,
- offer presets for easire configuration (currently, with the monitors the examples are based upon, you have to specify individual features and their constraints to get enforcement monitors).
