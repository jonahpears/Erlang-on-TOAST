Erlang on TOAST artifact,

GitHub <https://github.com/jonahpears/Erlang-on-TOAST-timeout-protocol-reengineering-implementation->

An extension (fork) of:
Protocol re-engineering artifact,
GitHub  <https://github.com/LauraVoinea/protocol-reengineering-implementation>

## 0. updates

```erl
gen_stub:gen(ali,spec,default,"_ali_test.erl").
```

### todo

- examples of how to encode TOAST into protocol:
  - mixed-choice with gaps between sending and receiving regions
    - either add delays to protocols, or show how these can be accomodated for in protocol
  - single-direction cascading time constraints
    - time-consuming data with ability to skip if it takes too long. how to signal that an action is time consuming?
- explanation of diagonal constraints
- add `sample_app` directory for easy testing
- add sample `configuration presets` for monitors (verify only, enforce light, enforce strong)
- update readme properly
- smooth out session instantiation, using `tpri_session`
- add functionality for delay and timers
- look into using parse_transform to swap out certain functions for the ones in stub.hrl

## 1. Getting started guide

This is a tool for composing protocols, generating state machines based on
protocol specification, and extracting protocols from existing code.

The protocol type is defined within interleave.erl. This file also contains a few
examples together with the algorithm for protocol composition.

### Prerequisites

The following software needs to be installed:

- Erlang: Erlang/OTP 24 Erts 12.2.1
- rebar3: rebar 3.17.0

### Building

The tool can be built using rebar3:

    rebar3 compile

## 2. Step-by-Step Instructions

### Running

Enter the rebar3 shell:

  rebar3 shell
Protocol Composition:
Strong:

  interleave:interleave(Protocol1, Protocol2).

Weak:

    interleave:interleaveWeak(Protocol1, Protocol2).

Correlating:

  interleave:interleaveCorrelating(Protocol1, Protocol2).

All:

  interleave:interleaveAll(Protocol1, Protocol2).

For example:

    interleave:interleaveWeak(examples:pin(), examples:tan()).

Code Generation:

    generate:gen(Protocol, FileName).

For example:

    generate:gen(examples:pin(),"pin.erl").

Protocol Extraction:

    extract:protocol(FileName).

For example:

    extract:protocol("pin.erl").

To obtain Table 1 from the paper run:

  examples:table().

## 2. Overview of the claims

Claims supported by the artifact:

 1. Protocol Composition: Strong, Weak, Correlating, All
 2. Protocol Generation
 3. Protocol Extraction

Results presented in Table 1 should be obtained on any machine.
To obtain Table 1 from the paper run:

  examples:table().

The examples can be found under src/examples/
