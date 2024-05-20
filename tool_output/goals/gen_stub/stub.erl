-module(stub).

-include_lib("stdlib/include/assert.hrl").

%% exported from within stub.hrl
-export([ run/1, run/2, main/2
        ]).

-include("tool_output/goals/gen_stub/stub.hrl").


run(CoParty) -> run(CoParty, []).
run(_CoParty, Data) -> ok.

main(_CoParty, Data) -> ok.

  


