-module(stub).

-include_lib("stdlib/include/assert.hrl").

%% exported from within stub.hrl
-export([ run/1, run/2, main/2
        ]).

-include("stub.hrl").


run(CoParty) -> run(CoParty, default_map()).
run(_CoParty, Data) -> ok.

main(_CoParty, Data) -> ok.

  


