-module(stub).

-include_lib("stdlib/include/assert.hrl").

%% exported from within stub.hrl
-export([ start_link/0,
          start_link/1,
          init/1 
        ]).

-include("tool_output/goals/gen_stub/stub.hrl").


main(_CoParty) -> ok.

  


