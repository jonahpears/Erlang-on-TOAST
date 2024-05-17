-module(ali_stub).

-include_lib("stdlib/include/assert.hrl").

-export([ main/1
        ]).

%% @doc determines if start_link/0/1 creates monitor in the same node
-define(MONITORED, true).
%% @doc if monitored, put specification here (i.e.: module:spec_fun() or spec_map=#{...}) OR, pass in with Args list
-define(MONITOR_SPEC, false).

-include("tool_output/goals/gen_stub/stub.hrl").

main(_CoParty) -> ok.

  


