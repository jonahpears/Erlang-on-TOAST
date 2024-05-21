%% @doc Processes Fsm (from build_fsm:to_fsm) to map() used by monitors
-module(build_spec).

-export([to_monitor_spec/1]).

-spec to_monitor_spec({list(), map()}) -> map().
to_monitor_spec(_Fsm) -> {pass, #{}}.