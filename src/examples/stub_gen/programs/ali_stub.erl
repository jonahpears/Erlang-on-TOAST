-module(ali_stub).

-include_lib("stdlib/include/assert.hrl").

-export([ run/1, run/2, main/2
        ]).

%% @doc determines if start_link/0/1 creates monitor in the same node
-define(MONITORED, true).
%% @doc if monitored, put specification here (i.e.: module:spec_fun() or spec_map=#{...}) OR, pass in with Args list
-define(MONITOR_SPEC, false).

-include("stub.hrl").

run(CoParty) -> run(CoParty, []).
run(CoParty, Data) -> main(CoParty,Data).

%% {act, s_msg, {rec, a, {select, [{s_a, {rvar, a}}, {s_b, endP}] }}}

main(CoParty, Data) -> 
  Payload_s_msg = ok,
  send(CoParty, {s_msg, Payload_s_msg}),
  loop_a(CoParty, Data).

loop_a(CoParty, Data) ->
  SelectOption = ok, %% set to some function to decide selection
  SelectedPayload = ok, 
  case SelectOption of
    s_a -> 
      send(CoParty, {s_a, SelectedPayload}),
      loop_a(CoParty, Data);
    s_b -> 
      send(CoParty, {s_b, SelectedPayload}),
      stopping(normal, CoParty, Data)
  end.

stopping(Reason, CoParty, Data) -> ok.
      


  