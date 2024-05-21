-module(imp_msger).

-file("imp_msger.erl", 1).

-export([ init/0, run/1, run/2 ]).

init() ->
  M = 0,
  run(M).

run(M) ->
  M ! {send, msg1, "A"},
  receive {M, recv, ack1, _Ack1} -> run(M)
  after 3 -> run(b, M)
  end.

run(b, M) -> 
  M ! {send, msg2, "B"},
  receive {M, recv, ack2, _Ack2} -> run(M)
  after 3 -> M ! {send, msg2, "B"}
  end.

