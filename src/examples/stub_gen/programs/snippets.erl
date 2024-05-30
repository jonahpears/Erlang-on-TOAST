-module(snippets).
-compile(export_all).
-compile(nowarn_export_all).

-include("stub.hrl").


%% this file is for examplar code snippets to be aimed for by the tool



snippet(delay_timer) ->
  Timer_t = erlang:start_timer(5000, self(), {timer_expired, t}),
  receive {timeout, Timer_t, {timer_expired, t}} -> ok end,
  ok;


snippet(recv_after_timer) ->
  Timer_t = erlang:start_timer(5000, self(), {timer_expired, t}),
  receive 
    {_, msg1, _} -> 
      ok; %% reception
    {timeout, Timer_t, {timer_expired, t}} -> 
      ok  %% enter after branch
  end,
  ok;


snippet(send_after_timer) ->
  Timer_t = erlang:start_timer(5000, self(), {timer_expired, t}),
  NonBlocking = nonblocking_waiter(fun(_)->ok end,[],self(),Timer_t),
  receive 
    {NonBlocking, ok, Result} -> 
      ok; %% send Result 
    {NonBlocking, ko} -> 
      ko  %% enter after branch
  end,
  ok;





snippet(endP) -> ok.
