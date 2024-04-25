-module(role_imp_ali).
-file("role_imp_ali.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, ali).

-include("printout.hrl").
-include("role_cb_funs.hrl").
-include("rand_from_range.hrl").

start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  PID = erlang:spawn_link(?MODULE, ?MODULE, init, Params),
  {ok, PID}.


%% @doc init sequence -- waits to receive PID of monitor
init(Params) ->
  printout(?NAME, "~p.", [?FUNCTION_NAME]),

  Params1 = maps:from_list(Params),
  Name = maps:get(name,Params1),
  ?assert(Name==?NAME, io_lib:format("Error in ~p: Name in file (~p) does not match Name provided (~p).",[?MODULE,?NAME,Name])),

  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {tpri, Name, imp, self()},

  %% wait to receive coparty ID
  receive
    {setup_coparty, ToServer} -> 
      %% setup options with monitor
      %% allow sending actions to be queued if untimely
      special_request(ToServer, {options, queue_actions, #{enabled=>true,flush_after_recv=>false}}),
      %% monitor printout
      special_request(ToServer, {options, printout_enabled, true}),
      %% wait for signal to begin
      receive
        {setup_finished, start} -> ?MODULE:main(ToServer)
      end
  end.

%% ali is a client that sends messages.
%% ali's messages are a sequence of '*' representing some large data.
%% each '*' takes 500 ms to create.

%% ali is sending these to the server for processing.
%% ali does not care about anything the server sends in response.
%% ali only cares about sending messages.
%% ali instructs the monitor to queue all messages, regardless of response.
%% ali's monitor handles sending messages to the server at the right time.

%% crux: 
%% 1) the protocol expects the server to be able to respond within 3 seconds
%% 2) the protocol specifies that 2 unacknowledged messages is a violation
%% 3) if ali sends a message of "***" or more, the server will be forced to violate the protocol

main(ToServer) ->
  Num = rand_from_range(1, 10)+1,
  loop(ToServer, Num).

loop(_ToServer, 0) -> 
  printout(?NAME, "~p, finished.", [?FUNCTION_NAME]),
  ok;
loop(ToServer, Iterations) ->
  Len = rand_from_range(1, 10),
  Message = msg_builder(Len),
  printout(?NAME, "~p, build ~p: ~p.",[?FUNCTION_NAME,Iterations,Message]),

  mon_send(ToServer, msg, Message),
  loop(ToServer, Iterations-1).


msg_builder(Num) when is_integer(Num) -> 
  msg_builder("",Num).

msg_builder(Msg, 0) -> Msg;

msg_builder(Msg, Num) -> 
  timer:sleep(500), 
  msg_builder(Msg++"*", Num-1).

