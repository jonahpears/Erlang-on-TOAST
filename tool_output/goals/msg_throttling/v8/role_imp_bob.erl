-module(role_imp_bob).
-file("role_imp_bob.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, bob).

-include("printout.hrl").
-include("role_cb_funs.hrl").

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
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {tpri, Name, imp, self()},

  %% wait to receive coparty ID
  receive
    {setup_coparty, ToClient} -> 
      %% setup options with monitor
      %% request messages to be immediately forwarded automatically
      special_request(ToClient, {options, forward_receptions, #{enabled=>true,to=>self()}}),
      %% allow sending actions to be queued if untimely
      special_request(ToClient, {options, queue_actions, #{enabled=>true,flush_after_recv=>false}}),
      %% monitor printout
      special_request(ToClient, {options, printout_enabled, true}),
      %% wait for signal to begin
      receive
        {setup_finished, start} -> ?MODULE:main(ToClient)
      end
  end.

%% bob is a server that processes messages .
%% bob receives messages that are a sequence of '*' representing some large data.
%% each '*' takes 1 second to process.

%% bob is meant to be able to process each message in under 3 seconds.
%% bob tries sends a message once a message has been processed.
%% sometimes bob will receive the next message before responding to the previous.
%% bob's monitor handles the time sensitive communication.
%% bob requests that all messages from the client (received by monitor) are forwarded immediately.

%% bob can either acknowledge each message distinctly,
%% or, bob can send a generic acknowledgement. <- we show this case

%% crux: 
%% 1) the protocol expects the server to be able to respond within 3 seconds
%% 2) the protocol specifies that 2 unacknowledged messages is a violation
%% 3) if ali sends a message of "***" or more, the server will be forced to violate the protocol

main(ToClient) ->
  receive 
    {ToClient, Label, Msg} -> 
      printout(?NAME, "~p, processing: ~p.",[?FUNCTION_NAME,{Label,Msg}]),
      process_msg(Msg), 
      mon_send(ToClient, ack),
      main(ToClient) 
  end.


process_msg("") -> ok;
process_msg("*" ++ Msg) -> timer:sleep(1000), process_msg(Msg).


