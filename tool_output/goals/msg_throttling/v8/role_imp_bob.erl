-module(role_imp_bob).
-file("role_imp_bob.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, bob_role_imp).

%% comment out below to change behaviour of program
-define(SAFE_DURATION, 0). %% ! <- comment in/out
-ifdef(SAFE_DURATION).
-define(DURATION, ?SAFE_DURATION).
-else.
-define(DURATION, 1000).
-endif.

-include("printout.hrl").
-include("role_cb_funs.hrl").

start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout(?NAME, "~p.", [?FUNCTION_NAME]),
  % printout(?NAME, "~p, Params: ~p.", [?FUNCTION_NAME, Params]),
  Params1 = maps:from_list(Params),
  RoleID = maps:get(reg_id,Params1),
  ?assert(RoleID==?MODULE, io_lib:format("Error in ~p: Module of file (~p) does not match Role/ID provided (~p).",[?MODULE,?MODULE,RoleID])),
  Name = maps:get(name,Params1),
  ?assert(Name==?NAME, io_lib:format("Error in ~p: Name in file (~p) does not match Name provided (~p).",[?MODULE,?NAME,Name])),
  PID = erlang:spawn_link(?MODULE, init, [Params]),
  printout(?NAME, "leaving ~p as : ~p.", [?FUNCTION_NAME, PID]),
  {ok, PID}.


%% @doc init sequence -- waits to receive PID of monitor
init(Params) ->
  printout(?NAME, "~p.", [?FUNCTION_NAME]),

  Params1 = maps:from_list(Params),
  Role = maps:get(role,Params1),

  %% get app ID and send self() and Role
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {role, Role, imp, self()},

  %% wait to receive coparty ID
  receive
    {setup_coparty, Client} -> 
      %% setup options with monitor
      % %% request messages to be immediately forwarded automatically
      % special_request(Client, {options, forward_receptions, #{enabled=>true,to=>self(),any=>true}}),
      % %% allow sending actions to be queued if untimely
      % special_request(Client, {options, queue, #{enabled=>true,flush_after_recv=>#{enabled => false}}}),

      % %% request messages to be immediately forwarded automatically
      % special_request(Client, {options, forward_receptions, [{enabled,true},{to,self()}{any,true}]}),
      % %% allow sending actions to be queued if untimely
      % special_request(Client, {options, queue, [{enabled,true},{to,self()}{any,true}]}),
      % special_request(Client, {options, queue, #{enabled=>true,flush_after_recv=>#{enabled => false, after_any => false, after_labels => []},aging => #{ enabled => false, max_age => -1}}}),
      % %% monitor printout
      % special_request(Client, {options, printout, #{ enabled => true, verbose => true }}),


      %% request messages to be immediately forwarded automatically
      special_request(Client, {options, forward_receptions, #{enabled=>true,to=>self(),any=>true,labels=> []}}),
      %% allow sending actions to be queued if untimely
      special_request(Client, {options, queue, #{enabled=>true,flush_after_recv=>#{enabled => false, after_any => false, after_labels => []},aging => #{ enabled => false, max_age => -1}}}),
      %% monitor printout
      special_request(Client, {options, printout, #{ enabled => true, verbose => true }}),

      %% wait for signal to begin
      receive
        {setup_finished, start} -> 
          printout(?NAME, "~p, process duration: ~p.", [?FUNCTION_NAME, ?DURATION]),
          printout(?NAME, "leaving ~p.", [?FUNCTION_NAME]),
          main(Client)
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

main(Client) -> loop(Client).

%% main loop
loop(Client) ->
  %% receive message from client
  receive 
    {Client, Label, Msg} -> 
      printout(?NAME, "~p, processing: ~p.",[?FUNCTION_NAME,{Label,Msg}]),
      process_msg(Msg), 
      %% after processing, send ack
      mon_send(Client, ack),
      %% loop
      loop(Client) 
  end.

%% process message, each * takes ?DURATION
process_msg("*" ++ Msg) -> timer:sleep(?DURATION), process_msg(Msg);
process_msg(_Msg) -> ok.


