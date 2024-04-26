-module(role_imp_ali).
-file("role_imp_ali.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, ali_role_imp).

%% comment out below to change behaviour of program
% -define(SAFE_UPPER_BOUND, 2). %% ! <- comment in/out
-ifdef(SAFE_UPPER_BOUND).
-define(UPPER_BOUND, ?SAFE_UPPER_BOUND).
-else.
-define(UPPER_BOUND, 10).
-endif.

% -define(SAFE_DURATION, 1000). %% ! <- comment in/out
-ifdef(SAFE_DURATION).
-define(DURATION, ?SAFE_DURATION).
-else.
-define(DURATION, 500).
-endif.

-include("printout.hrl").
-include("role_cb_funs.hrl").
-include("rand_from_range.hrl").

start_link() -> start_link([]).

start_link(Params) -> 
  printout(?NAME, "~p.", [?FUNCTION_NAME]),
  % printout(?NAME, "~p, Params: ~p.", [?FUNCTION_NAME, Params]),
  Params1 = maps:from_list(Params),
  RoleID = maps:get(id,Params1),
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
    {setup_coparty, ToServer} -> 
      %% setup options with monitor
      %% allow sending actions to be queued if untimely
      special_request(ToServer, {options, queue_actions, #{enabled=>true,flush_after_recv=>false}}),
      %% monitor printout
      special_request(ToServer, {options, printout_enabled, true}),
      %% wait for signal to begin
      receive
        {setup_finished, start} -> 
          printout(?NAME, "~p, build upper bound: ~p.", [?FUNCTION_NAME, ?UPPER_BOUND]),
          printout(?NAME, "~p, build duration: ~p.", [?FUNCTION_NAME, ?DURATION]),
          printout(?NAME, "leaving ~p.", [?FUNCTION_NAME]),
          main(ToServer),
          %% finish ?
          mon_terminate(ToServer)
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
  %% determine loop iterations
  Num = rand_from_range(1, 10),
  %% begin loop
  loop(ToServer, Num).

%% loop finished
loop(_ToServer, 0) -> 
  printout(?NAME, "~p, finished.", [?FUNCTION_NAME]),
  ok;

%% main loop
loop(ToServer, Iterations) ->
  %% get length of next message to sent
  Len = rand_from_range(1, ?UPPER_BOUND),
  printout(?NAME, "~p, building ~p.",[?FUNCTION_NAME,Iterations]),

  %% build and send message 
  Message = msg_builder(Len) ++ integer_to_list(Iterations),
  mon_send(ToServer, msg, Message),
  printout(?NAME, "~p, sent ~p.",[?FUNCTION_NAME,Message]),

  %% loop
  loop(ToServer, Iterations-1).


%% build message of length Num
msg_builder(Num) when is_integer(Num) -> msg_builder("",Num).

%% stop building message
msg_builder(Msg, 0) -> Msg++"*";

%% build message, which takes ?DURATION
msg_builder(Msg, Num) -> 
  timer:sleep(?DURATION), 
  %% continue building message
  msg_builder(Msg++"*", Num-1).

