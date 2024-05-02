-module(role_imp_cal).
-file("role_imp_cal.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, cal_imp).

-define(THROTTLING_CAPACITY, 2).

%% comment out below to change behaviour of program
-define(SAFE_UPPER_BOUND, 2). %% ! <- comment in/out
-ifdef(SAFE_UPPER_BOUND).
-define(UPPER_BOUND, ?SAFE_UPPER_BOUND).
-else.
-define(UPPER_BOUND, 10).
-endif.

-define(SAFE_DURATION, 1000). %% ! <- comment in/out
-ifdef(SAFE_DURATION).
-define(DURATION, ?SAFE_DURATION).
-else.
-define(DURATION, 500).
-endif.

-include("printout.hrl").
-include("role_cb_funs.hrl").
-include("rand_from_range.hrl").

start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout(?NAME, "~p.", [?FUNCTION_NAME]),
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
    {setup_coparty, Server} -> 
      receive
        {setup_finished, start} -> main(Server)
      end
  end.

%% cal is a client that sends messages
%% cal is meant to behave the same as ali
%% but cal does not have a monitor
%% cal must behave correctly by themselves
%% cal may represent a more typical implementation

main(Server) -> 
  %% determine loop iterations
  % Num = rand_from_range(1, 100),
  Num = 12, 
  %% begin loop
  {Remainder, Capacity} = loop(Server, Num, ?THROTTLING_CAPACITY),
  %% finished
  printout(?NAME, "~p, sent (~p/~p) messages.", [?FUNCTION_NAME, Num-Remainder,Num]),
  case Capacity=:=0 of
    true -> error_exceeded_throttling_capacity;
    _ -> ok
  end.

%% buffer exceeded
loop(_Server, Iterations, 0) ->
  printout(?NAME, "~p, throttling exceeded!", [?FUNCTION_NAME]),
  {Iterations, 0};

%% loop finished
loop(_Server, 0, Capacity) ->
  printout(?NAME, "~p, finished.", [?FUNCTION_NAME]),
  {0, Capacity};

%% main loop -- entry point, when capacity is maximal
loop(Server, Iterations, ?THROTTLING_CAPACITY=Capacity) ->
  do_send(Server, Iterations),
  loop(Server, Iterations-1, Capacity-1);

loop(Server, Iterations, Capacity) ->
  %% wait to receive for 5000 ms
  receive 
    {Server, ack, _} -> %% if receive promptly, regain capacity and reenter
      loop(Server, Iterations, Capacity+1)
  after 
    5000 -> %% otherwise, do send again and continue
      do_send(Server, Iterations),
      loop(Server, Iterations-1, Capacity)
  end.

do_send(Server, Iterations) ->
  %% get length of next message to send
  Len = rand_from_range(1, ?UPPER_BOUND),
  %% build and send message
  Message = msg_builder(Len) ++ integer_to_list(Iterations),
  dir_send(Server, msg, Message),
  printout(?NAME, "~p, ~p to ~p.", [?FUNCTION_NAME, {msg,Message}, Server]),
  ok.
  

%% build message of length Num
msg_builder(Num) when is_integer(Num) -> msg_builder("",Num).

%% stop building message
msg_builder(Msg, 0) -> Msg++"*";

%% build message, which takes ?DURATION
msg_builder(Msg, Num) -> 
  timer:sleep(?DURATION), 
  %% continue building message
  msg_builder(Msg++"*", Num-1).

