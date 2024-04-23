-module(tpri_server).
-file("tpri_server.erl", 1).
-behaviour(gen_server).

-include("tpri_data_records.hrl").

%% gen_server API
-export([ start_link/0,
          init/1,
          handle_call/3,
          handle_cast/2
          % handle_info/2
         ]).

-export([ start_link/1 ]).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

%% gen_server -- start_link, called to start
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  {ok, PID} = gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []),
  {ok, PID}.


%% gen_server -- init, called after gen_server:start_link()
%% called once server started and registered process (ready to receive)
init(Params) -> 
  printout("~p, init().", [?FUNCTION_NAME]),
  %% get app ID and send self()
  AppID = ets:lookup(tpri,app_id),
  AppID ! {tpri, server_id, self()},
  %% start tpri_sup, pass Params
  tpri_sup:start_link(Params),
  %% tpri_sup setup -- children exchange PIDs
  tpri_sup:run_setup(),
  {ok, #state{}}.
