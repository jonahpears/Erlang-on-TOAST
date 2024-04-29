-module(tpri_server).
-file("tpri_server.erl", 1).
-behaviour(gen_server).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

%% gen_server API
-export([ start_link/0,
          init/1,
          stop/0,
          handle_call/3,
          handle_cast/2
          % handle_info/2
         ]).

-export([ start_link/1 ]).

-record(state, {}).

-include("printout.hrl").

%% gen_server -- start_link, called to start
start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),
  {ok, PID} = gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []),
  {ok, PID}.

stop() -> exit(whereis(?MODULE), shutdown).

%% gen_server -- init, called after gen_server:start_link()
%% called once server started and registered process (ready to receive)
init(Params) -> 
  printout("~p.", [?FUNCTION_NAME]),
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  % printout("~p, lookup AppID: ~p.", [?FUNCTION_NAME, AppID]),
  AppID ! {tpri, server_id, self()},

  {ok, _SupID} = tpri_sup:start_link(Params),

  {ok, #state{}}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call(_Request, _From, State) -> 
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) -> 
  {noreply, State}.

