-module(tpri_app).
-file("tpri_app.erl", 1).
-behaviour(application).

-export([start/2, stop/1]).

-export([ start/1,
          get_env/1
        ]).

get_env(Key) -> 
  case application:get_env(?MODULE, Key) of
    {ok, Val} -> {ok, Val};
    undefined -> undefined
  end.


start(Args) -> start(normal, Args).
start(_Type, Args) -> 
  {ok, TPRIServerID} = tpri_server:start_link(Args),

  TPRIServerID ! {self(), tpri_app, get_worker_id, msger},
  receive {TPRIServerID, worker_id, MsgerID} -> ok end,

  TPRIServerID ! {self(), tpri_app, get_worker_id, acker},
  receive {TPRIServerID, worker_id, AckerID} -> ok end,

  

  {ok, TPRIServerID}.

stop(_State) -> ok.



