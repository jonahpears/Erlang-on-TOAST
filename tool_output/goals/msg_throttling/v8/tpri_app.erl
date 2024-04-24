-module(tpri_app).
-file("tpri_app.erl", 1).
-behaviour(application).

-export([start/2, stop/1]).

-export([ start/1,
          get_env/1
        ]).

-define(WORKER_NUM, 2).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

get_env(Key) -> 
  case application:get_env(?MODULE, Key) of
    {ok, Val} -> {ok, Val};
    undefined -> undefined
  end.


start(Args) -> start(normal, Args).
start(Type, Args) -> 
  printout("~p, type=~p.", [?FUNCTION_NAME, Type]),

  ets:new(tpri, [set,named_table,protected,{keyPos, 1}]),
  ets:insert(tpri, {app_id, self()}),
  printout("~p, created ets, added self()=~p.", [?FUNCTION_NAME, self()]),

  {ok, TPRIServerID} = tpri_server:start_link(Args),

  printout("~p, beginning ets setup.", [?FUNCTION_NAME]),
  finish_ets_setup(),
  printout("~p, finished ets setup.", [?FUNCTION_NAME]),

  {ok, TPRIServerID}.

stop(_State) -> ok.


%% @doc continually wait to receive message from children, adding them to ets
%% then check if all are accounted for before returning
finish_ets_setup() ->
  printout("~p, waiting.", [?FUNCTION_NAME]),
  receive
    {tpri, server_id, ServerID} = Msg -> 
      ets:insert(tpri, {tpri_server_id, ServerID});
    {tpri, sup_id, SupID} = Msg -> 
      ets:insert(tpri, {tpri_sup_id, SupID});
    {trm, sup_id, SupID} = Msg -> 
      ets:insert(tpri, {trm_sup_id, SupID});
    {imp, sup_id, SupID} = Msg -> 
      ets:insert(tpri, {imp_sup_id, SupID});
    {trm, worker_id, WorkerName, WorkerID} = Msg -> 
      Key = list_to_atom("trm_" ++ atom_to_list(WorkerName) ++ "_id"),
      ets:insert(tpri, {Key, WorkerID});
    {imp, worker_id, WorkerName, WorkerID} = Msg -> 
      Key = list_to_atom("imp_" ++ atom_to_list(WorkerName) ++ "_id"),
      ets:insert(tpri, {Key, WorkerID})
  end,
  printout("~p, received: ~p.", [?FUNCTION_NAME, Msg]),
  case check_ets_finished() of
    false -> finish_ets_setup();
    _ -> finish_ets_setup()
  end.


check_ets_finished() ->
  Keys = [ tpri_server_id,
           tpri_sup_id,
           trm_sup_id,
           imp_sup_id ],
  %% count if children are finished
  TrmCount = 0,
  ImpCount = 0,
  %% go through all entries in table
  %% for any with the prexif "trm_" or "imp_" we count these as successes, and just check their number is as expected
  KeyMatches = ets:foldl(fun({TKey, _TVal}) ->
      case string:find(atom_to_list(TKey), "trm_") of
        nomatch -> 
          case string:find(atom_to_list(TKey), "imp_") of
            %% check if it is part of Keys
            nomatch -> lists:member(TKey, Keys);
            _ -> 
              ImpCount = ImpCount + 1,
              true
          end;
        _ -> 
          TrmCount = TrmCount + 1,
          true
      end
    end, [], tpri),
    %% check each key is accounted for 
    KeyResults = lists:foldl(fun(Key) -> lists:member(Key, KeyMatches) end, [], Keys),
    IsFinished = not lists:filter(fun(Elem) -> Elem==false end, KeyResults),
    %% 
    IsFinished and (TrmCount==?WORKER_NUM) and (ImpCount==?WORKER_NUM).

