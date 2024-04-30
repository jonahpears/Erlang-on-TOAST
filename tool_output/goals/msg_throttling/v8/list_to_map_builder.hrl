-compile({nowarn_unused_function, [ {list_to_map_builder,1},
                                    {list_to_map_builder,2} ]}).

list_to_map_builder([], Default) -> Default;

list_to_map_builder([{Key,Val}|T]=List, Default) ->
  %% check tail does not contain an option with the same key
  %% this will end up being overwritten by this key
  case lists:member(Key, maps:keys(maps:from_list(T))) of
   true -> io:format("~p, ~p: key ~p appears twice in given params!\n", [?MODULE,?FUNCTION_NAME, Key]);
   _ -> ok
  end,
  %% build tail first (from Default_
  Tail = list_to_map_builder(T,Default),
  case lists:member(Key, maps:keys(Tail)) of
    true -> Map = maps:put(Key, Val, Tail);
    _ -> 
      io:format("~p, ~p: key was unexpected: ~p.\n", [?MODULE,?FUNCTION_NAME, Key]), 
      Map = Tail
  end,
  io:format("\n~p, ~p,\n\tlist: ~p,\n\tmap: ~p.\n", [?MODULE,?FUNCTION_NAME,List,Map]),
  Map;

list_to_map_builder([H|T],Default) ->
  io:format("~p, ~p: head param was unexpected: ~p.\n", [?MODULE,?FUNCTION_NAME, H]),
  list_to_map_builder(T,Default).

list_to_map_builder(Default) -> list_to_map_builder([], Default).

