-compile({nowarn_unused_function, [ {list_to_map_builder,1},
                                    {list_to_map_builder,2} ]}).

list_to_map_builder([], Default) -> Default;

list_to_map_builder([{Key,Val}|T], Default) ->
  %% check tail does not contain an option with the same key
  %% this will end up being overwritten by this key
  case lists:member(Key, maps:keys(maps:from_list(T))) of
   true -> io:format("~p, ~p: key ~p appears twice in given params!\n", [?MODULE,?FUNCTION_NAME, Key]);
   _ -> ok
  end,
  %% build tail first (from default_options
  Tail = list_to_map_builder(T,Default),
  case lists:member(Key, maps:keys(Tail)) of
    true -> Options = maps:put(Key, Val, Tail);
    _ -> 
      io:format("~p, ~p: key was unexpected: ~p.\n", [?MODULE,?FUNCTION_NAME, Key]), 
      Options = Tail
  end,
  Options;

list_to_map_builder([H|T],Default) ->
  io:format("~p, ~p: head param was unexpected: ~p.\n", [?MODULE,?FUNCTION_NAME, H]),
  list_to_map_builder(T,Default).

list_to_map_builder(Default) -> list_to_map_builder([], Default).

