-compile({nowarn_unused_function, [ {list_to_map_getter,2},
                                    {list_to_map_getter,3} ]}).

list_to_map_getter([], Map, _Default) -> Map;

list_to_map_getter([H|T]=List, Map, Default) 
when is_list(List) and is_map(Map) and is_atom(Default) ->
  Map1 = maps:get(H, Map, Default),
  list_to_map_getter(T, Map1, Default).

list_to_map_getter(List, Map) -> list_to_map_getter(List, Map, undefined).

