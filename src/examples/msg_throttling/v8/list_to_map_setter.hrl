-compile({nowarn_unused_function, [ {list_to_map_setter,2},
                                    {list_to_map_setter,3} ]}).

list_to_map_setter({Key, Val}, [], Map) -> maps:put(Key, Val, Map);

list_to_map_setter({Key, Val}, [H|T]=List, Map) 
when is_list(List) and is_map(Map) and is_atom(Key) and is_atom(H) ->
  Map1 = maps:get(H,Map),
  Map2 = list_to_map_setter({Key,Val}, T, Map1),
  maps:put(H,Map2).

list_to_map_setter({Key, Val}, Map) when is_map(Map) -> list_to_map_setter({Key, Val}, [], Map).

