-compile({nowarn_unused_function, [ {nested_map_merger,2} ]}).

nested_map_merger(Map1, Map2) -> 
  NestedMapCombinator = fun (_Key, Val1, Val2) ->
    case is_map(Val1) of 
      true -> nested_map_merger(Val1, Val2);
      _ -> Val2
    end
  end,
  maps:merge_with(NestedMapCombinator, Map1, Map2).


