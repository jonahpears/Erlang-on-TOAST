-compile({nowarn_unused_function, [ {ets_get,2}, {ets_string,1} ]}).

ets_get(Table, Key) -> 
  Vals = ets:lookup(Table, Key),
  {_, Vals1} = lists:nth(1, Vals),
  Vals1.

ets_string(Table) -> ets:match_object(Table, {'$0', '$1'}).
