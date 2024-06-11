-compile({nowarn_unused_function, [ {printout,2},
                                    {printout,3} ]}).

%% @doc
printout(Str, Params) 
when is_list(Str) and is_list(Params) -> 
  io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

%% @doc
printout(Name, Str, Params) 
when is_atom(Name) and is_list(Str) and is_list(Params) -> 
  io:format("[~p|~p]: " ++ Str ++ "\n", [Name, self()] ++ Params);
  
printout(#{role:=#{name:=Name}}=_Data, Str, Params)
when is_map(_Data) and is_atom(Name) and is_list(Str) and is_list(Params) -> 
  printout(Name,Str,Params);
  
printout(List, Str, Params)
when is_list(List) and is_list(Str) and is_list(Params) ->
  printout(maps:from_list(List),Str,Params).

