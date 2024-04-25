-compile({nowarn_unused_function, [ {printout,2},
                                    {printout,3} ]}).

printout(Str, Params) 
when is_list(Str) -> 
  io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).
printout(Name, Str, Params) 
when is_atom(Name) and is_list(Str) -> 
  io:format("[~p|~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).
