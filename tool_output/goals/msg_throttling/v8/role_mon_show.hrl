-compile({nowarn_unused_function, [ {show,1},
                                    {show,2},
                                    {show,5} ]}).


show({Str, Args, #{options:=#{printout:=#{enabled:=true}}}=_Data}) -> printout(Str, Args);

show({Name, Str, Args, #{options:=#{printout:=#{enabled:=true}}}=_Data}) -> printout(Name, Str, Args);

show(_) -> ok.



show(verbose, {Str, Args, #{options:=#{printout:=#{enabled:=true,verbose:=true}}}=_Data}) -> printout(Str, Args);

show(verbose, {Name, Str, Args, #{options:=#{printout:=#{enabled:=true,verbose:=true}}}=_Data}) -> printout(Name, Str, Args);

show(verbose, _) -> ok.



show(verbose, {Str1, Args1}, else, {Str2, Args2}, {#{options:=#{printout:=#{enabled:=true,verbose:=Verbose}}}=_Data}) -> 
  case Verbose of
    true -> printout(Str1, Args1);
    _ -> printout(Str2, Args2)
  end;

show(verbose, {Str1, Args1}, else, {Str2, Args2}, {Name, #{options:=#{printout:=#{enabled:=true,verbose:=Verbose}}}=_Data}) -> 
  case Verbose of
    true -> printout(Name, Str1, Args1);
    _ -> printout(Name, Str2, Args2)
  end;

show(verbose, _, else, _, _) -> ok.

