-compile({nowarn_unused_function, [ {show,1},
                                    {show,2} ]}).


show({Str, Args, #{options:=#{printout:=#{enabled:=true}}}=_Data}) -> printout(Str, Args);
show({Name, Str, Args, #{options:=#{printout:=#{enabled:=true}}}=_Data}) -> printout(Name, Str, Args);
show(_) -> ok.

show(verbose, {Str, Args, #{options:=#{printout:=#{enabled:=true,verbose:=true}}}=_Data}) -> printout(Str, Args);
show(verbose, {Name, Str, Args, #{options:=#{printout:=#{enabled:=true,verbose:=true}}}=_Data}) -> printout(Name, Str, Args);
show(verbose, _) -> ok.

