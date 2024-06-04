-compile({nowarn_unused_function, [ {sup_flags,4},
                                    {default_sup_flags,0} ]}).


sup_flags(Strategy, Intensity, Period, AutoShutdown) -> 
  #{ strategy => Strategy,
     intensity => Intensity,
     period => Period,
     auto_shutdown => AutoShutdown }.

default_sup_flags() -> sup_flags(one_for_all, 1, 5, any_significant).

