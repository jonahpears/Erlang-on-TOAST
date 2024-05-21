-compile({nowarn_unused_function, [ {sup_flags,3},
                                    {sup_flags,0} ]}).


sup_flags(Strategy, Intensity, Period) -> 
  #{ strategy => Strategy,
     intensity => Intensity,
     period => Period }.

sup_flags() -> sup_flags(one_for_all, 1, 5).
