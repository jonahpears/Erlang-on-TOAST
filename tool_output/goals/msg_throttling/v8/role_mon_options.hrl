-compile({nowarn_unused_function, [ {options,0},
                                    {options,1},
                                    {default_options,0} ]}).

options() -> options([]).

options(List) -> list_to_map_builder(List, default_options()).

default_options() -> 
  #{ delayable_sends => 
          #{ enabled => false }, 
     printout => 
          #{ enabled => true, 
             verbose => false },
     queue => 
          #{ enabled => false, 
             flush_after_recv => 
                  #{ enabled => false, 
                     after_any => false, 
                     after_labels => [] },
             aging =>
                  #{ enabled => false,
                     max_age => -1 } %% ignore local ages too
            },
     forward_receptions => 
          #{ enabled => false, 
             to => undefined, 
             any => false, 
             labels => [] },
      support_auto_label => %% only for sending actions
          #{ enabled => false }
    }.

