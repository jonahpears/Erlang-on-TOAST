-compile({nowarn_unused_function, [ {options,0},
                                    {options,1},
                                    {default_options,0},
                                    {default_options,1} ]}).

options() -> options([]).

options(List) -> list_to_map_builder(List, default_options()).

default_options() -> 
  #{ delayable_sends => default_options(delayable_sends),
     printout => default_options(printout),
     queue => default_options(queue),
     forward_receptions => default_options(forward_receptions),
     support_auto_label => default_options(support_auto_label) %% only relevant to sending actions
    }.


default_options(delayable_sends) -> #{ enabled => false };
default_options(printout) -> #{ enabled => true, verbose => false };
default_options(queue) -> 
  #{ enabled => false, 
     flush_after_recv => #{ enabled => false, 
                            after_any => false, 
                            after_labels => [] },
     aging => #{ enabled => false,
                 max_age => -1 } %% ignore local ages too
    };
default_options(forward_receptions) -> #{ enabled => false, to => undefined, any => false, labels => [] };
default_options(support_auto_label) -> #{ enabled => false }.

