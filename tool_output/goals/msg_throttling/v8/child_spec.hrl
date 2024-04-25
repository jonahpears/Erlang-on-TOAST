-compile({nowarn_unused_function, [ {child_spec,4} ]}).

child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type }, ChildParams) ->
  #{ id => ID,
     start => {Module, start_link, [[{name,ID}] ++ ChildParams]},
     restart => Restart,
     shutdown => Shutdown,
     type => Type,
     modules => [Module] }.
