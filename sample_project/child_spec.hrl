-compile({nowarn_unused_function, [ {child_spec,4} ]}).

child_spec(Module, ID, #child_options{ restart = Restart, shutdown = Shutdown, type = Type, significant = Significant }, ChildParams) ->
  #{ id => ID,
     start => {Module, start_link, [ChildParams]},
     restart => Restart,
     shutdown => Shutdown,
     type => Type,
     significant => Significant,
     modules => [Module] }.
