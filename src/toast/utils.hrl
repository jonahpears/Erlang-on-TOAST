-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).


-define(DEFAULT_SHOW_TRACE,true).
-define(WORKER_SHOW_TRACE,false).

%% how fine grained to check decimals
-define(DECIMAL_PRECISION,1).

-define(INFINITY,10).


%% @doc type conversion functions
to_float(N) when is_integer(N) -> list_to_float(integer_to_list(N)++".0");
% to_float(N) when is_list(N) -> 
to_float(N) when is_binary(N) -> binary_to_float(N);
to_float(N) when is_float(N) -> N.

to_integer(N) when is_float(N) -> round(N);
% to_integer(N) when is_list(N) -> 
to_integer(N) when is_binary(N) -> binary_to_integer(N);
to_integer(N) when is_integer(N) -> N.

to_binary(N) when is_float(N) -> integer_to_binary(N);
to_binary(N) when is_list(N) -> list_to_binary(N);
to_binary(N) when is_integer(N) -> float_to_binary(N);
to_binary(N) when is_binary(N) -> N.

to_string(N) when is_float(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_integer(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_binary(N) -> lists:flatten(io_lib:format("~w",[N]));
to_string(N) when is_list(N) -> lists:flatten(N).



%% @doc increments all Timers by T
%% @see increment_clocks/2
increment_timers(Timers,T)
when is_map(Timers) ->
  Timers1 = maps:fold(fun(Name,Val,In) ->
    maps:put(Name,Val+T,In)
  end, #{}, Timers),
  %% return
  Timers1.
%%

%% @doc increments all Clocks by T
%% @see increment_timers/2
increment_clocks(Clocks,T)
when is_map(Clocks) ->
  Clocks1 = maps:fold(fun(Name,Val,In) ->
    maps:put(Name,Val+T,In)
  end, #{}, Clocks),
  %% return
  Clocks1.
%%

%% @doc makes sure each clock in List has a value in the returned Clocks.
%% if no value is found, then they are assigned value of 'global' clock
%% if no 'global' clock is found, then warning printed and 'global' added as highest value clock, which missing clocks are subsequently given the same valuation
instansiate_clocks(Clocks,List)
when is_map(Clocks) and is_list(List) ->
  %% check if 'global' in clocks
  case is_map_key('global',Clocks) of 
    %% global present, do nothing
    true -> Max = maps:get('global',Clocks), Clocks1 = Clocks;
    %% need to add global, find maximal clock
    _ -> 
      Max = lists:max(maps:fold(fun(_Clock,Val,InVals) -> InVals++[Val] end, [], Clocks)),
      Clocks1 = maps:put('global',Max,Clocks)
  end,
  %% for each in List, set value to Max if not set already
  Clocks2 = lists:foldl(fun(Clock, InMap) ->
    case is_map_key(Clock,Clocks1) of 
      true -> InMap;
      _ -> maps:put(Clock,Max,Clocks1)
    end
  end, Clocks1, List),
  %% return
  Clocks2;
%%

%% @doc helper function to convert Clocks to map and back to list.
instansiate_clocks(Clocks,List)
when is_list(Clocks) and is_list(List) ->
  Clocks1 = instansiate_clocks(maps:from_list(Clocks),List),
  %% retrurn as list
  maps:to_list(Clocks1).
%%

%% @doc 
minimal_precision() -> 1.0/(list_to_integer(lists:flatten("1"++lists:duplicate(?DECIMAL_PRECISION,"0")))).


%% @doc rounds to DECIMAL_PRECISION decimal places
precision_rounding(Float) 
when is_float(Float) -> 
  Offset = math:pow(10.0,?DECIMAL_PRECISION),
  round(Float*Offset)/Offset.
%%
