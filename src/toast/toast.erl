-module(toast).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).

%% used when generating constants, constraints and time delays
-define(CONSTANT_UPPER_BOUND,9).
-define(CONSTANT_LOWER_BOUND,9).
-define(CONSTRAINT_UPPER_BOUND, 5).
-define(CONSTRAINT_LOWER_BOUND, 2).
-define(DELAY_UPPER_BOUND,5.0).

%% for random tests, how many to run to be sure
-define(RAND_TEST_NUM,5). %% ! setting low for testing

-type process () :: toast_process:toast_process().
-type type () :: toast_type:toast_type().
-type delta () :: toast_type:constraints().


%% @doc wrapper function for type-checking a type and process 
check(Type,Process) ->
  io:format("\n\n= = = = = = =\n\nchecking,\n\ntype:\t~p,\n\nprocess:\t~p.\n",[Type,Process]),

  Result = checker:type_check(Type,Process),

  io:format("\n\n- - - - - - -\n\nResult:\t~p.",[Result]),

  ok.
%%

%% @doc wrapper function for deriving toast_type from string
type(String)
when is_list(String) -> 
  toast_type:parse_toast(wrapper, String).
%%


%% TODO :: sample(constraints) -- diagonal constraints


%% @doc helper function for wrapping a toast type in list of interactions.
%% if necessary, unfolds recursive types.
%% choice are just a list of interactions anyway.
%% single interactions put in list.
%% only fails for 'end' or {'call', ...}
-spec interactions(type()) -> [toast_type:interact_type()]|error.

%% @doc unfolds recursive definition type
interactions({'def', _Name, S}=_Type) -> interactions(S);

%% @doc wraps single interact type in list
interactions({_Direction, _Msg, _Constraints, _Resets, _S}=Type) -> [Type];

%% @doc returns choice type as is, a list of interactions
interactions([_Interaction|_T]=Type) -> Type;

%% @doc unhandled (invalid) type
interactions(_Type) -> error.


%% @doc helper functions to provide sample labels, clocks, dbc, types
-spec sample(atom()|{atom(),list()}|{atom(),{list(),any()}}) -> list().

sample(labels) -> [a,b,c,d,e,f,g];
sample(clocks) -> [u,v,w,x,y,z];
sample(types) -> [interact,choice,def];
sample(dbc) -> ['gtr','eq'];
sample(dbc_extended) -> sample(dbc)++['geq','les','leq','neq'];

%% @doc sample list of clock valuations
sample({clocks_valuations,{Clocks,Bounds}})
when is_map(Bounds) -> 
  %% unpack bounds
  Lower = maps:get(lower,Bounds,?CONSTRAINT_LOWER_BOUND),
  Upper = maps:get(upper,Bounds,lists:max([Lower+1,?CONSTRAINT_UPPER_BOUND])),
  Global = maps:get(global,Bounds,Upper),
  %% for each clock, set values
  Result = lists:foldl(fun(Clock, In) ->
    %% check if any specific bounds specified
    Value = case maps:get(Clock,Bounds,undefined) of 

      %% no specific bounds given, use Lower and Upper
      undefined -> rand_float_from_range(Lower,Upper);

      %% specific value requested
      #{value:=Val} -> Val;

      %% both bounds
      #{lower:=LowerVal,upper:=UpperVal} -> rand_float_from_range(LowerVal,UpperVal);

      %% upper bounds
      #{upper:=UpperVal} -> rand_float_from_range(Lower,UpperVal);

      %% lower bounds
      #{lower:=LowerVal} -> rand_float_from_range(LowerVal,Upper);

      %% unknown
      _ -> rand_float_from_range(Lower,Upper)

    end,
    %% add to list
    In++[{Clock,Value}]
  end, [], Clocks) ++ [{global,Global}],
  %% return
  Result;
%%

%% @doc sample select random from list
sample({rand_from_list,List}) -> lists:nth(rand:uniform(length(List)),List);

%% @doc sample make list of random things
%% Num is the desired size of the returned list
sample({list_of_rand,{List,Num,IsUnique}}) 
when is_boolean(IsUnique) -> 
  {Result,_Input} = lists:foldl(fun(_, {In,_L}) ->
    %% select random from List (via _L)
    Selected = lists:nth(rand:uniform(length(_L)),_L),
    %% if list needs to be unique, remove option from List
    case IsUnique of 
      true -> NewList = lists:delete(Selected,_L);
      _ -> NewList = _L
    end,
    %% return
    {In++[Selected],NewList}
  end, {[],List}, lists:duplicate(Num,undefined)),
  %% return
  Result;
%%

sample(_) -> undefined.


%% @doc helper functions to provide sample types or constraints
-spec sample(atom(),map()) -> type()|delta().

%% @doc sample continuation (as defined in options)
sample(continuation,Options) ->
  %% check if continuation is specified, else use end
  case maps:get(has_continuation,Options,no_continuation) of 

    %% no continuation, is end
    no_continuation -> maps:get(continuation,Options,'end');

    [Next|T] -> 
      %% has continuation, check if first is defined
      Continuation = case Next of 

        %% undefined continuation, select from random valid ones
        undefined -> sample({rand_from_list,sample(type)});

        %% specific continuation specified
        _ -> Next
          
      end,
      %% check if this is the last continuation
      case length(T)>0 of 

        %% this is not the last 
        true -> sample(Continuation,maps:put(has_continuation,T,Options));

        %% get continuatino, but no more after
        false -> sample(Continuation,maps:put(has_continuation,false,Options))

      end;

      %% unrecognised continuation, use end but warn
      Err -> 
        io:format("\n\nWarning, ~p, unrecognised continuation: ~p.\n\tUsing 'end' instead.\n(Options:\t~p.)\n",[Err,Options]),
        'end'
  end;
%%

%% @doc sample constraint
sample(constraints,Options) ->
  %% unpack constraint options
  Spec = maps:get(constraint_spec,Options,#{}),
  Lower = maps:get(lower,Spec,?CONSTANT_LOWER_BOUND),
  Upper = maps:get(upper,Spec,?CONSTANT_UPPER_BOUND),
  %% check if has constraints
  Constraints = case maps:get(has_constraints,Options,no_constraints) of 

    %% no constraints, use true
    no_constraints -> 'true';

    %% make half-random constraints
    {mode,Mode} -> 
      %% extract from args
      Kind = maps:get(kind,Spec,simple),
      Num = maps:get(num,Spec,1),
      Constants = maps:get(constants,Spec,#{}),

      %% depending on kind of sample requested (diagonal or simple)
      case Kind of
        
        %% diagonal constraints
        diagonal -> 

          %% TODO :: expand on this

          io:format("\n\nWarning, selected 'diagonal' constraints, but this is not currently implemented.\nSetting constraint to 'true' instead."),
          'true';

        %% simple 
        _ -> 
          %% get list of unique clocks 
          Clocks = maps:get(clocks,Spec,sample({list_of_rand,{sample(clocks),Num,true}})),
          %% get list of non-unique DBC operators
          DBCs = maps:get(dbcs,Spec,sample({list_of_rand,{sample(dbc),Num,false}})),
    
          %% depending on mode (random or half-random)
          case Mode of 

            %% make random where defaults are not provided
            half_random -> 
              %% for each clock, construct constraint
              lists:foldl(fun(Clock, {Conj, [DBC|T],In}) ->
                %% get clock spec (if any)
                ClockSpec = maps:get(Clock,Constants,undefined),
                %% get value of constant (either rand from default range, or specific to clock)
                {Constant, Negated} = case ClockSpec of 
                  
                  %% use random from range of defaults
                  undefined -> {rand_int_from_range(Lower,Upper), false};

                  %% value for constant provided
                  #{constant:=Val} -> {Val, maps:get(negated,ClockSpec,false)};
                
                  %% lower bound provided 
                  #{lower:=LowerVal} -> {rand_int_from_range(LowerVal,Upper), maps:get(negated,ClockSpec,false)};
                
                  %% lower bound provided 
                  #{upper:=UpperVal} -> {rand_int_from_range(Lower,UpperVal), maps:get(negated,ClockSpec,false)};
                
                  %% unknown
                  _ -> {rand_int_from_range(Lower,Upper), false}

                end,
                %% create new constraint
                _New = {Clock, DBC, Constant},
                New = case Negated of 
                  true -> {'neg', _New};
                  _ -> _New
                end,
                %% if conj with existing (building tree)
                NextIn = case Conj of 
                  true -> {In, 'and', New};
                  _ -> New
                end,
                %% return
                {T, NextIn}
              end, {false, DBCs,'true'}, Clocks);

            %% make random constraint with random clock, DBC and constant
            random ->
              %% get non-unique list of DBC operators
              DBCs = sample({list_of_rand,{sample(dbc),Num,false}}),
              %% for each DBC, construct random constraint
              lists:foldl(fun(DBC, In) ->
                %% create new constraint
                New = {sample({rand_from_list,sample(clocks)}), DBC, rand:uniform(?CONSTRAINT_UPPER_BOUND)},
                %% conj with existing (building tree)
                {In, 'and', New}
              end, 'true', DBCs);

            %% unexpected
            _Err -> 
              io:format("\n\nWarning, ~p, unexpected Mode: '~p', returning 'true'.\n\tOptions: ~p.\n",[?FUNCTION_NAME,Mode,Options]),
              'true'

          end

      end;

    %% has constraints, return them
    _Constraints -> _Constraints
      
  end,
  %% return constraints
  Constraints;
%%

%% @doc sample choice type
sample(choice,Options) ->
  %% unpack num of interactions
  Num = maps:get(choice_size,Options,1),
  %% get choice spec, just assign this option if none is provided
  ChoiceSpec = maps:get(choice_spec,Options,list:duplicate(Num,Options)),
  %% create list of size Num, containing fresh 
  Result = lists:foldl(fun(Spec, In) ->
    In++[sample(interact,Spec)]
  end, [], ChoiceSpec),
  %% return
  Result;
%%

%% @doc random sample interact type
sample(interact,#{mode:=random}=Options) ->

  %% randomise direction (50/50 send/recv)
  Direction = case rand:uniform()<0.5 of true -> send; _ -> recv end,

  %% set random message label
  Labels = sample(labels),
  Msg = {lists:nth(rand:uniform(length(Labels)), Labels), none},

  %% constraints
  Constraints = sample(constraints,Options),

  %% resets
  Clocks = sample(clocks),
  Resets = sample({list_of_rand,{Clocks,rand:uniform(length(Clocks)-1),true}}),
  
  %% check continuation
  S = sample(continuation,Options),

  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc half-random sample interact type
sample(interact,#{mode:=half_random}=Options) ->

  %% if not defined, randomise direction (50/50 send/recv)
  Direction = maps:get(direction,Options,case rand:uniform()<0.5 of true -> send; _ -> recv end),

  %% if not defined, set random message label
  Labels = sample(labels),
  Msg = maps:get(msg,Options,{lists:nth(rand:uniform(length(Labels)), Labels), none}),

  %% constraints
  Constraints = maps:get(constraints,Options,sample(constraints,Options)),

  %% resets
  Clocks = sample(clocks),
  Resets = maps:get(resets,Options,sample({list_of_rand,{Clocks,rand:uniform(length(Clocks)-1),true}})),

  %% check continuation
  S = sample(continuation,Options),

  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc non-random sample interact type
sample(interact,Options) ->
  %% unpack
  Direction = maps:get(direction,Options,send),
  Msg = maps:get(msg,Options,{m,none}),
  Constraints = maps:get(constraints,Options,true),
  Resets = maps:get(resets,Options,[]),

  %% check continuation
  S = sample(continuation,Options),
  
  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc unsupported type
sample(Unsupported,_Map) -> 
  io:format("\n\nError, unsupported type: ~p.",[Unsupported]),
  %% return end type
  'end'.
%%



%% @doc suite of tests
tests() -> tests(all).

%% @doc contains all test names
get_all_tests() -> [t_reading,from_string].

%% @doc tests for t-reading
tests(t_reading) -> [{send_gtr,[true,true]},{send_leq,[true,true]},{recv_gtr,[false,true]},{recv_leq,[true,false]},{rec_loop,[true,true]}];

%% @doc tests for extracting types from string
tests(from_string) -> [{send,1},{recv,1},{rec_loop,1}];

%% @doc run all tests (requires test name to be stored in get_all_tests/0)
tests(all) ->
  %% run each test
  {Num, Result} = lists:foldl(fun(Test, {Count,Pass}) ->
    %% if test returns ok, then pass
    case run_tests(Test,tests(Test)) of 
      true -> {Count+1, Pass and true};
      _ -> {Count, false}
    end
  end, {0,true}, get_all_tests()),
  %% check if passed?
  case Result of 
    true -> 
      io:format("\n\nAll tests (~p) passed!.\n",[Num]);
    _ -> 
      io:format("\n\nWarning, only (~p) tests passed.\n",[Num])
  end;

tests(_) -> tests(all).


%% @doc runs different kinds of test suits
-spec run_tests(atom(),[{atom(),list()}]|{list(),integer()}|[{atom(),integer()}]) -> boolean().

%% @doc runs test for given suite with tests that use the index to provide different expected results
%% Names is a list of tuples: {Name,Expected} where Expected is a list of values expected to return, given their own index
run_tests(Kind,[{_Name,_Expected}|_T]=Names)
when is_list(Names) and is_atom(_Name) and is_list(_Expected) -> 
  %% to map
  Map = maps:from_list(Names),
  %% for each in map, run and test each
  Results = maps:fold(fun(Name, Expected, InResults) ->
    %% for each expected
    TestResults = lists:foldl(fun(ExpectedResult, {Index, InTest}) ->
      CurrentTest = test(Kind,Name,Index),
      %% return whether it was expected
      {Index+1, InTest++[CurrentTest=:=ExpectedResult]}
    end, {1,[]},Expected),
    InResults++TestResults
  end, [], Map),
  %% return
  Result = lists:all(fun(R) -> (R=:=true) end, Results),
  Result;
%%

%% @doc runs test for given basic suite (these kinds of tests tail-loop)
run_tests(Kind,{Name,Index})
when is_atom(Name) and is_integer(Index) -> test(Kind,Name,Index). % and run_tests(Kind,{Name,Index-1}).



%% @doc individual tests
-spec test(atom(), atom(), integer()) -> boolean().

%% @doc catch for returning once index has reached 0
test(_Kind,_Name,0) -> true;


%% @doc t_reading tests, send_gtr
test(t_reading=_Name, send_gtr=_Kind, Index) ->
  %% get type and delay
  Type = sample(interact,#{
    mode=>half_random,
    direction=>send,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negation=>false}},
      dbcs=>['gtr']
    }}),
  io:format("\n\n(~p:~p/~p) Type: ~p.\n\n",[_Name,_Kind,Index,Type]),
  T = rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample({clocks_valuations,{sample(clocks),#{x=>#{value=>4}}}}),
  %% ask z3
  Result = checker:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  %% return (and call next iteration)
  Result;
%%

%% @doc t_reading tests, send_leq
test(t_reading=_Name, send_leq=_Kind, Index) ->
  %% get type and delay
  Type = sample(interact,#{
    mode=>half_random,
    direction=>send,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negation=>true}},
      dbcs=>['gtr']
    }}),
  io:format("\n\n(~p:~p/~p) Type: ~p.\n\n",[_Name,_Kind,Index,Type]),
  T = rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample({clocks_valuations,{sample(clocks),#{x=>#{value=>4}}}}),
  %% ask z3
  Result = checker:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  %% return (and call next iteration)
  Result;
%%

%% @doc t_reading tests, recv_gtr
test(t_reading=_Name, recv_gtr=_Kind, Index) ->
  %% get type and delay
  Type = sample(interact,#{
    mode=>half_random,
    direction=>recv,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negation=>false}},
      dbcs=>['gtr']
    }}),
  io:format("\n\n(~p:~p/~p) Type: ~p.\n\n",[_Name,_Kind,Index,Type]),
  T = rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample({clocks_valuations,{sample(clocks),#{x=>#{value=>4}}}}),
  %% ask z3
  Result = checker:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  %% return (and call next iteration)
  Result;
%%

%% @doc t_reading tests, recv_leq
test(t_reading=_Name, recv_leq=_Kind, Index) ->
  %% get type and delay
  Type = sample(interact,#{
    mode=>half_random,
    direction=>recv,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negation=>true}},
      dbcs=>['gtr']
    }}),
  io:format("\n\n(~p:~p/~p) Type: ~p.\n\n",[_Name,_Kind,Index,Type]),
  T = rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample({clocks_valuations,{sample(clocks),#{x=>#{value=>4}}}}),
  %% ask z3
  Result = checker:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  %% return (and call next iteration)
  Result;
%%



%% @doc send string extraction tests
test(from_string=Name, send=Kind, Index) ->
  Type = type("!a(x>5).end"),
  %% result is if they are equal
  Result = (Type=:={send, {a, none}, {"x",'gtr', 5}, [], 'end'}),
  %% return (and call next iteration)
  Result and test(Name,Kind,Index-1);
%%

%% @doc recv string extraction tests
test(from_string=Name, recv=Kind, Index) ->
  Type = type("?a(x>5).end"),
  %% result is if they are equal
  Result = (Type=:={recv, {a, none}, {"x",'gtr', 5}, [], 'end'}),
  %% return (and call next iteration)
  Result and test(Name,Kind,Index-1);
%%

%% @doc rec loop string extraction tests
test(from_string=Name, rec_loop=Kind, Index) ->
  Type = type("def(a).?a(x>5).call(a)"),
  %% result is if they are equal
  Result = (Type=:={'def', "a", {recv, {a, none}, {"x",'gtr', 5}, [], {'call',"a"}}}),
  %% return (and call next iteration)
  Result and test(Name,Kind,Index-1);
%%

%% @doc unknown test
test(_Kind,_Name,_Index) -> 
  io:format("\n\nWarning, unknown test called: (~p:~p)/~p.",[_Kind,_Name,_Index]),
  false.
%%



%% @doc returns random int in range Lower <-> Upper
%% @see rand_float_from_range/2
rand_int_from_range(Lower,Upper) -> 
  %% call for float, then floor
  Result = rand_float_from_range(Lower,Upper),
  %% return
  floor(Result).
%%

%% @doc returns random float in range Lower <-> Upper
rand_float_from_range(Lower,Upper) -> 
  %% if same, return
  Result = case (Lower=:=Upper) of 
    true -> Lower;
    
    %% pick random from range
    _ -> lists:min([Lower+rand:uniform(Upper+1)-1,Upper])

  end,
  %% return
  Result.
%%
