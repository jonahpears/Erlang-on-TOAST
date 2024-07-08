-module(toast).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).


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
%% TODO :: (in toast_type) finish extracting constraints from string


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
interactions(_Type) -> 
  io:format("\n\n(~p, ~p) Warning, unhanded type: ~p.\n",[?LINE,?FUNCTION_NAME,_Type]), 
  error.
%%


%% @doc current hot-test
hot_test() -> type_checking.
test() -> run_tests(hot_test(),tests(hot_test())).

%% @doc suite of tests
tests() -> tests(all).

%% @doc contains all test names
get_all_tests() -> [t_reading,not_t_reading,type_checking]. %  from_string
% get_all_tests() -> [type_checking].%[t_reading,not_t_reading]. % from_string

%% @doc tests for type-checking
tests(type_checking) -> [
                         {del_t_pass_send,[true]},
                         {del_t_fail_send,[false]},
                         {del_t_recv,[false,true,false]},
                         {del_t_branch,[false,true,false]},
                         {del_t_branch_cascade_simple,[true]},
                         {del_t_branch_cascade,[true]},
                         {del_delta_send,[true]},
                         {recv,[true,false]},
                         {if_timer,[true,false]},
                         {recursion_good,[true]}
                         ];

%% @doc tests for t-reading
tests(t_reading) -> [{send_gtr,[false,false]},
                     {send_leq,[false,false]},
                     {recv_gtr,[true,false]},
                     {recv_leq,[true,false]}];
tests(not_t_reading) -> lists:foldl(fun({Kind,Expected}, In) -> In++[{Kind,lists:foldl(fun(Result, InBools) -> InBools++[not Result] end, [], Expected)}] end, [], tests(t_reading));

%% @doc tests for extracting types from string
tests(from_string) -> [{send,1},{recv,1},{rec_loop,1}];

%% @doc run all tests (requires test name to be stored in get_all_tests/0)
tests(all) ->
  %% run each test
  {Num, Result} = lists:foldl(fun(Test, {Count,Pass}) ->
    io:format("\n\n= = = = = = = = = = = = = = = =\n\ntest suite: ~p...\n",[Test]),
    TestResults = run_tests(Test,tests(Test)),
    io:format("\ntest suite: ~p, results: ~p.\n",[Test,TestResults]),
    %% if test returns ok, then pass
    case TestResults of 
      true -> {Count+1, Pass and true};
      _ -> {Count, false}
    end
  end, {0,true}, get_all_tests()),
  io:format("\n\n= = = = = = = = = = = = = = = ="),
  %% check if passed?
  case Result of 
    true -> 
      io:format("\n\nAll (~p) test suites passed!.\n",[Num]);
    _ -> 
      case Num of 
        0 -> io:format("\n\nFailure, no test suites passed.\n");
        _ -> io:format("\n\nWarning, only (~p) test suites passed.\n",[Num])
      end
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
    {_,TestResults} = lists:foldl(fun(ExpectedResult, {Index, InTest}) ->
      {_,CurrentTest} = test(Kind,Name,Index),
      io:format("\n~p:~p/~p, result/expected: ~p/~p.\n\n- - - - - - - -\n\n\n",[Kind,Name,Index,CurrentTest,ExpectedResult]),
      %% pause if test does not match expected
      case (CurrentTest=:=ExpectedResult) of true -> ok; _ -> timer:sleep(10000) end,
      %% return whether it was expected
      {Index+1, InTest++[CurrentTest=:=ExpectedResult]}
    end, {1,[]},Expected),
    InResults++TestResults
  end, [], Map),
  %% return
  Result = lists:all(fun(R) -> (R=:=true) end, Results),
  io:format("\n~p, result: ~p.\n",[Kind,Result]),
  Result;
%%

%% @doc runs test for given basic suite (these kinds of tests tail-loop)
run_tests(Kind,[{_Name,_Index}|_T]=Names)
when is_atom(_Name) and is_integer(_Index) -> 
  %% to map
  Map = maps:from_list(Names),
  %% for each in map, run and test each
  Results = maps:fold(fun(Name, Iterations, InResults) ->
    %% for each expected
    TestResults = test(Kind,Name,Iterations),
    io:format("\n~p:~p/~p, result: ~p.\n",[Kind,Name,Iterations,TestResults]),
    InResults++[TestResults]
  end, [], Map),
  %% return
  Result = lists:all(fun(R) -> (R=:=true) end, Results),
  io:format("\n~p, result: ~p.\n",[Kind,Result]),
  %% pause if test does not match expected
  case Results of true -> ok; _ -> timer:sleep(10000) end,
  Result.
%%



%% @doc individual tests
-spec test(atom(), atom(), integer()) -> boolean().

%% @doc catch for returning once index has reached 0
test(_Kind,_Name,0) -> true;

% %% @doc test for type-checking recursive processes, rules [Rec] and [Var]
% test(type_checking=_Name, recursion_fail=_Kind, Index) ->
%   %% sending 
%   %% get process, type and clocks
%   Process = {'p','->','infinity',{start,undefined},
%               {'set', "x", {'def', 
%                 {'p','->',{'leq',1},{stop,undefined},term,
%                  'after', {'delay', {t,'leq',5}, 
%                     {'if', {"x", 'les', 5}, 
%                      'then', {'p','<-',{data,undefined},{'set',"x",{'call',"loop"}}},
%                      'else', {'p','->',{stop,undefined},term}
%                 }}}}, 'as', {"loop", []}}},
%   Type = {recv,{start,none},true,[x], {'def',"loop",
%           [{recv,{stop,none},{x,'leq',1},[],'end'},
%            {send,{data,none},{{x,'gtr',1}, 'and', {x,'les',5}},[x],{'call',"loop"}},
%            {recv,{stop,none},{x,'geq',5},[],'end'}]}},
%   Clocks = [{x,0}],
%   %% type check
%   Gamma = #{},
%   Theta = #{},
%   Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
%   {IsTyped,_} = Result,
%   io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nIsTyped: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,IsTyped]),
%   {ok, IsTyped};
% %%

%% @doc test for type-checking recursive processes, rules [Rec] and [Var]
test(type_checking=_Name, recursion_good=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {'p','->',0,{start,undefined}, {'set', "y", 
              {'def', 
                {'p','->',{'leq',1},{stop_early,undefined},term,
                 'after', {'delay', {t,'leq',1}, 
                    {'if', {"y", 'les', 2}, 
                     'then', {'p','<-',{data,undefined},{'set',"y",{'call',{"loop",{[],[]}}}}},
                     'else', {'p','->','infinity',{stop_late,undefined},term}
                }}}, 'as', {"loop",{[],[]}}}}},
  Type = [{recv,{start,none},{x,eq,0},[x,y],
            {'def',"loop",
              [{recv,{stop_early,none},{x,'leq',1},[],'end'},
              {send,{data,none},{{{y,'gtr',1}, 'and', {x,'gtr',1}}, 'and', {x,'les',2}},[y],{'call',"loop"}},
              {recv,{stop_late,none},{x,'geq',2},[],'end'}]}}],
  Clocks = [{x,0}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  {IsTyped,_} = Result,
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nIsTyped: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,IsTyped]),
  {ok, IsTyped};
%%

%% @doc test for type-checking, rules [IfTrue]  and [IfFalse]
test(type_checking=_Name, if_timer=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {'set', "x", {delay, {t,'leq',5}, {
              'if', {"x", 'les', 5}, 
              'then', {'p','<-',{a,undefined}, 'term'},
              'else', {'p','->',infinity,{b,undefined}, 'term'}
            }}},
  Type = [{send,{a,none},{x,'les',5},[],'end'},
          {recv,{b,none},{x,'geq',5},[],'end'}],
  % Clocks = [{x,0}],
  Clocks = [{x,Index-1}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  {IsTyped,_} = Result,
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nIsTyped: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,IsTyped]),
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Recv] 
test(type_checking=_Name, recv=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {'p','->',{'leq',5},{a,undefined}, 'term'},
  Type = {recv,{a,none},{x,'leq',5},[],'end'},
  Clocks = [{x,Index-1}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  {IsTyped,_} = Result,
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nIsTyped: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,IsTyped]),
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-delta] 
test(type_checking=_Name, del_delta_send=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, {t,'les',3}, {'p','<-',{a,undefined}, 'term'}},
  Type = {send,{a,none},{x,'les',9},[],'end'},
  Clocks = [{x,3}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(#{gamma=>Gamma, theta=>Theta, process=>Process, delta=>{Clocks,Type}, show_trace=>true}),
  {IsTyped,_} = Result,
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nIsTyped: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,IsTyped]),
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] 
test(type_checking=_Name, del_t_branch_cascade=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 1.0, {'p','->',0,[{{c,undefined},'term'}], 
            'after', {delay, 5.0, {'p','->',{'les',3},[{{a,undefined},'term'},{{b,undefined},'term'}],
                                  'after', {'p','->',infinity,{b,undefined},'term'}}}}},
  Type = [{recv,{a,none},{{x,'geq',6},'and',{x,'les',9}},[],'end'},
          {recv,{b,none},{y,'geq',6},[],'end'},
          {recv,{c,none},{z,'eq',1},[],'end'}],
  Clocks = [{x,0}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~w,\nType:\t~w,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] 
test(type_checking=_Name, del_t_branch_cascade_simple=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 1.0, 
              {'p','->',{'les',3},[{{a,undefined},'term'},{{b,undefined},'term'}],
               'after', {'p','->',infinity,{b,undefined},'term'}}},
  Type = [{recv,{a,none},{{x,'geq',1},'and',{x,'les',4}},[],'end'},
          {recv,{b,none},{y,'geq',1},[],'end'}],
  Clocks = [{x,0}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~w,\nType:\t~w,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] 
test(type_checking=_Name, del_t_branch=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 2.0*(Index), {'p','->',infinity,[{{a,undefined},'term'},{{b,undefined},'term'}]}},
  Type = [{recv,{a,none},{x,'geq',6},[],'end'},
          {recv,{b,none},{y,'geq',6},[],'end'}],
  Clocks = [{x,2}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] 
test(type_checking=_Name, del_t_recv=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 2.0*(Index), {'p','->',infinity,{a,undefined}, 'term'}},
  Type = {recv,{a,none},{x,'geq',6},[],'end'},
  Clocks = [{x,2}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] that should pass
test(type_checking=_Name, del_t_pass_send=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 4.0, {'p','<-',{a,undefined}, 'term'}},
  Type = {send,{a,none},{x,'gtr',4},[],'end'},
  Clocks = [{x,3*Index}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc test for type-checking, rule [Del-t] that should fail
test(type_checking=_Name, del_t_fail_send=_Kind, Index) ->
  %% sending 
  %% get process, type and clocks
  Process = {delay, 4.0, {'p','<-',{a,undefined}, 'term'}},
  Type = {send,{a,none},{'not',{x,'gtr',4}},[],'end'},
  Clocks = [{x,3*Index}],
  %% type check
  Gamma = #{},
  Theta = #{},
  Result = checker:eval(Gamma, Theta, Process, {Clocks,Type}),
  io:format("\n~p:~p/~p,\n\nGamma: ~p, Theta: ~p,\nPrc:\t~p,\nType:\t~p,\nClocks: ~p.\n\nResult: ~p.",[_Name,_Kind,Index,Gamma,Theta,Process,Type,Clocks,Result]),
  {IsTyped,_} = Result,
  {ok, IsTyped};
%%

%% @doc not_t_reading tests
test(not_t_reading=_Name, Kind, Index) -> 
  {ok, Result} = test(t_reading, Kind, Index),
  % io:format("\n~p:~p/~p, Result: ~p (not ~p).",[_Name,Kind,Index,not Result,Result]),
  {ok, not Result};

%% @doc t_reading tests, send_gtr
test(t_reading=_Name, send_gtr=_Kind, Index) ->
  %% get type and delay
  Type = sample:sample(interact,#{
    mode=>half_random,
    direction=>send,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negated=>false}},
      dbcs=>['gtr']
    }}),
  %% get time delay
  T = 2.5,%rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample:sample({clocks_valuations,{sample:sample(clocks),#{x=>#{value=>2}}}}),
  io:format("\n(~p:~p/~p) Type: ~p,\n\tClocks: ~p,\n\tT: ~p.\n\n",[_Name,_Kind,Index,Type,Clocks,T]),
  %% ask z3
  {ok, Result, _ExecString} = z3:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  case Result of true -> ok; _ -> io:format("\n\nWarning, not t_reading. ExecString:\n~s\n.",[_ExecString]) end,
  %% return (and call next iteration)
  {ok, Result};
%%

%% @doc t_reading tests, send_leq
test(t_reading=_Name, send_leq=_Kind, Index) ->
  %% get type and delay
  Type = sample:sample(interact,#{
    mode=>half_random,
    direction=>send,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negated=>true}},
      dbcs=>['gtr']
    }}),
  %% get time delay
  T = 2.5,%rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample:sample({clocks_valuations,{sample:sample(clocks),#{x=>#{value=>2}}}}),
  io:format("\n(~p:~p/~p) Type: ~p,\n\tClocks: ~p,\n\tT: ~p.\n\n",[_Name,_Kind,Index,Type,Clocks,T]),
  %% ask z3
  {ok, Result, _ExecString} = z3:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  case Result of true -> ok; _ -> io:format("\n\nWarning, not t_reading. ExecString:\n~s\n.",[_ExecString]) end,
  %% return (and call next iteration)
  {ok, Result};
%%

%% @doc t_reading tests, recv_gtr
test(t_reading=_Name, recv_gtr=_Kind, Index) ->
  %% get type and delay
  Type = sample:sample(interact,#{
    mode=>half_random,
    direction=>recv,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3*Index,negated=>false}},
      dbcs=>['gtr']
    }}),
  %% get time delay
  T = 2.5,%rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample:sample({clocks_valuations,{sample:sample(clocks),#{x=>#{value=>2}}}}),
  io:format("\n(~p:~p/~p) Type: ~p,\n\tClocks: ~p,\n\tT: ~p.\n\n",[_Name,_Kind,Index,Type,Clocks,T]),
  %% ask z3
  {ok, Result, _ExecString} = z3:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  case Result of true -> ok; _ -> io:format("\n\nWarning, not t_reading. ExecString:\n~s\n.",[_ExecString]) end,
  %% return (and call next iteration)
  {ok, Result};
%%

%% @doc t_reading tests, recv_leq
test(t_reading=_Name, recv_leq=_Kind, Index) ->
  %% get type and delay
  Type = sample:sample(interact,#{
    mode=>half_random,
    direction=>recv,
    has_constraints=>{mode,half_random},
    constraint_spec=>#{
      clocks=>[x],
      constants=>#{x=>#{constant=>3,negated=>true}},
      dbcs=>['gtr']
    }}),
  %% get time delay
  T = 2.5,%rand:uniform_real()*?DELAY_UPPER_BOUND,
  %% get random set of clocks
  Clocks = sample:sample({clocks_valuations,{sample:sample(clocks),#{x=>#{value=>2*Index}}}}),
  io:format("\n(~p:~p/~p) Type: ~p,\n\tClocks: ~p,\n\tT: ~p.\n\n",[_Name,_Kind,Index,Type,Clocks,T]),
  %% ask z3
  {ok, Result, _ExecString} = z3:ask_z3(t_reading, #{clocks=>Clocks,type=>Type,t=>T}),
  case Result of true -> ok; _ -> io:format("\n\nWarning, not t_reading. ExecString:\n~s\n.",[_ExecString]) end,
  %% return (and call next iteration)
  {ok, Result};
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


