-module(checker).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").


%% @doc python interface for z3
z3() ->

  Z3FileName = filename:absname("")++"/src/toast/",
  io:format("\nZ3FileName:\t~p.\n",[Z3FileName]),

  {ok, P} = python:start([{python_path, [Z3FileName]},{python, "python3"}]),


  R1 = python:call(P, 'os.path', splitext, [<<"name.ext">>]),
  io:format("\nR1:\t~p.\n",[R1]),


  R2 = python:call(P, z3_interface, test, [test(constraints)]),
  io:format("\nR2:\t~p.\n",[R2]),


  python:stop(P),

  ok.







test(constraints) ->
  Constraint = {"x", leq, 4},

  Constraint.


