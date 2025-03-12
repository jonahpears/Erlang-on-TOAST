-module(running_exa).
-compile(export_all).
-compile(nowarn_export_all).

prc1() -> { 'def', { 'set', "x", {
  'p', '->', {'les', 5000}, [ {{'a', 'undefined'}, 'term'} ], 
  'after', { 'if', {"x", 'eq', 5000}, 'then', {
      'p', '<-', {'b', 'undefined'}, {'call', {"S", {[], []}}}
    }, 'else', 'error' } } }, 'as', {"S", {[], []}} }.

spec() -> {rec,"S", {act, r_a, endP, aft, 5000, {act, s_b, {rvar,"S"}, aft, 0, error}}}.


