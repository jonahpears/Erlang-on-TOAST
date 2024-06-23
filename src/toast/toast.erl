-module(toast).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).

-type process () :: toast_process:toast_process().
-type type () :: toast_type:toast_type().









check(Type,Process) ->
  io:format("\n\n= = = = = = =\n\nchecking,\n\ntype:\t~p,\n\nprocess:\t~p.\n",[Type,Process]),

  Result = checker:type_check(Type,Process),

  io:format("\n\n- - - - - - -\n\nResult:\t~p.",[Result]),

  ok.










type(String)
when is_list(String) -> 
  toast_type:parse_toast(wrapper, String).
%%


test(send_a) -> type("!a(x>5).end");
test(recv_a) -> type("?a(x>5).end");
test(rec_loop) -> type("def(a).?a(x>5).call(a)");
test(_) -> test().
test() -> test(send_a).