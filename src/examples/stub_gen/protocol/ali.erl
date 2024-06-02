-module(ali).
-compile(export_all).
-compile(nowarn_export_all).

%% change this one for protocol
-define(PROTOCOL, basic_recv_send_loop).

%% use this
%% gen_stub:gen(ali,spec,default,"_ali_test.erl").


%% ali:gentest(tests).
%% ali:gentest(implemented).
%% ali:gentest(all).

gentest() -> gen_stub:gen(ali,spec,default,"_ali_test.erl").

gentest(basic_actions) -> 
  gen_stub:gen(ali,spec,basic_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_send,"_ali_test.erl"),
  ok;

gentest(basic_loops) -> 
  gen_stub:gen(ali,spec,basic_send_recv_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_send_loop,"_ali_test.erl"),
  ok;

gentest(basic_timers) -> 
  gen_stub:gen(ali,spec,basic_timer_before,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_timer_after,"_ali_test.erl"),
  ok;

gentest(basic_delays) -> 
  gen_stub:gen(ali,spec,basic_delay,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_timer_delay,"_ali_test.erl"),
  ok;
  
gentest(basic_timeouts) -> 
  gen_stub:gen(ali,spec,basic_recv_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_timer,"_ali_test.erl"),
  ok;
  
gentest(basic_cotimeouts) -> 
  gen_stub:gen(ali,spec,basic_send_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_timer,"_ali_test.erl"),
  ok;

gentest(basic_choices) ->
  gen_stub:gen(ali,spec,basic_branch,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select,"_ali_test.erl"),
  ok;

gentest(advanced_timeouts) ->
  gen_stub:gen(ali,spec,basic_branch_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_timer,"_ali_test.erl"),
  ok;

gentest(advanced_cotimeouts) ->
  gen_stub:gen(ali,spec,basic_select_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_timer,"_ali_test.erl"),
  ok;

gentest(basic_if_statements) ->
  gen_stub:gen(ali,spec,basic_if_then,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_then_else,"_ali_test.erl"),
  ok;

gentest(implemented) -> 
  ok=gentest(basic_actions),
  ok=gentest(basic_loops),
  ok=gentest(basic_timers),
  ok=gentest(basic_delays),
  ok=gentest(basic_timeouts),
  ok=gentest(basic_cotimeouts),
  ok=gentest(basic_choices),
  ok=gentest(advanced_timeouts),
  ok=gentest(advanced_cotimeouts),
  ok;

gentest(tests) -> 
  ok=gentest(basic_if_statements),
  ok;

gentest(all) -> 
  gentest(implemented),
  gentest(tests),
  ok;

gentest(_) -> gen_stub:gen(ali,spec,default,"_ali_test.erl").



spec() -> spec(?PROTOCOL).

spec(default) -> spec(?PROTOCOL);

spec(basic_send) -> {act, s_msgA, endP};
spec(basic_send_loop) -> {rec, "a", {act, s_msgA, {rvar, "a"}}};

spec(basic_recv) -> {act, r_msg1, endP};
spec(basic_recv_loop) -> {rec, "a", {act, r_msg1, {rvar, "a"}}};

spec(basic_send_recv) -> {act, s_msgA, spec(basic_recv)};
spec(basic_send_recv_loop) -> {act, s_msgA, spec(basic_recv_loop)};

spec(basic_recv_send) -> {act, r_msg1, spec(basic_send)};
spec(basic_recv_send_loop) -> {act, r_msg1, spec(basic_send_loop)};

%% timers & delays
spec(basic_timer_before) -> {timer, "t", 5000, {act, s_after_t, endP}};
spec(basic_timer_after) -> {act, s_before_t, {timer, "t", 5000, endP}};

spec(basic_delay) -> {act, s_before_5s, {delay, 5000, {act, s_after_5s, endP}}};

spec(basic_timer_delay) -> {timer, "t", 5000, {act, s_before_t, {delay, "t", {act, s_after_t, endP}}}};

%% new tests
spec(basic_branch) -> {branch, [
                            {r_msg1, {act, s_msgA, endP}},
                            {r_msg2, {act, s_msgB, endP}},
                            {r_msg3, {act, s_msgC, endP}}
                          ]};

spec(basic_select) -> {select, [
                            {s_msgA, {act, r_msg1, endP}},
                            {s_msgB, {act, r_msg2, endP}},
                            {s_msgC, {act, r_msg3, endP}}
                          ]};

%% timeouts / co-timeouts
spec(basic_send_after) -> {act, s_before_5s, endP, aft, 5000, {act, r_after_5s, endP}};

spec(basic_recv_after) -> {act, r_before_5s, endP, aft, 5000, {act, s_after_5s, endP}};


spec(basic_send_after_timer) -> {timer, "t", 5000, {act, s_before_5s, endP, aft, "t", {act, r_after_5s, endP}}};

spec(basic_recv_after_timer) -> {timer, "t", 5000, {act, r_before_5s, endP, aft, "t", {act, s_after_5s, endP}}};


spec(basic_branch_after) -> {branch, [
                            {r_msg1, {act, s_msgA, endP}},
                            {r_msg2, {act, s_msgB, endP}},
                            {r_msg3, {act, s_msgC, endP}}
                          ], aft, 5000, {act, s_timeout, endP}};

spec(basic_select_after) -> {select, [
                            {s_msgA, {act, r_msg1, endP}},
                            {s_msgB, {act, r_msg2, endP}},
                            {s_msgC, {act, r_msg3, endP}}
                          ], aft, 5000, {act, s_timeout, endP}};


spec(basic_branch_after_timer) -> {timer, "t", 5000, {branch, [
                            {r_msg1, {act, s_msgA, endP}},
                            {r_msg2, {act, s_msgB, endP}},
                            {r_msg3, {act, s_msgC, endP}}
                          ], aft, "t", {act, s_timeout, endP}}};

spec(basic_select_after_timer) -> {timer, "t", 5000, {select, [
                            {s_msgA, {act, r_msg1, endP}},
                            {s_msgB, {act, r_msg2, endP}},
                            {s_msgC, {act, r_msg3, endP}}
                          ], aft, "t", {act, s_timeout, endP}}};

%% if statements
spec(basic_if_then) -> {timer, "t", 5000, {rec, "a", 
                          { if_timer, "t", {act, s_finished, endP} }}};

spec(basic_if_then_else) -> {timer, "t", 5000, {rec, "a", 
                          { if_timer, "t", {act, s_finished, endP},
                            else, {act, s_data, {rvar, "a"}} }}};

spec(nothing) -> endP.
