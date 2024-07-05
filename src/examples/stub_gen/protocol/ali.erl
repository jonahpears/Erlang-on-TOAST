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
  gen_stub:gen(ali,spec,basic_send_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_loop,"_ali_test.erl"),
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
  gen_stub:gen(ali,spec,basic_recv_after_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_timer_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_timer_recv,"_ali_test.erl"),
  ok;
  
gentest(basic_cotimeouts) -> 
  gen_stub:gen(ali,spec,basic_send_after_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_timer_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_timer_recv,"_ali_test.erl"),
  ok;

gentest(basic_choices) ->
  gen_stub:gen(ali,spec,basic_branch,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select,"_ali_test.erl"),
  ok;

gentest(advanced_timeouts) ->
  gen_stub:gen(ali,spec,basic_branch_after_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_timer_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_timer_recv,"_ali_test.erl"),
  ok;

gentest(advanced_cotimeouts) ->
  gen_stub:gen(ali,spec,basic_select_after_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_timer_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_timer_recv,"_ali_test.erl"),
  ok;

gentest(basic_if_statements) ->
  gen_stub:gen(ali,spec,basic_if_then,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_then_else,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_else,"_ali_test.erl"),
  ok;

gentest(basic_if_statements_loops) ->
  gen_stub:gen(ali,spec,basic_if_then_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_then_else_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_else_loop,"_ali_test.erl"),
  ok;

gentest(advanced_mixed_choice) ->
  gen_stub:gen(ali,spec,advanced_mixed_choice_send_first,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_recv_first,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_select_first,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_branch_first,"_ali_test.erl"),
  ok;

gentest(advanced_mixed_choice_loops) ->
  gen_stub:gen(ali,spec,advanced_mixed_choice_send_first_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_recv_first_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_select_first_loop,"_ali_test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_branch_first_loop,"_ali_test.erl"),
  ok;

gentest(basic_errors) ->
  gen_stub:gen(ali,spec,basic_error_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_select,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_branch,"_ali_test.erl"),
  ok;

gentest(basic_errors_after) ->
  gen_stub:gen(ali,spec,basic_error_send_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_recv_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_select_after,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_branch_after,"_ali_test.erl"),
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
  ok=gentest(basic_if_statements),
  ok=gentest(basic_if_statements_loops),
  ok=gentest(basic_errors),
  ok=gentest(basic_errors_after),
  ok=gentest(advanced_mixed_choice),
  ok;

%% TODO:: fix gen_snippets:next_state_funs
%% TODO::  - correctly generates for scope function, but fails to pull back to main
%% TODO::  - ideally, just use callback function in main, and leave function as it is 
%% TODO::  - however, still some issues in the function scope, in all except standard recursive states
gentest(tests) -> 
  %% basic actions
  gen_stub:gen(ali,spec,basic_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_send_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_send,"test.erl"),

  %% basic loops
  gen_stub:gen(ali,spec,basic_send_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_send_recv_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_send_loop,"test.erl"),

  %% basic timers
  gen_stub:gen(ali,spec,basic_timer_before,"test.erl"),
  gen_stub:gen(ali,spec,basic_timer_after,"test.erl"),

  %% basic delays
  gen_stub:gen(ali,spec,basic_delay,"test.erl"),
  gen_stub:gen(ali,spec,basic_timer_delay,"test.erl"),

  %% basic choices
  gen_stub:gen(ali,spec,basic_branch,"test.erl"),
  gen_stub:gen(ali,spec,basic_select,"test.erl"),

  % %% basic timeouts
  gen_stub:gen(ali,spec,basic_recv_after_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_timer_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_recv_after_timer_recv,"test.erl"),

  %% advanced timeouts
  gen_stub:gen(ali,spec,basic_branch_after_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_timer_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_branch_after_timer_recv,"test.erl"),

  %% basic co-timeouts
  gen_stub:gen(ali,spec,basic_send_after_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_timer_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_send_after_timer_recv,"test.erl"),

  %% advanced co-timeouts
  gen_stub:gen(ali,spec,basic_select_after_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_recv,"test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_timer_send,"test.erl"),
  gen_stub:gen(ali,spec,basic_select_after_timer_recv,"test.erl"),

  %% basic if-statements
  gen_stub:gen(ali,spec,basic_if_then,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_then_else,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_else,"test.erl"),

  %% basic if-statement loops
  gen_stub:gen(ali,spec,basic_if_then_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_then_else_loop,"test.erl"),
  gen_stub:gen(ali,spec,basic_if_not_then_else_loop,"test.erl"),

  %% advanced mixed-choice
  gen_stub:gen(ali,spec,advanced_mixed_choice_send_first,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_recv_first,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_select_first,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_branch_first,"test.erl"),

  %% advanced mixed-choice loops
  gen_stub:gen(ali,spec,advanced_mixed_choice_send_first_loop,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_recv_first_loop,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_select_first_loop,"test.erl"),
  gen_stub:gen(ali,spec,advanced_mixed_choice_branch_first_loop,"test.erl"),

  %% errors
  gen_stub:gen(ali,spec,basic_error_send,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_recv,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_select,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_error_branch,"_ali_test.erl"),

  %% equal timing
  gen_stub:gen(ali,spec,basic_send_eq,"_ali_test.erl"),
  gen_stub:gen(ali,spec,basic_recv_eq,"_ali_test.erl"),





  %% tests for the spec extractor
  % ok=gentest(basic_delays),
  % ok=gentest(basic_timeouts),
  % ok=gentest(basic_cotimeouts),


  % %% tests for the stub generator
  % gen_stub:gen(ali,spec,basic_if_then_else_loop,"_ali_test.erl"),%% TODO:: fix as above
  % ok=gentest(advanced_mixed_choice_loops),%% TODO:: fix as above
  ok;

gentest(all) -> 
  gentest(implemented),
  gentest(tests),
  ok;

gentest(_) -> gen_stub:gen(ali,spec,default,"_ali_test.erl").

t() -> gentest(tests).

all() -> gentest(all).

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

%% eq testing
spec(basic_send_eq) -> 
  {act, s_before_5s, endP, 
    aft, '?EQ_LIMIT_MS', error};

spec(basic_recv_eq) -> 
  {act, r_before_5s, endP, 
    aft, '?EQ_LIMIT_MS', error};


%% cotimeouts
spec(basic_send_after_send) -> 
  {act, s_before_5s, endP, 
    aft, 5000, {act, s_after_5s, endP}};

spec(basic_send_after_recv) -> 
  {act, s_before_5s, endP, 
    aft, 5000, {act, r_after_5s, endP}};

spec(basic_send_after_timer_send) -> 
  {timer, "t", 5000, 
    {act, s_before_5s, endP, 
     aft, "t", {act, s_after_5s, endP}}};

spec(basic_send_after_timer_recv) -> 
  {timer, "t", 5000, 
    {act, s_before_5s, endP, 
     aft, "t", {act, r_after_5s, endP}}};

%% timeouts 
spec(basic_recv_after_send) -> 
  {act, r_before_5s, endP, 
    aft, 5000, {act, s_after_5s, endP}};

spec(basic_recv_after_recv) -> 
  {act, r_before_5s, endP, 
   aft, 5000, {act, r_after_5s, endP}};

spec(basic_recv_after_timer_send) -> 
  {timer, "t", 5000, {act, r_before_5s, endP, 
    aft, "t", {act, s_after_5s, endP}}};

spec(basic_recv_after_timer_recv) -> 
  {timer, "t", 5000, {act, r_before_5s, endP, 
    aft, "t", {act, r_after_5s, endP}}};

%% branch timeouts
spec(basic_branch_after_send) -> 
  {branch, [
            {r_msg1, {act, s_msgA, endP}},
            {r_msg2, {act, s_msgB, endP}},
            {r_msg3, {act, s_msgC, endP}}
          ], aft, 5000, {act, s_timeout, endP}};

spec(basic_branch_after_recv) -> 
  {branch, [
            {r_msg1, {act, s_msgA, endP}},
            {r_msg2, {act, s_msgB, endP}},
            {r_msg3, {act, s_msgC, endP}}
          ], aft, 5000, {act, r_timeout, endP}};

spec(basic_branch_after_timer_send) -> 
  {timer, "t", 5000, 
    {branch, [
              {r_msg1, {act, s_msgA, endP}},
              {r_msg2, {act, s_msgB, endP}},
              {r_msg3, {act, s_msgC, endP}}
            ], aft, "t", {act, s_timeout, endP}}};

spec(basic_branch_after_timer_recv) -> 
  {timer, "t", 5000, 
    {branch, [
              {r_msg1, {act, s_msgA, endP}},
              {r_msg2, {act, s_msgB, endP}},
              {r_msg3, {act, s_msgC, endP}}
            ], aft, "t", {act, r_timeout, endP}}};

%% select cotimeouts
spec(basic_select_after_send) -> 
  {select, [
            {s_msgA, {act, r_msg1, endP}},
            {s_msgB, {act, r_msg2, endP}},
            {s_msgC, {act, r_msg3, endP}}
          ], aft, 5000, {act, s_timeout, endP}};

spec(basic_select_after_recv) -> 
  {select, [
            {s_msgA, {act, r_msg1, endP}},
            {s_msgB, {act, r_msg2, endP}},
            {s_msgC, {act, r_msg3, endP}}
          ], aft, 5000, {act, r_timeout, endP}};

spec(basic_select_after_timer_send) -> 
  {timer, "t", 5000, 
    {select, [
              {s_msgA, {act, r_msg1, endP}},
              {s_msgB, {act, r_msg2, endP}},
              {s_msgC, {act, r_msg3, endP}}
            ], aft, "t", {act, s_timeout, endP}}};

spec(basic_select_after_timer_recv) -> 
  {timer, "t", 5000, 
    {select, [
              {s_msgA, {act, r_msg1, endP}},
              {s_msgB, {act, r_msg2, endP}},
              {s_msgC, {act, r_msg3, endP}}
            ], aft, "t", {act, r_timeout, endP}}};

%% if statements
spec(basic_if_then) -> 
  {timer, "t", 5000, 
    {if_timer, "t", 
      {act, s_finished, endP} }};

spec(basic_if_then_loop) -> 
  {timer, "t", 5000, 
    {rec, "a", 
      {if_timer, "t", 
        {act, s_finished, {rvar, "a"}} }}};

spec(basic_if_then_else) -> 
  {timer, "t", 5000, 
    {if_timer, "t", {act, s_finished, endP},
     'else', {act, s_data, endP} }};

spec(basic_if_then_else_loop) -> 
  {timer, "t", 5000, 
    {rec, "a", 
      {if_timer, "t", {act, s_finished, endP},
       'else', {act, s_data, {rvar, "a"}} }}};

%% if not 
spec(basic_if_not_then) -> 
  {timer, "t", 5000, 
    {if_not_timer, "t", 
      {act, s_finished, endP} }};

spec(basic_if_not_then_loop) -> 
  {timer, "t", 5000, 
    {rec, "a", 
      {if_not_timer, "t", 
        {act, s_finished, {rvar, "a"}} }}};

spec(basic_if_not_then_else) -> 
  {timer, "t", 5000, 
    {if_not_timer, "t", {act, s_finished, endP},
     'else', {act, s_data, endP} }};

spec(basic_if_not_then_else_loop) -> 
  {timer, "t", 5000, 
    {rec, "a", 
      {if_not_timer, "t", {act, s_finished, endP},
       'else', {act, s_data, {rvar, "a"}} }}};



%% mixed choice
spec(advanced_mixed_choice_send_first) ->
  {timer, "t1", 5000, {
    act, s_first, {
      act, r_second, endP, aft, "t1", error
    },
    aft, 3000, {
      act, r_second, endP, 
      aft, "t1", {
        act, s_third, endP
      }
    }
  }};


spec(advanced_mixed_choice_recv_first) ->
  {timer, "t1", 5000, {
    act, r_first, {
      act, s_second, endP, aft, "t1", error
    },
    aft, 3000, {
      act, s_second, endP, 
      aft, "t1", {
        act, r_third, endP
      }
    }
  }};

%% advanced select/branch
spec(advanced_mixed_choice_select_first) ->
  {timer, "t1", 5000, {
    select, [
      {s_first, {
        act, r_second, endP, aft, "t1", error
      }},
      {s_third, endP}
    ],
    aft, 3000, {
      branch, [
        {r_fourth, endP},
        {r_fifth, endP}
      ],
      aft, "t1", {
        act, s_sixth, endP
      }
    }
  }};

  spec(advanced_mixed_choice_branch_first) ->
    {timer, "t1", 5000, {
      branch, [
        {r_first, {
          act, s_second, endP, aft, "t1", error
        }},
        {r_third, endP}
      ],
      aft, 3000, {
        select, [
          {s_fourth, endP},
          {s_fifth, endP}
        ],
        aft, "t1", {
          act, r_sixth, endP
        }
      }
    }};
  
  

%% mixed choice loops
spec(advanced_mixed_choice_send_first_loop) ->
  {rec, "r1", {timer, "t1", 5000, {
    act, s_first, {
      act, r_second, endP, aft, "t1", error
    },
    aft, 3000, {
      act, r_second, {rvar, "r1"}, 
      aft, "t1", {rec, "r2", {
        act, s_third, {rvar, "r2"}
      }}
    }
  }}};


spec(advanced_mixed_choice_recv_first_loop) ->
  {rec, "r1", {timer, "t1", 5000, {
    act, r_first, {
      act, s_second, endP, aft, "t1", error
    },
    aft, 3000, {
      act, s_second, {rvar, "r1"}, 
      aft, "t1", {rec, "r2", {
        act, r_third, {rvar, "r2"}
      }}
    }
  }}};

%% advanced select/branch loops
spec(advanced_mixed_choice_select_first_loop) ->
  {timer, "t1", 5000, {rec, "r1", {
    select, [
      {s_first, {
        act, r_second, {rvar, "r1"}, aft, "t1", error
      }},
      {s_third, endP}
    ],
    aft, 3000, {
      branch, [
        {r_fourth, {rvar, "r1"}},
        {r_fifth, endP}
      ],
      aft, "t1", {
        act, s_sixth, {rec, "r2", {
          select, [
            {s_seventh, {
              act, r_eighth, {rvar, "r1"}, aft, "t1", error
            }},
            {s_nine, endP}
          ],
          aft, 3000, {
            branch, [
              {r_ten, {rvar, "r2"}},
              {r_eleven, endP}
            ],
            aft, "t1", {
              act, s_twleve, endP
            }
          }
        }}
      }
    }
  }}};

spec(advanced_mixed_choice_branch_first_loop) ->
  {timer, "t1", 5000, {rec, "r1", {
    branch, [
      {r_first, {
        act, s_second, {rvar, "r1"}, aft, "t1", error
      }},
      {r_third, endP}
    ],
    aft, 3000, {
      select, [
        {s_fourth, {rvar, "r1"}},
        {s_fifth, endP}
      ],
      aft, "t1", {
        act, r_sixth, {rec, "r2", {
          branch, [
            {r_seventh, {
              act, s_eighth, {rvar, "r1"}, aft, "t1", error
            }},
            {r_nine, endP}
          ],
          aft, 3000, {
            select, [
              {s_ten, {rvar, "r2"}},
              {s_eleven, endP}
            ],
            aft, "t1", {
              act, r_twleve, endP
            }
          }
        }}
      }
    }
  }}};
  


%% type: {!a(5<x<10), ?b(x>10)}
spec(mixed_test) -> 
  {timer, "t10", 1000, {
    timer, "t5", 5000,  {
      delay, "t5", {
        act, s_a, endP,
        aft, "t10", {act, r_b, endP}
      }
    }
  }};

%% type: {!a(x<3), !b(x<5), ?c(5<x<10), !d(x>10)}
%% type: [ (x<3)   :{!a,!b},
%%         (3<x<5) :{!b},
%%         (5<x<10):{?c},
%%         (10>x)  :{!b}    ]
spec(interleaved_test) -> 
  {timer, "t10", 1000, {
    timer, "t5", 5000,  {
      delay, "t5", {
        act, s_a, endP,
        aft, "t10", {act, r_b, endP}
      }
    }
  }};

%% type: \mu r1.{!data(x>1,{x}).r1, ?stop(x<1 and y>10).end}
%% type: \mu r1.[ (x<1) :{?stop.end} &(y>10)
%%                (x>1) :{!data{x}.r1} ]
spec(iteration_test) -> 
  {timer, "x1", 1000, {
    timer, "y10", 10000, {
      rec, "r1", {
        if_timer, "y10", {
          act, r_stop, endP
        }, 'else', {
          delay, "x1", {
            act, s_data, {
              {timer, "x1", {rvar, "x1"}}
            }
          }
        }
      }
    }
  }};


spec(basic_error_send) -> {act, s_msg, error};
spec(basic_error_recv) -> {act, r_msg, error};

spec(basic_error_send_after) -> {act, s_msg, endP, aft, 50, error};
spec(basic_error_recv_after) -> {act, r_msg, endP, aft, 50, error};

spec(basic_error_select) -> {select, [
                              {msgA, endP},
                              {msgB, error}
                            ]};  
spec(basic_error_branch) -> {branch, [
                              {msgA, endP},
                              {msgB, error}
                            ]};  

spec(basic_error_select_after) -> {select, [
                              {msgA, endP},
                              {msgB, endP}
                            ], aft, 50, error};  
spec(basic_error_branch_after) -> {branch, [
                              {msgA, endP},
                              {msgB, endP}
                            ], aft, 50, error};  

spec(nothing) -> endP.
