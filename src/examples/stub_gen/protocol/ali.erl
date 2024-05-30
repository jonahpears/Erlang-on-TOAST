-module(ali).
-compile(export_all).
-compile(nowarn_export_all).

%% change this one for protocol
-define(PROTOCOL, basic_recv_send_loop).

%% use this
%% gen_stub:gen(ali,spec,default,"_ali_test.erl").


spec() -> spec(?PROTOCOL).

spec(default) -> spec(?PROTOCOL);

spec(basic_send) -> {act, s_msg1, endP};
spec(basic_send_loop) -> {rec, "a", {act, s_msg1, {rvar, "a"}}};

spec(basic_recv) -> {act, r_msg1, endP};
spec(basic_recv_loop) -> {rec, "a", {act, r_msg1, {rvar, "a"}}};

spec(basic_send_recv) -> {act, s_msg1, spec(basic_recv)};
spec(basic_send_recv_loop) -> {act, s_msg1, spec(basic_recv_loop)};

spec(basic_recv_send) -> {act, r_msg1, spec(basic_send)};
spec(basic_recv_send_loop) -> {act, r_msg1, spec(basic_send_loop)};

%% new tests
spec(basic_branch) -> {branch, [
                            {act, r_msg1, {act, s_msgA, endP}},
                            {act, r_msg2, {act, s_msgB, endP}},
                            {act, r_msg3, {act, s_msgC, endP}}
                          ]};

spec(basic_select) -> {select, [
                            {act, s_msgA, {act, r_msg1, endP}},
                            {act, s_msgB, {act, r_msg2, endP}},
                            {act, s_msgC, {act, r_msg3, endP}}
                          ]};

%% timers & delays
spec(basic_delay) -> {act, s_before_5s, {delay, 5000, {act, s_after_5s, endP}}};

spec(basic_timer) -> {timer, "t", 5000, {act, s_before_t, {delay, "t", {act, s_after_t, endP}}}};

%% timeouts / co-timeouts
spec(basic_send_after) -> {act, s_before_5s, aft, 5000, {act, s_after_5s, endP}};

spec(basic_recv_after) -> {act, s_before_5s, aft, 5000, {act, s_after_5s, endP}};


spec(basic_send_after_timer) -> {timer, "t", 5000, {act, s_before_5s, aft, "t", {act, s_after_5s, endP}}};

spec(basic_recv_after_timer) -> {timer, "t", 5000, {act, s_before_5s, aft, "t", {act, s_after_5s, endP}}};


spec(basic_branch_after) -> {branch, [
                            {act, r_msg1, {act, s_msgA, endP}},
                            {act, r_msg2, {act, s_msgB, endP}},
                            {act, r_msg3, {act, s_msgC, endP}}
                          ], aft, 5000, {act, s_timeout, endP}};

spec(basic_select_acter) -> {select, [
                            {act, s_msgA, {act, r_msg1, endP}},
                            {act, s_msgB, {act, r_msg2, endP}},
                            {act, s_msgC, {act, r_msg3, endP}}
                          ], aft, 5000, {act, s_timeout, endP}};


spec(basic_branch_after_timer) -> {timer, "t", 5000, {branch, [
                            {act, r_msg1, {act, s_msgA, endP}},
                            {act, r_msg2, {act, s_msgB, endP}},
                            {act, r_msg3, {act, s_msgC, endP}}
                          ], aft, "t", {act, s_timeout, endP}}};

spec(basic_select_after_timer) -> {timer, "t", 5000, {select, [
                            {act, s_msgA, {act, r_msg1, endP}},
                            {act, s_msgB, {act, r_msg2, endP}},
                            {act, s_msgC, {act, r_msg3, endP}}
                          ], aft, "t", {act, s_timeout, endP}}};



spec(nothing) -> endP.
