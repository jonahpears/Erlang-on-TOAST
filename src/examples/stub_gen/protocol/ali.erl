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


spec(nothing) -> endP.
