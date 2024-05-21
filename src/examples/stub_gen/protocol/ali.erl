-module(ali).
-compile(export_all).
-compile(nowarn_export_all).

%% change this one for protocol
-define(PROTOCOL, basic_send).

%% use this
%% gen_stub:gen(ali:spec(),"_ali_test.erl").


spec() -> spec(?PROTOCOL).


spec(basic_send) -> {act, s_msg1, endP};

spec(basic_recv) -> {act, r_msg1, endP};

spec(nothing) -> endP.
