-module(test_basic_timeout_exa).
-compile(export_all).
-compile(nowarn_export_all).

%% use this
%% generate:gen(test_basic_timeout_exa:role_s(),"tb_tout_s.erl").

role_s() ->
    {act, s_msg, 
        {act, r_ack, endP,
         aft, 3, {act, s_tout, endP}
        }
    }.


role_r() -> 
    {act, r_msg,
        {act, s_ack, endP}
    }.
