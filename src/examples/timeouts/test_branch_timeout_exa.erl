-module(test_branch_timeout_exa).
-compile(export_all).
-compile(nowarn_export_all).

%% use this
%% generate:gen(test_branch_timeout_exa:role_s(),"tbranch_tout_s.erl").

role_s() ->
    {act, s_msg, 
        {branch, [
                {r_accept, endP},
                {r_reject, endP}
            ],aft, 3, {act, s_tout, endP}
        }
    }.


role_r() -> 
    {act, r_msg,
        {act, s_accept, endP}
    }.
