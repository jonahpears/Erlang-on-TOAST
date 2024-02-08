-module(msg_throttling_exa).
-compile(export_all).
-compile(nowarn_export_all).

%% use this
%% generate:gen(msg_throttling_exa:role_msger(),"msg_throttling_msger.erl").
%% generate:gen(msg_throttling_exa:role_acker(),"msg_throttling_acker.erl").


%% message throttling (m=2)
role_msger() ->
    {rec, "s1", 
        {act, s_msg1, 
            {rec, "s2", 
                {act, r_ack1, 
                    {rvar, "s1"}, 
                 aft, 3, 
                    {act, s_msg2, 
                        {rec, "s2", 
                            {act, r_ack2, 
                                {rvar, "s2"}, 
                             aft, 3,
                                {act, s_tout, endP}
                            }
                        }
                    }
                }
            }
        }
    }.

role_acker() ->
    {rec, "t1", 
        {act, r_msg1, 
            {rec, "t2", 
                {act, s_ack1, 
                    {rvar, "t1"}, 
                 aft, 3, 
                    {act, r_msg2, 
                        {rec, "t2", 
                            {act, s_ack2, 
                                {rvar, "t2"}, 
                             aft, 3,
                                {act, r_tout, endP}
                            }
                        }
                    }
                }
            }
        }
    }.
        
% role_acker() ->
%     {rec, "t1",
%         {act, r_msg1,
%             {set, "x", {delay, {0, leq, 6}, 
%                 {iff, {"x", leq, 3},
%                     {act, s_ack2, {rvar, "t1"}},
%                 else, {rec, "t2", 
%                     {act, r_msg2,
%                         {set, "x", {delay, {0, leq, 6}, 
%                             {iff, {"x", leq, 3},
%                                 {act, s_ack2, {rvar, "t2"}},
%                             else, {act, r_tout, endP}}
%                         }}
%                     }
%                 }}
%             }}
%         }
%     }.
        
    