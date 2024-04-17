-module(msg_throttling_v3).
-compile(export_all).
-compile(nowarn_export_all).

%% use this
%% generate:gen(msg_throttling_v3:role_msger(),"_msg_throttling_msger_v3.erl"),generate:gen(msg_throttling_v3:role_acker(),"_msg_throttling_acker_v3.erl").
%% 
%% TODO :: OR use this 
%% generate:gen(server, msg_throttling_v3:role_server(),"_msg_throttling_v3").

%% message throttling (m=2)
role_msger() ->
    {rec, "s1", 
        {act, s_msg1, 
            {rec, "s2", 
                {act, r_ack1, 
                    {rvar, "s1"}, 
                 aft, 3000, 
                    {act, s_msg2, 
                        {act, r_ack2, 
                            {rvar, "s2"}, 
                        aft, 3000,
                            issue_timeout
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
                 aft, 3000, 
                    {act, r_msg2, 
                        {act, s_ack2, 
                            {rvar, "t2"}
                        }
                    }
                }
            }
        }
    }.

role_msger(notimeout) ->
    {rec, "s1", 
        {act, s_msg1, 
            {rec, "s2", 
                {act, r_ack1, 
                    {rvar, "s1"}, 
                 aft, 3000, 
                    {act, s_msg2, 
                        {act, r_ack2, 
                            {rvar, "s2"}, 
                        aft, 3000,
                            {act, s_tout, endP}
                        }
                    }
                }
            }
        }
    }.

role_acker(notimeout) ->
    {rec, "t1", 
        {act, r_msg1, 
            {rec, "t2", 
                {act, s_ack1, 
                    {rvar, "t1"}, 
                 aft, 3000, 
                    {act, r_msg2, 
                        {act, s_ack2, 
                            {rvar, "t2"}, 
                        aft, 3000,
                            {act, r_tout, endP}
                        }
                    }
                }
            }
        }
    }.

%% message throttling (m=2)
role_server() ->
    {roles, [msger, acker],
        {rec, "s1", 
            {act, {msger, acker}, msg1, 
                {rec, "s2", 
                    { act_del, {acker, msger}, ack1, {rvar, "s1"}, 
                      aft, 3000, 
                        { act, {msger, acker}, msg2, 
                            { act_del, {acker, msger}, ack2, {rvar, "s2"}, 
                              aft, 3000, {error, msger}
                            }
                        }
                    }
                }
            }
        }
    }.