-module(msg_protocol).
-compile(export_all).
-compile(nowarn_export_all).



%% message throttling (m=2)
role_server() ->
    {roles, [msger, acker],
        {rec, "s1", 
            {act, {from, msger, to, acker}, msg1, 
                {rec, "s2", 
                    { act, {from, acker, to, msger}, ack1, {rvar, "s1"}, 
                      aft, 3000, 
                        { act, {from, msger, to, acker}, msg2, 
                            { act, {from, acker, to, msger}, ack2, {rvar, "s2"}, 
                              aft, 3000, {error, msger}
                            }
                        }
                    }
                }
            }
        }
    }.


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
                            {act, s_tout, endP}
                        }
                    }
                }
            }
        }
    }.

role_msger(error_timeout) ->
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
                            {error, timeout}
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
                            {rvar, "t2"}, 
                        aft, 3000,
                            {act, r_tout, endP}
                        }
                    }
                }
            }
        }
    }.

