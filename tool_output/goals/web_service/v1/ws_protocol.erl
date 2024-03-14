-module(ws_protocol).
-compile(export_all).
-compile(nowarn_export_all).


global() -> ok.


client() -> ok.


server() -> { rec, "top",
                { branch, [ {service_a, },
                            {service_b, },
                            {service_c, } ],
                  aft, 5000, { act, 

                             }
                }
            }.

