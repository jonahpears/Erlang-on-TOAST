-module(toast_schema).
-file("toast_schema", 1).

-compile(export_all).
-compile(nowarn_export_all).

-include("data_records.hrl").


msger() -> 
  #statem_schema{ init_state = state1_send_msg1, 
                  timeouts = #{ state2a_recv_ack1 => {5000, state2b_send_msg2},
                                state3a_recv_ack2 => {5000, issue_timeout}      },
                  state_map = #{ state1_send_msg1  => #{send => #{msg1 => state2a_recv_ack1} },
                                 state2a_recv_ack1 => #{recv => #{ack1 => state1_send_msg1}  },
                                 state2b_send_msg2 => #{send => #{msg2 => state3a_recv_ack2} },
                                 state3a_recv_ack2 => #{recv => #{ack2 => state2a_recv_ack1} }} }.


acker() -> 
  #statem_schema{ init_state = state1_recv_msg1, 
                  timeouts = #{ state2a_send_ack1 => {5000, state2b_recv_msg2} },
                  state_map = #{ state1_recv_msg1  => #{recv => #{msg1 => state2a_send_ack1} },
                                 state2a_send_ack1 => #{send => #{ack1 => state1_recv_msg1}  },
                                 state2b_recv_msg2 => #{recv => #{msg2 => state3a_send_ack2} },
                                 state3a_send_ack2 => #{send => #{ack2 => state2a_send_ack1} }} }.



