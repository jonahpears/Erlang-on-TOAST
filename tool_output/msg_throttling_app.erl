-module(msg_throttling_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([m_send_msg1/1,
         m_send_msg2/1,
         m_send_msg3/1,
         a_send_ack1/1,
         a_send_ack2/1]).

start(_StartType, _StartArgs) -> msg_throttling_sup:start_link().

stop(_State) -> ok.

%% internal functions
m_send_msg1(Msg1) -> io:format("m_send_msg1(~p).\n",[Msg1]), ok.
m_send_msg2(Msg2) -> io:format("m_send_msg2(~p).\n",[Msg2]), ok.
m_send_msg3(Msg3) -> io:format("m_send_msg3(~p).\n",[Msg3]), ok.
a_send_ack1(Ack1) -> io:format("a_send_ack1(~p).\n",[Ack1]), ok.
a_send_ack2(Ack2) -> io:format("a_send_ack2(~p).\n",[Ack2]), ok.


