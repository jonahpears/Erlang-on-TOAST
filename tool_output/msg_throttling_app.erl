-module(msg_throttling_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([m_send_msg1/1,
         m_send_msg2/1,
         m_send_tout/1,
         a_send_ack1/1,
         a_send_ack2/1]).

-record(args, {mode = normal}).

-define(SUPPORTED_MODES, [auto, manual])

start() -> start(normal, #args{mode = auto}).
start(_StartType, #args{mode = Mode} = _StartArgs) -> 

    msg_throttling:start_link(),

    io:format("app(~p).\n", [Mode]),
    case Mode of
        manual -> ok;
        
        auto ->
            
            ok;

        _Else -> 
            io:format("app: unexpected mode given (~p).\n", [Mode])
    end.

stop(_State) -> ok.

%% internal functions
m_send_msg1(Msg1) -> io:format("m_send_msg1(~p).\n",[Msg1]), ok.
m_send_msg2(Msg2) -> io:format("m_send_msg2(~p).\n",[Msg2]), ok.
m_send_tout(Tout) -> io:format("m_send_tout(~p).\n",[Tout]), ok.
a_send_ack1(Ack1) -> io:format("a_send_ack1(~p).\n",[Ack1]), ok.
a_send_ack2(Ack2) -> io:format("a_send_ack2(~p).\n",[Ack2]), ok.


