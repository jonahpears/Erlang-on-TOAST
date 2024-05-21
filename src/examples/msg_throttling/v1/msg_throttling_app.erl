-module(msg_throttling_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([m_send_msg1/1,
         m_send_msg2/1,
         m_send_tout/1,
         a_send_ack1/1,
         a_send_ack2/1]).

%% 
%% normal + trace:
%% c(msg_throttling_acker),c(msg_throttling_msger),c(msg_throttling_sup),c(msg_throttling),c(msg_throttling_app),msg_throttling_app:start(normal,{normal,false}),msg_throttling_msger:send_msg1("test1"),msg_throttling_acker:send_ack1("ack1").
%% 
%% 
%% normal + delayable:
%% c(msg_throttling_acker),c(msg_throttling_msger),c(msg_throttling_sup),c(msg_throttling),c(msg_throttling_app),msg_throttling_app:start(normal,{normal,true}).
%% 
%% 
%% normal + delayable + trace:
%% c(msg_throttling_acker),c(msg_throttling_msger),c(msg_throttling_sup),c(msg_throttling),c(msg_throttling_app),msg_throttling_app:start(normal,{normal,true}),msg_throttling_msger:send_msg1("test1").
%% 
%% 
%% 
%% auto:
%% c(msg_throttling_acker),c(msg_throttling_msger),c(msg_throttling_sup),c(msg_throttling),c(msg_throttling_app),msg_throttling_app:start(normal,{auto,true}).

% -record(args, {mode = normal, delayable_sends = false}).

% -define(SUPPORTED_MODES, [auto, manual])

start(_StartType, {Mode, true} = _StartArgs) -> msg_throttling:start_link(delayable_sends), start_mode(Mode);
start(_StartType, {Mode, false} = _StartArgs) -> msg_throttling:start_link(), start_mode(Mode).
% start(_StartType, #args{mode = _Mode, delayable_sends = true} = _StartArgs) -> msg_throttling:start_link(delayable_sends);
% start(_StartType, #args{mode = _Mode, delayable_sends = false} = _StartArgs) -> msg_throttling:start_link().

start_mode(Mode) ->
    case Mode of
        manual -> ok;
        auto -> auto_run();
        _ -> ok
    end.

    % case True of
    %     true -> msg_throttling:start_link(delayable_sends);
    %     _ -> msg_throttling:start_link()
    % end.

    % io:format("app(~p).\n", [Mode]),
    % case Mode of
    %     manual -> ok;
        
    %     auto ->

    %         ok;

    %     _Else -> 
    %         io:format("app: unexpected mode given (~p).\n", [Mode])
    % end.

% run() -> start(normal, #args{mode = auto, delayable_sends = true}).

auto_run() ->
    % MsgerID = msg_throttling
    ok.

stop(_State) -> ok.

%% internal functions
m_send_msg1(Msg1) -> io:format("m_send_msg1(~p).\n",[Msg1]), ok.
m_send_msg2(Msg2) -> io:format("m_send_msg2(~p).\n",[Msg2]), ok.
m_send_tout(Tout) -> io:format("m_send_tout(~p).\n",[Tout]), ok.
a_send_ack1(Ack1) -> io:format("a_send_ack1(~p).\n",[Ack1]), ok.
a_send_ack2(Ack2) -> io:format("a_send_ack2(~p).\n",[Ack2]), ok.


