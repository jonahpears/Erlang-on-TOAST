-module(msg_throttling).

-behaviour(gen_server).

%% c(msg_throttling_acker),c(msg_throttling_msger),c(msg_throttling_sup),c(msg_throttling),msg_throttling:start_link().
%% msg_throttling:stop().
%% 
%% msg_throttling_msger:send_msg1("test1").

%% API
-export([start_link/0, compose/3, factorize/2, generate/2, extract/1]).

-export([stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  io:format("\n\n\n\n\n-----[~p]-----\nmsg_throttling:start_link() starting...\n\n", [erlang:timestamp()]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> exit(whereis(?MODULE), shutdown).

init([]) ->
    msg_throttling_sup:start_link(),

    % ChildSpecList = [child(msg_throttling_acker),
    %                  child(msg_throttling_msger)],
    % supervisor:start_child(msg_throttling_sup, ChildSpecList),

    msg_throttling_sup:start(),
    io:format("\n"),
    {ok, #state{}}.


% child(Module) ->
%     #{id => Module,
%      start => {Module, start_link, []},
%      restart => permanent,
%      shutdown => 2000,
%      type => worker,
%      modules => [Module]}.



compose(FirstProtocol, SecondProtocol, WeakFlag)->
  gen_server:call(?MODULE, {compose, FirstProtocol, SecondProtocol, WeakFlag}).

factorize(FirstProtocol, SecondProtocol)->
  gen_server:call(?MODULE, {factorize, FirstProtocol, SecondProtocol}).

generate(Protocol, FileName)->
  gen_server:call(?MODULE, {generate, Protocol, FileName}).

extract(FileName)->
  gen_server:call(?MODULE, {extract, FileName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
% init([]) ->
%   {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




% -define(HANDLE_SEND, ?FUNCTION_NAME(T, C, D) -> handle_send(T, C, D)).
% -define(HANDLE_RECV, ?FUNCTION_NAME(T, C, D) -> handle_recv(T, C, D)).








% handle_send({call,From}, {SendAction, Msg}, #{msger_id := MsgerID, state_trace := Trace} = Data) -> 
%     io:format("acker (~p): ~p:~p\n\ttrace: ~p.\n", [handle_send, SendAction,Msg,Trace]),
%     MsgerID ! {self(), Msg},
%     {keep_state, Data#{msger_id => MsgerID, state_trace => [SendAction] ++ Trace},[{reply,From,Msg}]}.

% handle_recv(info, Msg, _Data) -> 
%     io:format("acker (~p,~p): ~p.\n", [handle_recv, info,Msg]),
%     {keep_state_and_data,[{reply,}]};

% handle_recv(cast, {RecvAction, Msg}, #{msger_id := MsgerID, state_trace := Trace} = Data) -> 
%     io:format("acker (~p,~p): ~p:~p\n\ttrace: ~p.\n", [handle_recv, cast, RecvAction,Msg,Trace]),
%     receive 
%         {MsgerID, Msg} -> {keep_state, Data#{msger_id => MsgerID, state_trace => [RecvAction] ++ Trace}}
%     end.

