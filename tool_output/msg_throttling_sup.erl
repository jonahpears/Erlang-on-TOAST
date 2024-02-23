-module(msg_throttling_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([start_link/0,init/1]).
-export([stop/0]).
% -export([child/1]).
-export([child/2]).
-export([start/0]).

% -export([attach_msger/1,attach_acker/1]).
% -export([detach_msger/1,detach_acker/1]).

% -define(SERVER,msg_throttling).
-define(SERVER,?MODULE).

% start_link() -> supervisor:start_link({local,?MODULE}, ?MODULE, []).
start_link(delayable_sends) -> supervisor:start_link({local,?SERVER}, ?MODULE, [delayable_sends]).
start_link() -> supervisor:start_link({local,?SERVER}, ?MODULE, []).

stop() -> exit(whereis(?MODULE), shutdown).

init([delayable_sends|_T]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2,
                 perior => 3600},
    ChildSpecList = [child(msg_throttling_acker, [{delayable_sends, true}]),
                     child(msg_throttling_msger, [{delayable_sends, true}])],
    {ok, {SupFlags, ChildSpecList}};

init([]) -> 
    % hlr:new(),
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2,
                 perior => 3600},
    ChildSpecList = [child(msg_throttling_acker, [{delayable_sends, false}]),
                     child(msg_throttling_msger, [{delayable_sends, false}])],
    {ok, {SupFlags, ChildSpecList}}.
    % {ok, {SupFlags, []}}.

child(Module, Args) ->
    #{id => Module,
     start => {Module, start_link, Args},
     restart => transient,
    %  restart => permanent,
     shutdown => 2000,
     type => worker,
     modules => [Module]}.

start() ->

    io:format("msg_throttling_sup:self(): ~p.\n", [self()]),

    SupChildren = supervisor:which_children(?MODULE),

    MsgerIDs = lists:filter(fun(Elem) ->
        case Elem of
            {_, _, _, [msg_throttling_msger]} -> true;
            _Else -> false
        end end, SupChildren),


    AckerIDs = lists:filter(fun(Elem) ->
        case Elem of
            {_, _, _, [msg_throttling_acker]} -> true;
            _Else -> false
        end end, SupChildren),

    ElemMsgerID = lists:last(MsgerIDs),
    ElemAckerID = lists:last(AckerIDs),

    {_, MsgerID, _, _} = ElemMsgerID,
    {_, AckerID, _, _} = ElemAckerID,

    io:format("msg_throttling_sup MsgerID: ~p\n", [MsgerID]),
    io:format("msg_throttling_sup AckerID: ~p\n", [AckerID]),

    MsgerID ! {self(), sup_init, AckerID},
    AckerID ! {self(), sup_init, MsgerID},

    io:format("msg_throttling_sup:start() complete.\n").

% attach_msger(Ms) ->
%     case hlr:lookup_id(Ms) of
%         {ok, _Pid} -> {error, msger_attached};
%         _NotAttached -> supervisor:start_child(?MODULE, [Ms])
%     end.

% attach_acker(Ms) ->
%     case hlr:lookup_id(Ms) of
%         {ok, _Pid} -> {error, acker_attached};
%         _NotAttached -> supervisor:start_child(?MODULE, [Ms])
%     end.


% detach_msger(Ms) ->
%     case hlr:lookup_id(Ms) of
%         {ok, Pid} -> supervisor:terminate_child(?MODULE, Pid);
%         _NotAttached -> {error, msger_detached}
%     end.

% detach_acker(Ms) ->
%     case hlr:lookup_id(Ms) of
%         {ok, Pid} -> supervisor:terminate_child(?MODULE, Pid);
%         _NotAttached -> {error, acker_detached}
%     end.

