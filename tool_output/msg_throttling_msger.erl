-module(msg_throttling_msger).

-behaviour(gen_statem).

-define(NAME, ?MODULE).
% -define(SERVER, msg_throttling).
% -define(COPARTY, ?MODULE).
% -define(COPARTY, msg_throttling_acker).


% -define(HANDLE_SEND, ?FUNCTION_NAME(T, C, D) -> handle_send(T, C, D)).
% -define(HANDLE_RECV, ?FUNCTION_NAME(T, C, D) -> handle_recv(T, C, D)).

-record(statem_data, {acker_id, trace = [], msgs = [], state_map = {}}).

-export([callback_mode/0,
         init/1,
         custom_init_state/3,
         custom_stop_state/3,
         receive_ack1/0,
         receive_ack2/0,
        %  receive_ack1/1,
        %  receive_ack2/1,
         recv_after_state2/3,
         recv_after_state4/3,
        %  send_msg1/2,
        %  send_msg2/2,
        %  send_tout/2,
         send_msg1/1,
         send_msg2/1,
         send_tout/1,
         start_link/0,
         std_state1/3,
         std_state3/3,
         std_state5/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

% init([]) -> {ok, std_state1, {}}.
init([]) -> {ok, custom_init_state, {}}.

% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, AckerID} -> {next_state, std_state1, {AckerID}} end.
% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, AckerID} -> {next_state, std_state1, #{acker_id => AckerID}} end.

custom_init_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_init}]};
custom_init_state(state_timeout, finish_init, _Data) -> 
    io:format("msger(~p) waiting.\n", [self()]), 
    receive {_SupID, sup_init, AckerID} -> 
        io:format("msger(~p) received [~p].\n", [self(),AckerID]), 
        % {next_state, std_state1, #{acker_id => AckerID, 
        %                           trace => [std_state1], 
        %                           msgs => [],
        %                           state_map => #{send_msg1 => recv_after_state2,
        %                                          receive_ack1 => std_state1,
        %                                          send_msg2 => recv_after_state4,
        %                                          receive_ack2 => recv_after_state2,
        %                                          send_tout => custom_stop_state}}} 
        {next_state, std_state1, #statem_data{acker_id = AckerID, 
                                  trace = [std_state1], 
                                  msgs = [],
                                  state_map = #{send_msg1 => recv_after_state2,
                                                receive_ack1 => std_state1,
                                                send_msg2 => recv_after_state4,
                                                receive_ack2 => recv_after_state2,
                                                send_tout => custom_stop_state}}} 
    end.

    %% TODO
    % - specify what "ack" is? Msgs use labels? make record?


% -define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = D) -> 
%     io:format("HANDLE_COMMON: ~p(~p, ~p, ~p)\n.", [?FUNCTION_NAME, T, C, D]),
%     handle_common(T, C, #{acker_id => AckerID, trace => [maps:get(?FUNCTION_NAME,Map),H] ++ T, msgs => Msgs, state_map => Map})).
%     % handle_common(T, C, D)).

% handle_common(cast, {recv_msg}, 
%             #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%             % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(H, Map),
%     receive
%         {AckerID, Msg} -> 
%             NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => [Msg] ++ Msgs, state_map => Map},
%             io:format("msger:handle_common(cast, {recv_msg}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end;


% handle_common(cast, {send_msg, Msg}, 
%             #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%             % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(H, Map),
%     AckerID ! {self(), Msg},
%     NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%     % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => Msgs, state_map = Map},
%     io:format("msger:handle_common(cast, {send_msg, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
%     {next_state, NextState, NewData}.






std_state1(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map}) -> 
% std_state1(enter, _OldState, #{acker_id := _AckerID, trace := _Trace, msgs := _Msgs, state_map := _Map}) -> 
    io:format("msger (~p ->.)\n", [std_state1]),
    keep_state_and_data;
% {keep_state, #{acker_id => AckerID, trace => [maps:get(std_state1,Map),H] ++ T, msgs => Msgs, state_map => Map}};
    % {keep_state, #{acker_id => AckerID, trace => Trace, msgs => Msgs, state_map => Map}};
% std_state1(internal, {send_msg1, _Msg1}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) -> 
% % std_state1(internal, {send_msg1, _Msg1}, Data) -> 
% % % std_state1(internal, {send_msg1, _Msg1}, Data) -> 
%     {next_state, recv_after_state2, Data};
    % ?HANDLE_COMMON.
std_state1(cast, {send_msg1, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
            % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
    % io:format("msger:handle_common(cast, {send_msg1, ~p}, Data),\n\tData: ~p.\n", [Msg, Data]),
    NextState = maps:get(send_msg1, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => Msgs, state_map = Map},
    io:format("msger:handle_common(cast, {send_msg1, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.
% ?HANDLE_SEND.

recv_after_state2(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) ->
% recv_after_state2(enter, _OldState, Data) ->
    io:format("msger (~p ->.)\n", [recv_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state3}]};
% recv_after_state2(internal, {receive_ack1, _Ack1}, Data) -> {next_state, std_state1, Data};
% recv_after_state2(cast, {receive_ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
recv_after_state2(state_timeout, std_state3, Data) ->
    io:format("msger (timeout[~p] -> ~p.)\n", [3000, std_state3]),
    {next_state, std_state3, Data};
    % ?HANDLE_COMMON.
recv_after_state2(info, {AckerID, Msg}, #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    % io:format("msger:handle_common(info, {~p, ~p}, Data),\n\tData: ~p.\n", [AckerID, Msg, Data]),
    io:format("msger:handle_common(info): {~p, ~p}.\n", [AckerID, Msg]),
    NextState = maps:get(receive_ack1, Map),
    % io:format("acker:handle_common(cast, {receive_ack1}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % io:format("acker:handle_common(cast, {receive_ack1}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
    io:format("msger:handle_common(cast, {receive_ack1}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.
% recv_after_state2(cast, {receive_ack1}, 
%             #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%             % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(receive_ack1, Map),
%     receive
%         {AckerID, Msg} -> 
%             NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => [Msg] ++ Msgs, state_map => Map},
%             io:format("msger:handle_common(cast, {receive_ack1}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end.
% % ?HANDLE_RECV.

std_state3(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = _Data) -> 
    io:format("msger (~p ->.)\n", [std_state3]),
    keep_state_and_data;
std_state3(internal, {send_msg2, _Msg2}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) -> 
% std_state3(internal, {send_msg2, _Msg2}, Data) -> 
    {next_state, recv_after_state4, Data};
    % ?HANDLE_COMMON.
std_state3(cast, {send_msg2, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
            % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
    % io:format("msger:handle_common(cast, {send_msg2, ~p}, Data),\n\tData: ~p.\n", [Msg, Data]),
    NextState = maps:get(send_msg2, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => Msgs, state_map = Map},
    io:format("msger:handle_common(cast, {send_msg2, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.
% ?HANDLE_SEND.

recv_after_state4(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) ->
    io:format("msger (~p ->.)\n", [recv_after_state4]),
    {keep_state, Data, [{state_timeout, 3000, std_state5}]};
% recv_after_state4(internal, {receive_ack2, _Ack2}, Data) -> {next_state, recv_after_state2, Data};
% recv_after_state4(cast, {receive_ack2}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) -> {next_state, recv_after_state2, Data};
% recv_after_state4(cast, {receive_ack2, _Ack2}, Data) -> {next_state, recv_after_state2, Data};
%% This is a timeout branch:
recv_after_state4(state_timeout, std_state5, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) ->
    io:format("msger (timeout[~p] -> ~p.)\n", [3000, std_state5]),
    {next_state, std_state5, Data};
    % ?HANDLE_COMMON.
recv_after_state4(info, {AckerID, Msg}, #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    % io:format("msger:handle_common(info, {~p, ~p}, Data),\n\tData: ~p.\n", [AckerID, Msg, Data]),
    io:format("msger:handle_common(info): {~p, ~p}.\n", [AckerID, Msg]),
    NextState = maps:get(receive_ack2, Map),
    % io:format("acker:handle_common(cast, {receive_ack2}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % io:format("acker:handle_common(cast, {receive_ack2}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
    io:format("msger:handle_common(cast, {receive_ack2}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.
% recv_after_state4(cast, {receive_ack2}, 
%             #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%             % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(receive_ack2, Map),
%     receive
%         {AckerID, Msg} -> 
%             NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => [Msg] ++ Msgs, state_map => Map},
%             io:format("msger:handle_common(cast, {receive_ack2}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end.
% ?HANDLE_RECV.

std_state5(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = _Data) -> 
    io:format("msger (~p ->.)\n", [std_state5]),
    keep_state_and_data;
std_state5(internal, {send_tout, _Tout}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) -> 
% std_state5(internal, {send_tout, _Tout}, Data) -> 
    {next_state, custom_stop_state, Data};
    % ?HANDLE_COMMON.
std_state5(cast, {send_tout, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
            % #{acker_id := AckerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
    % io:format("msger:handle_common(cast, {send_tout, ~p}, Data),\n\tData: ~p.\n", [Msg, Data]),
    NextState = maps:get(send_tout, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % NewData = #{acker_id => AckerID, trace => [NextState,H] ++ T, msgs => Msgs, state_map = Map},
    io:format("msger:handle_common(cast, {send_tout, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.
    % {stop, normal, Data};?HANDLE_COMMON.
% ?HANDLE_SEND.


custom_stop_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_stop}]};
custom_stop_state(state_timeout, finish_stop, Data) -> {stop, normal, Data}.



terminate(Reason, State, Data) -> 
    io:format("msger (~p),\n\tReason: ~p,\n\tState: ~p\n\tData: ~p.\n", [terminate,Reason,State,Data]),
    ok.

% receive_ack1(Ack1) -> 
receive_ack1() -> 
    io:format("msger (~p).\n", [receive_ack1]),
    % gen_statem:cast(?NAME, {recv_msg}).
    gen_statem:cast(?NAME, {receive_ack1}).

% receive_ack2(Ack2) -> 
receive_ack2() -> 
    io:format("msger (~p).\n", [receive_ack2]),
    % gen_statem:cast(?NAME, {recv_msg}).
    gen_statem:cast(?NAME, {receive_ack2}).

% send_msg1(Msg1) -> 
send_msg1(Msg1) -> 
    io:format("msger (~p -> ~p).\n", [send_msg1, Msg1]),
    % gen_statem:cast(?NAME, {send_msg, Msg1}).
    gen_statem:cast(?NAME, {send_msg1, Msg1}).

% send_msg2(Msg2) -> 
send_msg2(Msg2) -> 
    io:format("msger (~p -> ~p).\n", [send_msg2, Msg2]),
    % gen_statem:cast(?NAME, {send_msg, Msg2}).
    gen_statem:cast(?NAME, {send_msg2, Msg2}).

% send_tout(Tout) -> 
send_tout(Tout) -> 
    io:format("msger (~p -> ~p).\n", [send_tout, Tout]),
    % gen_statem:cast(?NAME, {send_msg, Tout}).
    gen_statem:cast(?NAME, {send_tout, Tout}).

stop() -> 
    io:format("msger (~p).\n", [stop]),
    gen_statem:stop(?NAME).


% handle_send({call,_From}, {SendAction, Msg}, #{acker_id := AckerID, state_trace := Trace} = Data) -> 
%     io:format("msger (~p): ~p:~p\n\ttrace: ~p.\n", [handle_send, SendAction,Msg,Trace]),
%     AckerID ! {self(), Msg},
%     {keep_state, Data#{acker_id => AckerID, state_trace => [SendAction] ++ Trace}}.

% handle_recv(info, Msg, _Data) -> 
%     io:format("msger (~p,~p): ~p.\n", [handle_recv, info, Msg]),
%     keep_state_and_data;

% handle_recv(cast, {RecvAction, Msg}, #{acker_id := AckerID, state_trace := Trace} = Data) -> 
%     io:format("msger (~p): ~p:~p\n\ttrace: ~p.\n", [handle_recv, RecvAction,Msg,Trace]),
%     receive 
%         {AckerID, Msg} -> {keep_state, Data#{acker_id => AckerID, state_trace => [RecvAction] ++ Trace}}
%     end.
