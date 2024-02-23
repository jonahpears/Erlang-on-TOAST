-module(msg_throttling_acker).

-behaviour(gen_statem).

-define(NAME, ?MODULE).
% -define(SERVER, msg_throttling).
% -define(COPARTY, ?MODULE).
% -define(COPARTY, msg_throttling_msger).

% -define(HANDLE_SEND, ?FUNCTION_NAME(T, C, D) -> handle_send(T, C, D)).
% -define(HANDLE_RECV, ?FUNCTION_NAME(T, C, D) -> handle_recv(T, C, D)).

-record(statem_data, {msger_id, trace = [], msgs = [], state_map = {}}).

-export([callback_mode/0,
         init/1,
         custom_init_state/3,
         custom_stop_state/3,
         receive_msg1/0,
         receive_msg2/0,
         receive_tout/0,
        %  receive_msg1/1,
        %  receive_msg2/1,
        %  receive_tout/1,
        %  send_ack1/2,
        %  send_ack2/2,
         send_ack1/1,
         send_ack2/1,
         send_after_state2/3,
         send_after_state5/3,
         start_link/0,
         std_state1/3,
         std_state4/3,
         std_state7/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

% init([]) -> {ok, std_state1, {}}.
init([]) -> {ok, custom_init_state, {}}.
% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, MsgerID} -> {next_state, std_state1, #{msger_id => MsgerID}} end.
% custom_init_state(enter, _OldState, _Data) -> receive {_SupID, sup_init, MsgerID} -> {next_state, std_state1, {MsgerID}} end.

custom_init_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_init}]};
custom_init_state(state_timeout, finish_init, _Data) -> 
    io:format("acker(~p) waiting.\n", [self()]), 
    receive {_SupID, sup_init, MsgerID} -> 
        io:format("acker(~p) received [~p].\n", [self(),MsgerID]), 
        % {next_state, std_state1, #{msger_id => MsgerID,
        %                            trace => [std_state1],
        %                            msgs => [],
        %                            state_map => #{receive_msg1 => send_after_state2,
        %                                           send_ack1 => std_state1,
        %                                           receive_msg2 => send_after_state5,
        %                                           send_ack2 => send_after_state2,
        %                                           receive_tout => custom_stop_state}}} 
        {next_state, std_state1, #statem_data{msger_id = MsgerID,
                                   trace = [std_state1],
                                   msgs = [],
                                   state_map = #{receive_msg1 => send_after_state2,
                                                  send_ack1 => std_state1,
                                                  receive_msg2 => send_after_state5,
                                                  send_ack2 => send_after_state2,
                                                  receive_tout => custom_stop_state}}} 
    end.

% custom_init_state(internal, _OldState, _Data) -> receive {_SupID, sup_init, MsgerID} -> {next_state, std_state1, #{msger_id => MsgerID}} end.



% -define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).
% -define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, #{msger_id := MsgerID, trace := [H|T], msgs := Msgs, state_map := Map} = D) -> 
%     io:format("HANDLE_COMMON: ~p(~p, ~p, ~p)\n.", [?FUNCTION_NAME, T, C, D]),
%     handle_common(T, C, #{msger_id => MsgerID, trace => [maps:get(?FUNCTION_NAME,Map),H] ++ T, msgs => Msgs, state_map => Map})).


% handle_common(cast, {recv_msg}, 
%             #{msger_id := MsgerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(H, Map),
%     receive
%         {MsgerID, Msg} -> 
%             NewData = #statem_data{msger_id => MsgerID, trace => [NextState,H] ++ T, msgs => [Msg] ++ Msgs, state_map => Map},
%             io:format("acker:handle_common(cast, {recv_msg}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end;


% handle_common(cast, {send_msg, Msg}, 
%             #{msger_id := MsgerID, trace := [H|T], msgs := Msgs, state_map := Map} = Data) ->
%     NextState = maps:get(H, Map),
%     MsgerID ! {self(), Msg},
%     NewData = #statem_data{msger_id => MsgerID, trace => [NextState,H] ++ T, msgs => Msgs},
%     io:format("acker:handle_common(cast, {send_msg, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
%     {next_state, NextState, NewData}.





std_state1(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map}) -> 
    io:format("acker (~p ->.)\n", [std_state1]),
    keep_state_and_data;
% std_state1(internal, {receive_msg1, send_after_state2}, Data) -> {next_state, send_after_state2, Data};
% ?HANDLE_COMMON.
std_state1(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    % io:format("acker:handle_common(info, {~p, ~p}, Data),\n\tData: ~p.\n", [MsgerID, Msg, Data]),
    io:format("acker:handle_common(info): {~p, ~p}.\n", [MsgerID, Msg]),
    NextState = maps:get(receive_msg1, Map),
    % io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
    io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.
    % gen_statem:cast(?NAME, {receive_msg1, Msg});
% std_state1(cast, {receive_msg1}, 
%             #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%     io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p.\n", [Data]),
%     NextState = maps:get(receive_msg1, Map),
%     io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
%     receive
%         {MsgerID, Msg} -> 
%             NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end.
% std_state1(cast, {receive_msg1, Msg}, 
%             #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
% %     io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p.\n", [Data]),
%     NextState = maps:get(receive_msg1, Map),
%     io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
%     NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%     io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%     {next_state, NextState, NewData}.
% std_state1(internal, {receive_msg1, _Msg1}, Data) -> {next_state, send_after_state2, Data};?HANDLE_COMMON.
% std_state1(cast, {receive_msg1, _Msg1}, Data) -> {next_state, send_after_state2, Data}.
% ?HANDLE_RECV.

send_after_state2(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) ->
    io:format("acker (~p ->.)\n", [send_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state4}]};
send_after_state2(internal, {send_ack1, std_state1, _Ack1}, Data) -> {next_state, std_state1, Data};
% send_after_state2(internal, {send_ack1, _Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
send_after_state2(state_timeout, std_state4, Data) ->
    io:format("acker (timeout[~p] -> ~p.)\n", [3000, std_state4]),
    {next_state, std_state4, Data};
    % ?HANDLE_COMMON.
send_after_state2(cast, {send_ack1, Msg}, 
            #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    NextState = maps:get(send_ack1, Map),
    MsgerID ! {self(), Msg},
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = Msgs},
    io:format("acker:handle_common(cast, {send_ack1, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.
% ?HANDLE_SEND.

std_state4(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = _Data) -> 
    io:format("acker (~p ->.)\n", [std_state4]),
    keep_state_and_data;
std_state4(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    % io:format("acker:handle_common(info, {~p, ~p}, Data),\n\tData: ~p.\n", [MsgerID, Msg, Data]),
    io:format("acker:handle_common(info): {~p, ~p}.\n", [MsgerID, Msg]),
    NextState = maps:get(receive_msg2, Map),
    % io:format("acker:handle_common(cast, {receive_msg2}, Data),\n\tData: ~p,\n\tNextState: ~p.\n", [Data, NextState]),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    % io:format("acker:handle_common(cast, {receive_msg2}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
    io:format("acker:handle_common(cast, {receive_msg2}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.
% std_state4(internal, {receive_msg2, send_after_state5}, Data) -> {next_state, send_after_state5, Data};
% % ?HANDLE_COMMON.
% std_state4(cast, {receive_msg2}, 
%             #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%     NextState = maps:get(receive_msg2, Map),
%     receive
%         {MsgerID, Msg} -> 
%             NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             io:format("acker:handle_common(cast, {receive_msg2}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end.
% std_state4(internal, {receive_msg2, _Msg2}, Data) -> {next_state, send_after_state5, Data};?HANDLE_COMMON.
% std_state4(cast, {receive_msg2, _Msg2}, Data) -> {next_state, send_after_state5, Data}.
% ?HANDLE_RECV.

send_after_state5(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = Data) ->
    io:format("acker (~p ->.)\n", [send_after_state5]),
    {keep_state, Data, [{state_timeout, 3000, std_state7}]};
send_after_state5(internal, {send_ack2, send_after_state2, _Ack2}, Data) -> {next_state, send_after_state2, Data};
% send_after_state5(internal, {send_ack2, _Ack2}, Data) -> {next_state, send_after_state2, Data};
%% This is a timeout branch:
send_after_state5(state_timeout, std_state7, Data) ->
    io:format("acker (timeout[~p] -> ~p.)\n", [3000, std_state7]),
    {next_state, std_state7, Data};
    % ?HANDLE_COMMON.
send_after_state5(cast, {send_ack2, Msg}, 
            #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    NextState = maps:get(send_ack2, Map),
    MsgerID ! {self(), Msg},
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = Msgs},
    io:format("acker:handle_common(cast, {send_ack2, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.
% ?HANDLE_SEND.

std_state7(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map} = _Data) -> 
    io:format("acker (~p ->.)\n", [std_state7]),
    keep_state_and_data;
std_state7(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
    % io:format("acker:handle_common(info, {~p, ~p}, Data),\n\tData: ~p.\n", [MsgerID, Msg, Data]),
    io:format("acker:handle_common(info): {~p, ~p}.\n", [MsgerID, Msg]),
    NextState = maps:get(receive_tout, Map),
    % io:format("acker:handle_common(cast, {receive_tout}, Data),\n\tData: ~p.\n", [Data, NextState]),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
    io:format("acker:handle_common(cast, {receive_tout}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.
% std_state7(internal, {receive_tout, custom_stop_state}, Data) -> {next_state, custom_stop_state, Data};
% % ?HANDLE_COMMON.
% std_state7(cast, {receive_tout}, 
%             #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map} = Data) ->
%     NextState = maps:get(receive_tout, Map),
%     receive
%         {MsgerID, Msg} -> 
%             NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map},
%             io:format("acker:handle_common(cast, {receive_tout}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Data, NewData]),
%             {next_state, NextState, NewData}
%     end.
% std_state7(internal, {receive_tout, _Tout}, Data) -> {stop, normal, Data};?HANDLE_COMMON.
% std_state7(cast, {receive_tout, _Tout}, Data) -> {stop, normal, Data}.
% ?HANDLE_RECV.


custom_stop_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_stop}]};
custom_stop_state(state_timeout, finish_stop, Data) -> {stop, normal, Data}.

terminate(Reason, State, Data) -> 
    io:format("acker (~p),\n\tReason: ~p,\n\tState: ~p\n\tData: ~p.\n", [terminate,Reason,State,Data]),
    ok.

% receive_msg1(Msg1) -> 
receive_msg1() -> 
    io:format("acker (~p).\n", [receive_msg1]),
    % gen_statem:cast(?NAME, {recv_msg}).
    gen_statem:cast(?NAME, {receive_msg1}).
    % gen_statem:cast(?NAME, {receive_msg1, Msg1}).

% receive_msg2(Msg2) -> 
receive_msg2() -> 
    io:format("acker (~p).\n", [receive_msg2]),
    % gen_statem:cast(?NAME, {recv_msg}).
    gen_statem:cast(?NAME, {receive_msg2}).
    % gen_statem:cast(?NAME, {receive_msg2, Msg2}).

% receive_tout(Tout) -> 
receive_tout() -> 
    io:format("acker (~p).\n", [receive_tout]),
    % gen_statem:cast(?NAME, {recv_msg}).
    gen_statem:cast(?NAME, {receive_tout}).
    % gen_statem:cast(?NAME, {receive_tout, Tout}).

% send_ack1(Ack1) -> 
send_ack1(Ack1) -> 
    io:format("acker (~p).\n", [send_ack1]),
    % gen_statem:cast(?NAME, {send_msg, Ack1}).
    gen_statem:cast(?NAME, {send_ack1, Ack1}).

% send_ack2(Ack2) -> 
send_ack2(Ack2) -> 
    io:format("acker (~p).\n", [send_ack2]),
    % gen_statem:cast(?NAME, {send_msg, Ack2}).
    gen_statem:cast(?NAME, {send_ack2, Ack2}).

stop() -> 
    io:format("acker (~p).\n", [stop]),
    gen_statem:stop(?NAME).



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

