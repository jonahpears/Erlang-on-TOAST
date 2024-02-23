-module(msg_throttling_msger).

-behaviour(gen_statem).

-define(NAME, ?MODULE).
% -define(SERVER, msg_throttling).
% -define(COPARTY, ?MODULE).
% -define(COPARTY, msg_throttling_acker).

-record(statem_data, {acker_id, trace = [], msgs = [], state_map = {}, delayable_sends = false}).

-export([start_link/1]).
-export([callback_mode/0,
         init/1,
         custom_init_state/3,
         custom_stop_state/3,
         receive_ack1/0,
         receive_ack2/0,
         recv_after_state2/3,
         recv_after_state4/3,
         send_msg1/1,
         send_msg2/1,
         send_tout/1,
         start_link/0,
         std_state1/3,
         std_state3/3,
         std_state5/3,
         stop/0,
         terminate/3]).

start_link({delayable_sends, True}) -> gen_statem:start_link({local, ?NAME}, ?MODULE, [{delayable_sends, True}], []).
start_link() -> gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([{delayable_sends, True}|_T]) -> {ok, custom_init_state, #{delayable_sends => True}};
init([]) -> {ok, custom_init_state, #{delayable_sends => false}}.


custom_init_state(enter, _OldState, #{delayable_sends := _True} = Data) -> {keep_state, Data, [{state_timeout, 0, finish_init}]};
custom_init_state(state_timeout, finish_init, #{delayable_sends := True} = _Data) -> 
    io:format("msger(~p) waiting.\n", [self()]), 
    receive {_SupID, sup_init, AckerID} -> 
        io:format("msger(~p) received [~p].\n", [self(),AckerID]), 
        {next_state, std_state1, #statem_data{acker_id = AckerID, 
                                  trace = [std_state1], 
                                  msgs = [],
                                  state_map = #{send_msg1 => recv_after_state2,
                                                receive_ack1 => std_state1,
                                                send_msg2 => recv_after_state4,
                                                receive_ack2 => recv_after_state2,
                                                send_tout => custom_stop_state},
                                  delayable_sends = True}} 
    end.

    %% TODO
    % - specify what "ack" is? Msgs use labels? make record?
    % - incoorporate delays for sending





std_state1(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True}) -> 
    io:format("msger (~p ->.)\n", [std_state1]),
    keep_state_and_data;
std_state1(cast, {send_msg1, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    case True of
        true ->
        TimeDelay = rand:uniform(3000 * 2),
        io:format("msger delay(~p ms).\n",[TimeDelay]),
        timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
        receive 
            {delay_stop, TimeDelay, _MoreData} -> io:format("msger delay stopped.\n") 
        end;
        _ -> ok
    end,
    NextState = maps:get(send_msg1, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("msger:handle_common(cast, {send_msg1, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.

recv_after_state2(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) ->
    io:format("msger (~p ->.)\n", [recv_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state3}]};
%% This is a timeout branch:
recv_after_state2(state_timeout, std_state3, Data) ->
    io:format("msger (timeout[~p] -> ~p.)\n", [3000, std_state3]),
    {next_state, std_state3, Data};
recv_after_state2(info, {AckerID, Msg}, #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    NextState = maps:get(receive_ack1, Map),
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("msger:handle_common(cast, {receive_ack1}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.

std_state3(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = _Data) -> 
    io:format("msger (~p ->.)\n", [std_state3]),
    keep_state_and_data;
std_state3(internal, {send_msg2, _Msg2}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) -> 
    {next_state, recv_after_state4, Data};
std_state3(cast, {send_msg2, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    case True of
        true ->
        TimeDelay = rand:uniform(3000 * 2),
        io:format("msger delay(~p ms).\n",[TimeDelay]),
        timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
        receive 
            {delay_stop, TimeDelay, _MoreData} -> io:format("msger delay stopped.\n") 
        end;
        _ -> ok
    end,
    NextState = maps:get(send_msg2, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("msger:handle_common(cast, {send_msg2, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.

recv_after_state4(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) ->
    io:format("msger (~p ->.)\n", [recv_after_state4]),
    {keep_state, Data, [{state_timeout, 3000, std_state5}]};
%% This is a timeout branch:
recv_after_state4(state_timeout, std_state5, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) ->
    io:format("msger (timeout[~p] -> ~p.)\n", [3000, std_state5]),
    {next_state, std_state5, Data};
recv_after_state4(info, {AckerID, Msg}, #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    NextState = maps:get(receive_ack2, Map),
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("msger:handle_common(cast, {receive_ack2}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.

std_state5(enter, _OldState, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = _Data) -> 
    io:format("msger (~p ->.)\n", [std_state5]),
    keep_state_and_data;
std_state5(internal, {send_tout, _Tout}, #statem_data{acker_id = _AckerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) -> 
    {next_state, custom_stop_state, Data};
std_state5(cast, {send_tout, Msg}, 
            #statem_data{acker_id = AckerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    case True of
        true ->
        TimeDelay = rand:uniform(3000 * 2),
        io:format("msger delay(~p ms).\n",[TimeDelay]),
        timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
        receive 
            {delay_stop, TimeDelay, _MoreData} -> io:format("msger delay stopped.\n") 
        end;
        _ -> ok
    end,
    NextState = maps:get(send_tout, Map),
    AckerID ! {self(), Msg},
    NewData = #statem_data{acker_id = AckerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("msger:handle_common(cast, {send_tout, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.


custom_stop_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_stop}]};
custom_stop_state(state_timeout, finish_stop, Data) -> {stop, normal, Data}.



terminate(Reason, State, Data) -> 
    io:format("msger (~p),\n\tReason: ~p,\n\tState: ~p\n\tData: ~p.\n", [terminate,Reason,State,Data]),
    ok.

receive_ack1() -> 
    io:format("msger (~p).\n", [receive_ack1]),
    gen_statem:cast(?NAME, {receive_ack1}).

receive_ack2() -> 
    io:format("msger (~p).\n", [receive_ack2]),
    gen_statem:cast(?NAME, {receive_ack2}).

send_msg1(Msg1) -> 
    io:format("msger (~p -> ~p).\n", [send_msg1, Msg1]),
    gen_statem:cast(?NAME, {send_msg1, Msg1}).

send_msg2(Msg2) -> 
    io:format("msger (~p -> ~p).\n", [send_msg2, Msg2]),
    gen_statem:cast(?NAME, {send_msg2, Msg2}).

send_tout(Tout) -> 
    io:format("msger (~p -> ~p).\n", [send_tout, Tout]),
    gen_statem:cast(?NAME, {send_tout, Tout}).

stop() -> 
    io:format("msger (~p).\n", [stop]),
    gen_statem:stop(?NAME).

