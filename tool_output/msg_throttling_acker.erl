-module(msg_throttling_acker).

-behaviour(gen_statem).

-define(NAME, ?MODULE).
% -define(SERVER, msg_throttling).
% -define(COPARTY, ?MODULE).
% -define(COPARTY, msg_throttling_msger).

-record(statem_data, {msger_id, trace = [], msgs = [], state_map = {}, delayable_sends = false}).

-export([start_link/1]).
-export([callback_mode/0,
         init/1,
         custom_init_state/3,
         custom_stop_state/3,
         receive_msg1/0,
         receive_msg2/0,
         receive_tout/0,
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

start_link({delayable_sends, True}) -> gen_statem:start_link({local, ?NAME}, ?MODULE, [{delayable_sends, True}], []).
start_link() -> gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([{delayable_sends, True}|_T]) -> {ok, custom_init_state, #{delayable_sends => True}};
init([]) -> {ok, custom_init_state, #{delayable_sends => false}}.


custom_init_state(enter, _OldState, #{delayable_sends := _True} = Data) -> {keep_state, Data, [{state_timeout, 0, finish_init}]};
custom_init_state(state_timeout, finish_init, #{delayable_sends := True} = _Data) -> 
    io:format("acker(~p) waiting.\n", [self()]), 
    receive {_SupID, sup_init, MsgerID} -> 
        io:format("acker(~p) received [~p].\n", [self(),MsgerID]), 
        {next_state, std_state1, #statem_data{msger_id = MsgerID,
                                   trace = [std_state1],
                                   msgs = [],
                                   state_map = #{receive_msg1 => send_after_state2,
                                                  send_ack1 => std_state1,
                                                  receive_msg2 => send_after_state5,
                                                  send_ack2 => send_after_state2,
                                                  receive_tout => custom_stop_state}, 
                                   delayable_sends = True}} 
    end.

std_state1(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True}) -> 
    io:format("acker (~p ->.)\n", [std_state1]),
    keep_state_and_data;
std_state1(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    NextState = maps:get(receive_msg1, Map),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("acker:handle_common(cast, {receive_msg1}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.

send_after_state2(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) ->
    io:format("acker (~p ->.)\n", [send_after_state2]),
    {keep_state, Data, [{state_timeout, 3000, std_state4}]};
send_after_state2(internal, {send_ack1, std_state1, _Ack1}, Data) -> {next_state, std_state1, Data};
%% This is a timeout branch:
send_after_state2(state_timeout, std_state4, Data) ->
    io:format("acker (timeout[~p] -> ~p.)\n", [3000, std_state4]),
    {next_state, std_state4, Data};
send_after_state2(cast, {send_ack1, Msg}, 
            #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    case True of
        true ->
        TimeDelay = rand:uniform(3000 * 2),
        io:format("acker delay(~p ms).\n",[TimeDelay]),
        timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
        receive 
            {delay_stop, TimeDelay, _MoreData} -> io:format("acker delay stopped.\n") 
        end;
        _ -> ok
    end,
    NextState = maps:get(send_ack1, Map),
    MsgerID ! {self(), Msg},
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = Msgs, state_map = Map, delayable_sends = True},
    % io:format("acker:handle_common(cast, {send_ack1, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.

std_state4(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = _Data) -> 
    io:format("acker (~p ->.)\n", [std_state4]),
    keep_state_and_data;
std_state4(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    NextState = maps:get(receive_msg2, Map),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("acker:handle_common(cast, {receive_msg2}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.

send_after_state5(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = Data) ->
    io:format("acker (~p ->.)\n", [send_after_state5]),
    {keep_state, Data, [{state_timeout, 3000, std_state7}]};
send_after_state5(internal, {send_ack2, send_after_state2, _Ack2}, Data) -> {next_state, send_after_state2, Data};
%% This is a timeout branch:
send_after_state5(state_timeout, std_state7, Data) ->
    io:format("acker (timeout[~p] -> ~p.)\n", [3000, std_state7]),
    {next_state, std_state7, Data};
send_after_state5(cast, {send_ack2, Msg}, 
            #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    case True of
        true ->
        TimeDelay = rand:uniform(3000 * 2),
        io:format("acker delay(~p ms).\n",[TimeDelay]),
        timer:send_after(TimeDelay, self(), {delay_stop, TimeDelay, []}),
        receive 
            {delay_stop, TimeDelay, _MoreData} -> io:format("acker delay stopped.\n") 
        end;
        _ -> ok
    end,
    NextState = maps:get(send_ack2, Map),
    MsgerID ! {self(), Msg},
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = Msgs, state_map = Map, delayable_sends = True},
    % io:format("acker:handle_common(cast, {send_ack2, ~p}, Data),\n\tData: ~p,\n\tNewData: ~p.\n", [Msg, Data, NewData]),
    {next_state, NextState, NewData}.

std_state7(enter, _OldState, #statem_data{msger_id = _MsgerID, trace = _Trace, msgs = _Msgs, state_map = _Map, delayable_sends = _True} = _Data) -> 
    io:format("acker (~p ->.)\n", [std_state7]),
    keep_state_and_data;
std_state7(info, {MsgerID, Msg}, #statem_data{msger_id = MsgerID, trace = [H|T], msgs = Msgs, state_map = Map, delayable_sends = True} = _Data) ->
    NextState = maps:get(receive_tout, Map),
    NewData = #statem_data{msger_id = MsgerID, trace = [NextState,H] ++ T, msgs = [Msg] ++ Msgs, state_map = Map, delayable_sends = True},
    % io:format("acker:handle_common(cast, {receive_tout}, Data),\n\tData: ~p,\n\tNextState: ~p,\n\tNewData: ~p.\n", [Data, NextState, NewData]),
    {next_state, NextState, NewData}.


custom_stop_state(enter, _OldState, Data) -> {keep_state, Data, [{state_timeout, 0, finish_stop}]};
custom_stop_state(state_timeout, finish_stop, Data) -> {stop, normal, Data}.

terminate(Reason, State, Data) -> 
    io:format("acker (~p),\n\tReason: ~p,\n\tState: ~p\n\tData: ~p.\n", [terminate,Reason,State,Data]),
    ok.

receive_msg1() -> 
    io:format("acker (~p).\n", [receive_msg1]),
    gen_statem:cast(?NAME, {receive_msg1}).

receive_msg2() -> 
    io:format("acker (~p).\n", [receive_msg2]),
    gen_statem:cast(?NAME, {receive_msg2}).

receive_tout() -> 
    io:format("acker (~p).\n", [receive_tout]),
    gen_statem:cast(?NAME, {receive_tout}).

send_ack1(Ack1) -> 
    io:format("acker (~p).\n", [send_ack1]),
    gen_statem:cast(?NAME, {send_ack1, Ack1}).

send_ack2(Ack2) -> 
    io:format("acker (~p).\n", [send_ack2]),
    gen_statem:cast(?NAME, {send_ack2, Ack2}).

stop() -> 
    io:format("acker (~p).\n", [stop]),
    gen_statem:stop(?NAME).

