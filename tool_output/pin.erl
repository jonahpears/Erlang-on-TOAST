-module(pin).

-file("pin", 1).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_fail/1,
         act_ok/1,
         callback_mode/0,
         custom_end_state/3,
         init/1,
         init/2,
         init_setup_state/3,
         issue_timeout/2,
         recv/1,
         send/2,
         start_link/0,
         start_link/1,
         state1_std/3,
         state2_choice/3,
         stop/0,
         terminate/3]).

-record(statem_options, {allow_delayable_sends = false, printout_enabled = true, persistent_queue = false}).

-record(statem_data, {coparty_id = undefined, state_stack = [], msgs = #{}, queued_actions = [], options = #statem_options{}}).

-record(stop_data, {reason = undefined, statem_data = #statem_data{}}).

printout(Str, Params) -> io:format("[~p|~p]: " ++ Str ++ "\n", [?MODULE, self()] ++ Params).

start_link(Params) -> gen_statem:start_link({local, ?SERVER}, ?MODULE, Params, []).

start_link() -> ?MODULE:start_link([]).

callback_mode() -> [state_functions, state_enter].

send(Label, Msg) ->
    printout("~p: {~p, ~p}.", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?MODULE, {send, {Label, Msg}}).

recv(Label) ->
    printout("~p: ~p.", [?FUNCTION_NAME, Label]),
    gen_statem:call(?MODULE, {recv, Label}).

issue_timeout(Label, Msg) ->
    printout("~p: {~p, ~p}.", [?FUNCTION_NAME, Label, Msg]),
    gen_statem:cast(?MODULE, {issue_timeout, {Msg, Label}}).

init([{HKey, HVal} | T]) when is_atom(HKey) and is_map_key(HKey, #statem_options{}) -> init(T, #statem_data{options = maps:put(HKey, HVal, #statem_options{})});
init([_H | T]) -> init(T, #statem_data{});
init([]) -> {ok, init_setup_state, #statem_data{}}.

init([{HKey, HVal} | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = Options} = Data)
    when is_atom(HKey) and is_map_key(HKey, Options) ->
    init(T, maps:put(options, maps:put(HKey, HVal, Options), Data));
init([_H | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> init(T, Data);
init([_H | T], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) -> init(T, Data);
init([], #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = Data) ->
    {ok, init_setup_state, Data}.

init_setup_state(enter, _OldState,
                 #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    {keep_state_and_data, [{state_timeout, 0, wait_to_finish_setup}]};
init_setup_state(state_timeout, wait_to_finish_setup,
                 #statem_data{coparty_id = _CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options} = _Data) ->
    printout("~p, waiting to finish setup.", [?FUNCTION_NAME]),
    receive
        {_SupervisorID, sup_init, CoPartyID} ->
            printout("~p, received coparty ID (~p).", [?FUNCTION_NAME, CoPartyID]),
            {next_state, state1_std, #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue, options = Options}}
    end.

state1_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state1_std(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state1_std(state_timeout, process_queue,
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
state1_std(info, {CoPartyID, {pin, Pin}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, recv (~p) from ~p.", [?FUNCTION_NAME, {pin, Pin}, CoPartyID]),
    NextState = state2_choice,
    case maps:find(pin, Msgs) of
        {ok, StateMsgs} -> Msgs1 = maps:put(pin, [Pin] ++ StateMsgs, Msgs);
        error -> Msgs1 = maps:put(pin, [Pin], Msgs)
    end,
    case PersistentQueue of
        false -> Queue1 = [];
        true -> Queue1 = Queue
    end,
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = [NextState] ++ States, msgs = Msgs1, queued_actions = Queue1, options = Options},
    {next_state, NextState, Data1};
state1_std({call, From}, {recv, Label},
           #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
state1_std(cast, {send, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                Options} =
               _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
state1_std(info, {CoPartyID, {Label, Msg}},
           #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                        options =
                            #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                            persistent_queue = _PersistentQueue} =
                                _Options} =
               _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

state2_choice(enter, _OldState,
              #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [_H], options = _Options} = _Data) ->
    printout("(->) ~p, queued actions.", [?FUNCTION_NAME]),
    {keep_state_and_data, [{state_timeout, 0, process_queue}]};
state2_choice(enter, _OldState, #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = [], options = _Options} = _Data) ->
    printout("(->) ~p.", [?FUNCTION_NAME]),
    keep_state_and_data;
state2_choice(state_timeout, process_queue,
              #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = [H | T], options = Options} = _Data) ->
    printout("(->) ~p, queued action: ~p.", [?FUNCTION_NAME, H]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = T, options = Options},
    {repeat_state, Data1, [{next_event, cast, {send, H}}]};
%assert pin
state2_choice(cast, {act_ok, Ok}, Data) -> {next_state, custom_end_state, Data};
state2_choice(cast, {act_fail, Fail}, Data) -> {next_state, custom_end_state, Data};
state2_choice({call, From}, {recv, Label},
              #statem_data{coparty_id = _CoPartyID, state_stack = _States, msgs = Msgs, queued_actions = _Queue, options = _Options} = _Data) ->
    printout("~p, looking for msg with label (~p).", [?FUNCTION_NAME, Label]),
    case maps:find(Label, Msgs) of
        {ok, [H]} ->
            printout("~p, found msg (~p: ~p) out of 1.", [?FUNCTION_NAME, Label, H]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => 1, tail => []}};
        {ok, [H | T]} ->
            NumMsgs = length(T) + 1,
            printout("~p, found msg (~p: ~p) out of ~p.", [?FUNCTION_NAME, Label, H, NumMsgs]),
            ReplyMsg = {ok, #{label => Label, msg => H, total => NumMsgs, tail => T}};
        error ->
            printout("~p, no msgs with label (~p) found.", [?FUNCTION_NAME, Label]),
            ReplyMsg = {error, no_msg_found_under_label}
    end,
    {keep_state_and_data, [{reply, From, ReplyMsg}]};
state2_choice(cast, {send, {Label, Msg}},
              #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue,
                           options =
                               #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                               persistent_queue = _PersistentQueue} =
                                   Options} =
                  _Data) ->
    printout("~p, early send -- add to queue: ~p.", [?FUNCTION_NAME, {Label, Msg}]),
    Data1 = #statem_data{coparty_id = CoPartyID, state_stack = States, msgs = Msgs, queued_actions = Queue ++ [{Label, Msg}], options = Options},
    {keep_state, Data1};
state2_choice(info, {CoPartyID, {Label, Msg}},
              #statem_data{coparty_id = CoPartyID, state_stack = _States, msgs = _Msgs, queued_actions = _Queue,
                           options =
                               #statem_options{allow_delayable_sends = _AllowDelayableSends, printout_enabled = _PrintoutEnabled,
                                               persistent_queue = _PersistentQueue} =
                                   _Options} =
                  _Data) ->
    printout("~p, early recv (~p) -- postponing.", [?FUNCTION_NAME, {Label, Msg}]),
    {keep_state_and_data, [postpone]}.

custom_end_state(enter, _OldState, #stop_data{reason = _Reason, statem_data = _StatemData} = _Data) ->
    {keep_state_and_data, [{state_timeut, 0, go_to_terminate}]};
custom_end_state(state_timeout, go_to_terminate, #stop_data{reason = Reason, statem_data = StatemData} = Data) ->
    printout("(->) ~p,\nReason: ~p,\nStatemData: \n\t~p.", [?FUNCTION_NAME, Reason, StatemData]),
    {stop, Reason, Data}.

terminate(Reason, State, Data) ->
    printout("(->) ~p, ~p, Reason: ~p,\nData: \n\t~p.", [?FUNCTION_NAME, State, Reason, Data]),
    ok.

act_fail(Fail) -> gen_statem:cast(?SERVER, {act_fail, Fail}).

act_ok(Ok) -> gen_statem:cast(?SERVER, {act_ok, Ok}).

stop() -> gen_statem:stop(?SERVER).