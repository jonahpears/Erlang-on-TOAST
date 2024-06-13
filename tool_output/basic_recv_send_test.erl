-module(basic_recv_send_test).

-file("basic_recv_send_test", 1).

-define(MONITORED, false).

-define(SHOW_MONITORED, case ?MONITORED of true -> "(monitored) "; _ -> "" end).

-define(SHOW_ENABLED, true).

-define(SHOW(Str, Args, Data), case ?SHOW_ENABLED of true -> printout(Data, ?SHOW_MONITORED++"~p, "++Str, [?FUNCTION_NAME]++Args); _ -> ok end).

-define(SHOW_VERBOSE, ?SHOW_ENABLED and true).

-define(VSHOW(Str, Args, Data), case ?SHOW_VERBOSE of true -> printout(Data, ?SHOW_MONITORED++"(verbose) ~p, "++Str, [?FUNCTION_NAME]++Args); _ -> ok end).

-define(MONITOR_SPEC,
        #{init => state1_std, map => #{state1_std => #{recv => #{msg1 => state2_std}}, state2_std => #{send => #{msgA => stop_state}}}, timeouts => #{},
          resets => #{}}).

-define(PROTOCOL_SPEC, {act, r_msg1, {act, s_msgA, endP}}).

-include("stub.hrl").

-export([init/1, run/1, run/2]).

%% @doc Called to finish initialising process.
%% @see stub_init/1 in `stub.hrl`.
%% If this process is set to be monitored (i.e., temp_monitored()=:=true) then, in the space indicated below setup options for the monitor may be specified, before the session actually commences.
%% Processes wait for a signal from the session coordinator (SessionID) before beginning.
init(Args) ->
    printout("args:\n\t~p.", [Args], Args),
    {ok, Data} = stub_init(Args),
    printout("data:\n\t~p.", [Data], Data),
    CoParty = maps:get(coparty_id, Data),
    SessionID = maps:get(session_id, Data),
    case temp_monitored() =:= true of
        true ->
            CoParty ! {self(), setup_options, {printout, #{enabled => true, verbose => true, termination => true}}},
            CoParty ! {self(), ready, finished_setup},
            temp_vshow("finished setting options for monitor.", [], Data);
        _ -> ok
    end,
    receive
        {SessionID, start} ->
            temp_show("received start signal from session.", [], Data),
            run(CoParty, Data)
    end.

%% @doc Adds default empty list for Data.
%% @see run/2.
run(CoParty) ->
    Data = #{coparty_id => CoParty, timers => #{}, msgs => #{}, logs => #{}},
    temp_vshow("using default Data.", [], Data),
    run(CoParty, Data).

%% @doc Called immediately after a successful initialisation.
%% Add any setup functionality here, such as for the contents of Data.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a map that accumulates data as the program runs, and is used by a lot of functions in `stub.hrl`.
%% @see stub.hrl for helper functions and more.
run(CoParty, Data) ->
    temp_do_show("running...\nData:\t~p.\n", [Data], Data),
    main(CoParty, Data).

%%% @doc the main loop of the stub implementation.
%%% CoParty is the process ID corresponding to either:
%%%   (1) the other party in the session;
%%%   (2) if this process is monitored, then the transparent monitor.
%%% Data is a map that accumulates messages received, sent and keeps track of timers as they are set.
%%% Any loops are implemented recursively, and have been moved to their own function scope.
%%% @see stub.hrl for further details and the functions themselves.
main(CoParty, Data) ->
    temp_show("waiting to recv.", [], Data),
    receive
        {CoParty, msg1 = Label1, Payload1} ->
            Data1 = save_msg(recv, msg1, Payload1, Data),
            temp_show("recv ~p: ~p.", [Label1, Payload1], Data1),
            Payload2 = get_payload2(msgA, Data1),
            CoParty ! {self(), msgA, Payload2},
            Data2 = save_msg(send, msgA, Payload2, Data1),
            temp_show("sent msgA.", [], Data2),
            stopping(CoParty, Data2, normal)
    end.

%%% @doc Adds default reason 'normal' for stopping.
%%% @see stopping/3.
stopping(CoParty, Data) ->
    temp_vshow("\nData:\t~p.", [Data], Data),
    stopping(normal, CoParty, Data).

%%% @doc Adds default reason 'normal' for stopping.
%%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}.
stopping(normal = _Reason, _CoParty, _Data) ->
    temp_vshow("stopping normally.", [], _Data),
    exit(normal);
%%% @doc stopping with error.
%%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.
%%% @param CoParty is the process ID of the other party in this binary session.
%%% @param Data is a list to store data inside to be used throughout the program.
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) ->
    temp_vshow("error, stopping...\nReason:\t~p,\nDetails:\t~p,\nCoParty:\t~p,\nData:\t~p.", [Reason, Details, _CoParty, _Data], _Data),
    erlang:error(Reason, Details);
%%% @doc Adds default Details to error.
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) ->
    temp_vshow("error, stopping...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.", [Reason, _CoParty, _Data], _Data),
    stopping({error, Reason, []}, CoParty, Data);
%%% @doc stopping with Unexpected Reason.
stopping(Reason, _CoParty, _Data) when is_atom(Reason) ->
    temp_vshow("unexpected stop...\nReason:\t~p,\nCoParty:\t~p,\nData:\t~p.", [Reason, _CoParty, _Data], _Data),
    exit(Reason).

get_payload2(_Args) -> extend_with_functionality_for_obtaining_payload.