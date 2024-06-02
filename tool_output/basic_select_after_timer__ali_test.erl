-module('basic_select_after_timer__ali_test.erl').

-file("basic_select_after_timer__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-include("stub.hrl").

-export([]).

%% @doc Adds default empty list for Data.
%% @see run/2.
run(CoParty) -> run(CoParty, []).

%% @doc Called immediately after a successful initialisation.
%% Add any setup functionality here, such as for the contents of Data.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
run(CoParty, Data) -> main(CoParty, Data). %% add any init/start preperations below, before entering main

main(CoParty, Data) ->
    {Data1, _TID_t} = set_timer(t, 5000, Data),
    AwaitSelection = nonblocking_selection(fun select_state2/1, [], self(), t),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                msgA ->
                    CoParty ! {self(), msgA, Payload},
                    receive
                        {CoParty, msg1, Payload_Msg1} ->
                            Data3 = save_msg(msg1, Payload_Msg1, Data2),
                            stopping(CoParty, Data3)
                    end;
                msgB ->
                    CoParty ! {self(), msgB, Payload},
                    receive
                        {CoParty, msg2, Payload_Msg2} ->
                            Data6 = save_msg(msg2, Payload_Msg2, Data2),
                            stopping(CoParty, Data6)
                    end;
                msgC ->
                    CoParty ! {self(), msgC, Payload},
                    receive
                        {CoParty, msg3, Payload_Msg3} ->
                            Data8 = save_msg(msg3, Payload_Msg3, Data2),
                            stopping(CoParty, Data8)
                    end;
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            Data9 = Data2,
            Payload_Timeout = payload,
            CoParty ! {self(), timeout, Payload_Timeout},
            stopping(CoParty, Data9)
    end.

%% @doc Adds default reason 'normal' for stopping.
%% @see stopping/3.
stopping(CoParty, Data) -> stopping(normal, CoParty, Data).

%% @doc Adds default reason 'normal' for stopping.
%% @param Reason is either atom like 'normal' or tuple like {error, more_details_or_data}.
stopping(normal = _Reason, _CoParty, _Data) -> exit(normal);
%% @doc stopping with error.
%% @param Reason is either atom like 'normal' or tuple like {error, Reason, Details}.
%% @param CoParty is the process ID of the other party in this binary session.
%% @param Data is a list to store data inside to be used throughout the program.
stopping({error, Reason, Details}, _CoParty, _Data) when is_atom(Reason) -> erlang:error(Reason, Details);
%% @doc Adds default Details to error.
stopping({error, Reason}, CoParty, Data) when is_atom(Reason) -> stopping({error, Reason, []}, CoParty, Data);
%% @doc stopping with Unexpected Reason.
stopping(Reason, _CoParty, _Data) when is_atom(Reason) -> exit(Reason).

select_state2([]) -> msgA;
select_state2(_Selection) -> expand_this_stub.

get_state2_payload() -> ok.