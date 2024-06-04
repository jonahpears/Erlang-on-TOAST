-module('basic_error_select_after__ali_test.erl').

-file("basic_error_select_after__ali_test.erl", 1).

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
    AwaitSelection = nonblocking_selection(fun select_state1/1, [], self(), 50),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                act_msgA ->
                    CoParty ! {self(), act_msgA, Payload},
                    stopping(CoParty, Data1);
                act_msgB ->
                    CoParty ! {self(), act_msgB, Payload},
                    stopping(CoParty, Data1);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            error(unspecified_error),
            stopping(CoParty, Data4)
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

select_state1([]) -> act_msgA;
select_state1(_Selection) -> expand_this_stub.

get_state1_payload() -> ok.