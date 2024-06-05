-module('advanced_mixed_choice_select_first__ali_test.erl').

-file("advanced_mixed_choice_select_first__ali_test.erl", 1).

-define(MONITORED, false).

-define(MONITOR_SPEC, #{}).

-define(PROTOCOL_SPEC,
        {timer,
         "t1",
         5000,
         {select,
          [{s_first, {act, r_second, endP, aft, "t1", error}}, {s_third, endP}],
          aft,
          3000,
          {branch, [{r_fourth, endP}, {r_fifth, endP}], aft, "t1", {act, s_sixth, endP}}}}).

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
    {Data1, _TID_t1} = set_timer(t1, 5000, Data),
    AwaitSelection = nonblocking_selection(fun select_state2/1, [], self(), 3000),
    receive
        {AwaitSelection, ok, {Label, Payload}} ->
            case Label of
                first ->
                    CoParty ! {self(), first, Payload},
                    receive
                        {CoParty, second, Payload_Second} ->
                            Data3 = save_msg(second, Payload_Second, Data2),
                            stopping(CoParty, Data3);
                        {timeout, _TID_t1, {timer, t1}} ->
                            error(unspecified_error),
                            stopping(CoParty, Data5)
                    end;
                third ->
                    CoParty ! {self(), third, Payload},
                    stopping(CoParty, Data2);
                _ -> error(unexpected_label_selected)
            end;
        {AwaitPayload, ko} ->
            receive
                {CoParty, fourth, Payload_Fourth} ->
                    Data7 = save_msg(fourth, Payload_Fourth, Data2),
                    stopping(CoParty, Data7);
                {CoParty, fifth, Payload_Fifth} ->
                    Data7 = save_msg(fifth, Payload_Fifth, Data2),
                    stopping(CoParty, Data7);
                {timeout, _TID_t1, {timer, t1}} ->
                    Data9 = Data7,
                    Payload_Sixth = payload,
                    CoParty ! {self(), sixth, Payload_Sixth},
                    stopping(CoParty, Data9)
            end
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

select_state2([]) -> first;
select_state2(_Selection) -> expand_this_stub.

get_state2_payload() -> ok.