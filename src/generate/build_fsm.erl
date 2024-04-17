%% @doc Process a protocol structure into fsm edges and nodes
-module(build_fsm).

-export([to_fsm/1]).

-include("reng.hrl").

standard_state() -> standard_state.
choice_state() -> choice_state.
% mixed_choice_state() -> mixed_choice_state.
end_state() -> end_state.
custom_end_state() -> custom_end_state.

recv_after_state() -> recv_after_state.
branch_after_state() -> branch_after_state.
send_after_state() -> send_after_state.
select_after_state() -> select_after_state.
% after_recv_state() -> after_recv_state.
% after_branch_state() -> after_branch_state.
% after_send_state() -> after_send_state.
after_state() -> after_state.
unknown_state() -> unknown_state.

%% @doc wrapper function that initializes the main to_fsm function
%% takes a protocol and returns a list of transitions/edges and nodes/states
-spec to_fsm(interleave:protocol()) -> {list(), map()}.
to_fsm(P) ->
    Edge = #edge{
        from = 0,
        to = 1,
        edge_data = #edge_data{
            event = init,
            event_type = init
        },
        is_silent = false,
        is_delayable_send = false,
        is_custom_end = false
    },

    {Edges, Nodes, _, _, _, _, _} = to_fsm(
        P,
        [Edge],
        maps:put(0, init_state, maps:new()),
        maps:new(),
        1,
        1,
        -1,
        maps:put(
            0,
            #clock{
                label = "g",
                is_global = true,
                value = #clock_value{
                    is_abs = true,
                    upper_bound = 0
                }
            },
            maps:new()
        )
    ),

    {Edges, Nodes}.

%% @doc processes the actions and labels names and sets the event,
%% variable name and action accordingly. When the name starts with r_ that
%% represents a receive action, with a s_ it represents a send action
-spec args(atom()) -> {atom(), string(), atom()}.
args(Param) ->
    Str = atom_to_list(Param),
    Recv = string:find(Str, "r_"),
    Send = string:find(Str, "s_"),

    io:format("\n\n\nparam: ~p\nstr: ~p\nrecv: ~p\nsend: ~p\n\n\n", [Param, Str, Recv, Send]),

    if
        Recv =:= Str ->
            Event = cast,
            TransType = recv,
            Var = lists:last(string:split(Str, "r_")),
            Act = list_to_atom("receive_" ++ Var);
        Send =:= Str ->
            Event = internal,
            TransType = send,
            Var = lists:last(string:split(Str, "s_")),
            Act = list_to_atom("send_" ++ Var);
        true ->
            Event = cast,
            TransType = action,
            Var = Str,
            Act = list_to_atom("act_" ++ Var)
    end,
    {Act, string:titlecase(Var), Event, TransType}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom()) -> {atom(), string(), atom()}.
data(Param) ->
    io:format("param: ~p.", [Param]),
    {Act, Var, Event, TransType} = args(Param),
    #edge_data{event = {Act, list_to_atom(Var)}, event_type = Event, trans_type = TransType}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom(), interleave:time_constraint()) -> {atom(), string(), atom()}.
data(Param, Constraint) ->
    io:format("time constraint, param: ~p; constraint: ~p.", [Param, Constraint]),
    {Act, Var, Event, TransType} = args(Param),
    #edge_data{
        event = {Act, list_to_atom(Var)},
        event_type = Event,
        trans_type = TransType,
        timeout = Constraint
    }.

%% @doc transform the protocol to a list of transitions between states and a
%% map of nodes, easier to work with in generate
-spec to_fsm(
    interleave:protocol(),
    list(),
    map(),
    map(),
    integer(),
    integer(),
    % ! added clocks
    integer(),
    map()
) -> {list(), map(), map(), integer(), integer(), integer(), map()}.

to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Edge = #edge{
        from = PrevVis,
        to = Index,
        edge_data = data(Act),
        is_silent = false,
        is_delayable_send = false,
        is_custom_end = false
    },
    Edges1 = Edges ++ [Edge],
    Nodes1 = maps:put(PrevVis, standard_state(), Nodes),
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);
%% @doc generic timeout state (only called after main construct)
to_fsm({aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % +1,
    Index = PrevIndex,
    %% PrevVis contains index of node to transition from
    Edge = #edge{
        from = PrevVis,
        to = Index,
        edge_data = #edge_data{timeout = Timeout, comments = []},
        is_silent = true,
        is_delayable_send = false,
        is_custom_end = false
    },
    Edges1 = Edges ++ [Edge],
    %% TODO figure out why we do the below:
    % Nodes1 = maps:put(PrevVis, after_state(), Nodes),
    io:format(
        "\n[{aft, Timeout, Q}]:...\n\tEdges: ~p\n\tNodes: ~p\n\tPrevIndex: ~p\n\tPrevVis: ~p\n\tEndIndex: ~p.\n",
        [Edges, Nodes, PrevIndex, PrevVis, EndIndex]
    ),
    Nodes1 = maps:put(PrevIndex, after_state(), Nodes),
    io:format("\n[{aft, Timeout, Q}], Nodes1: ~p.\n", [Nodes1]),
    {Edges2, Nodes2, RecMap2, Index2, Index2, EndIndex2, Clocks2} = to_fsm(
        Q, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
    ),
    {Edges2, Nodes2, RecMap2, Index2, Index2, EndIndex2, Clocks2};
%% @doc single act with timeout
to_fsm({act, Act, P, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % Edge = #edge{from = PrevVis, to = Index, edge_data = data(Act)},
    % Edges1 = Edges ++ [Edge],
    %
    Index = PrevIndex + 1,
    EdgeData = data(Act, Timeout),
    case EdgeData#edge_data.trans_type of
        %% if recv, add timeout to silent transition between states ?
        recv ->
            Edge1 = #edge{
                from = PrevVis,
                to = Index,
                edge_data = EdgeData,
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            Edges1 = Edges ++ [Edge1],
            Nodes1 = maps:put(PrevVis, recv_after_state(), Nodes),
            {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = to_fsm(
                P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
            ),
            %% still pass on the current nodes index in PrevVis to after, to point back to this
            {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3} = to_fsm(
                {aft, Timeout, Q},
                Edges2,
                Nodes2,
                RecMap2,
                PrevIndex2,
                PrevIndex,
                EndIndex2,
                Clocks2
            ),
            io:format(
                "\n[recv {act, Act, P, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
                [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
            ),
            {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};
        send ->
            % io:format("\n[unhandled send] act aft: ~p.", [[{act,Act,P},{aft,Timeout,Q}]]),
            %% TEMP: act as normal

            %!<- changing this to false
            Edge1 = #edge{
                from = PrevVis,
                to = Index,
                edge_data = EdgeData,
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            % Edge1 = #edge{from = PrevVis, to = Index, edge_data = EdgeData, is_silent = false, is_delayable_send = true},
            Edges1 = Edges ++ [Edge1],
            Nodes1 = maps:put(PrevVis, send_after_state(), Nodes),
            {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = to_fsm(
                P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
            ),
            % io:format("\n[2send {act, Act, P, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n", [Edges2, Nodes2, PrevIndex2, PrevVis2, EndIndex2]),
            %% still pass on the current nodes index in PrevVis to after, to point back to this
            %% TODO why PrevIndex2+1?
            {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3} = to_fsm(
                {aft, Timeout, Q},
                Edges2,
                Nodes2,
                RecMap2,
                PrevIndex2 + 1,
                PrevIndex,
                EndIndex2,
                Clocks2
            ),
            io:format(
                "\n[send {act, Act, P, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
                [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
            ),
            {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};
        _Else ->
            io:format("\n[unhandled trans_type] act aft: ~p.", [[{act, Act, P}, {aft, Timeout, Q}]]),
            %% TEMP: act as normal
            Edge = #edge{
                from = PrevVis,
                to = Index,
                edge_data = data(Act),
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            Edges1 = Edges ++ [Edge],
            Nodes1 = maps:put(PrevVis, unknown_state(), Nodes),
            to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks)
    end;
%% @doc branching receive
to_fsm({branch, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    % case lists:last(Branches) of
    %   {_, _} ->
    lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = data(Label),
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            to_fsm(P1, E1, N, R, I1, I1, EI, CI)
        end,
        {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks},
        Branches
    );
%   {_, _, _} ->
%     lists:foldl(fun({Label, P1, Constraint}, {E, N, R, I, _, EI, CI}) ->
%       I1 = I + 1,
%       EdgeData = data(Label, Constraint),
%       Edge = #edge{from = PrevVis, to = I1, edge_data = EdgeData, is_silent = false},
%       E1 = E ++ [Edge],
%       to_fsm(P1, E1, N, R, I1, I1, EI, CI) end,
%       {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks}, Branches)
% end;

%% @doc branching receive with timeout
to_fsm(
    {branch, Branches, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks
) ->
    Index = PrevIndex + 1,
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    Nodes1 = maps:put(PrevVis, branch_after_state(), Nodes),
    %% fully explore the other branches first
    {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = data(Label),
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            to_fsm(P1, E1, N, R, I1, I1, EI, CI)
        end,
        {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks},
        Branches
    ),
    %% still pass on the current nodes index in PrevVis to after, to point back to this
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3} = to_fsm(
        {aft, Timeout, Q}, Edges2, Nodes2, RecMap2, PrevIndex2, PrevIndex, EndIndex2, Clocks2
    ),
    io:format(
        "\n[{branch, Branches, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
        [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
    ),
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};

%% @doc output selection
to_fsm({select, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, choice_state(), Nodes),

    lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = data(Label),
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            to_fsm(P1, E1, N, R, I1, I1, EI, CI)
        end,
        {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks},
        Branches
    );
%% @doc output selection with timeout
to_fsm(
    {select, Branches, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks
) ->
    Index = PrevIndex + 1,
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    Nodes1 = maps:put(PrevVis, select_after_state(), Nodes),
    %% fully explore the other branches first
    {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = data(Label),
                is_silent = false,
                is_delayable_send = false,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            to_fsm(P1, E1, N, R, I1, I1, EI, CI)
        end,
        {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks},
        Branches
    ),
    %% still pass on the current nodes index in PrevVis to after, to point back to this
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3} = to_fsm(
        {aft, Timeout, Q}, Edges2, Nodes2, RecMap2, PrevIndex2, PrevIndex, EndIndex2, Clocks2
    ),
    io:format(
        "\n[{select, Branches, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
        [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
    ),
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};


to_fsm({rec, BoundVar, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    RecMap1 = maps:put(BoundVar, PrevVis, RecMap),
    to_fsm(P, Edges, Nodes, RecMap1, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({rvar, Var}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    RecIndex = maps:get(Var, RecMap),
    LastEdge = lists:last(Edges),
    Edge = LastEdge#edge{to = RecIndex},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks};
%% @doc to_fsm(endP, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex)
%% iff EndIndex is uninitialised (-1)
%%  -> then this node is now EndIndex
%% else EndIndex is initialised (i.e., not -1, corresponds to other endP ID)
%%  -> then change the edge from the parent node to point to the already initialised endP Node
to_fsm(endP, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    if
        EndIndex =:= -1 ->
            Nodes1 = maps:put(PrevVis, custom_end_state(), Nodes),
            Index = PrevIndex + 1,
            Nodes2 = maps:put(Index, end_state(), Nodes1),
            Edges1 =
                Edges ++
                    [
                        #edge{
                            from = PrevVis,
                            to = Index,
                            edge_data = #edge_data{timeout = 0, comments = ["% is end"]},
                            is_silent = true,
                            is_delayable_send = false,
                            is_custom_end = true
                        }
                    ],
            {Edges1, Nodes2, RecMap, PrevIndex, PrevVis, PrevVis, Clocks};
        EndIndex =/= -1 ->
            LastEdge = lists:last(Edges),
            Edge = LastEdge#edge{to = EndIndex},
            Edges1 = lists:droplast(Edges) ++ [Edge],
            {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks}
    end;
to_fsm({assert, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{
        comments = LastEdge#edge.edge_data#edge_data.comments ++ [{assert, N}]
    },
    Edge = LastEdge#edge{edge_data = EdgeData},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({require, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{
        comments = LastEdge#edge.edge_data#edge_data.comments ++ [{require, N}]
    },
    Edge = LastEdge#edge{edge_data = EdgeData},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({consume, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{
        comments = LastEdge#edge.edge_data#edge_data.comments ++ [{consume, N}]
    },
    Edge = LastEdge#edge{edge_data = EdgeData},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({set, Name, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    %% TODO apply reset on
    %% ! (1: last act if above),
    %% ! (2: all incoming actions to state if rec)
    ClockName = "c_" ++ Name,
    Clocks1 = maps:put(
        ClockName,
        #clock{
            label = Name,
            is_global = false,
            value = #clock_value{is_abs = true, upper_bound = 0}
        },
        Clocks
    ),
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks1);
to_fsm({delay, _N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % ? skip
    %     ClockName = "c_" ++ Name,
    %     Clocks1 = maps:new(),
    %     maps:foreach(fun(Key,Value) ->
    %         % Value1 = if Value#clock_value.lower_bound==undefined -> #clock_value{is_abs = Value#clock_value.is_abs, lower_bound = 0} of
    %         Value1 = #clock{is_global = false, label = Value#clock.label,
    %           value = if Value#clock.value#clock_value.is_abs==true ->
    %         }
    %         maps:put(Key, #clock, Clocks1)
    %       end, Clocks),

    % (ClockName, #clock{label = Name, is_global = false,
    %                        value = case maps:get(ClockName, Clocks) of
    %                           {badmap, _Map} -> #clock_value{is_abs = true, upper_bound = 0};
    %                           {ClockValue, _Map} -> #clock_value{is_abs = ClockValue#clock_value.is_abs}
    %                           end}),
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({iff, _C, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % TODO add constraint to immediate inner act/branch
    % ? account for someone resetting timer before sending (likely to be another timer, maybe reorder these prior?)
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
to_fsm({iff, _C, P, else, _Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % TODO add constraint to immediate inner act/branch
    % TODO add inverse constraint to immediate inner act in Q (if any)
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
% to_fsm(error, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
%     to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex);

to_fsm({_, _, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks).
