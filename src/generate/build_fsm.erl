%% @doc Process a protocol structure into fsm edges and nodes
-module(build_fsm).

-export([to_fsm/1]).

-include("reng.hrl").
-include("stub_tools.hrl").
-include("fsm_tools.hrl").
error_state() -> error_state.
timer_start_state() -> timer_start_state.
delay_state() -> delay_state.
fatal_timeout_state() -> fatal_timeout_state.
send_state() -> standard_state.%send_state.
recv_state() -> standard_state.%recv_state.
branch_state() -> branch_state.
select_state() -> select_state.
% standard_state() -> standard_state.
% choice_state() -> choice_state.
% mixed_choice_state() -> mixed_choice_state.
end_state() -> end_state.
custom_end_state() -> custom_end_state.
if_then_else_state() -> if_then_else_state.
if_state() -> if_state.

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
-spec to_fsm(interleave:protocol()) -> {list(), map(), map()}.
to_fsm(P) ->
    Edge = #edge{
        from = 0,
        to = 1,
        edge_data = #edge_data{
            event = init,
            event_type = init
        },
        is_silent = false,
        is_delay = false,
        is_custom_end = false
    },

    {Edges, Nodes, RecMap, _, _, _, _} = to_fsm(
        P,
        [Edge],
        maps:put(0, init_state, maps:new()),
        maps:new(),
        1,
        1,
        -1,
        % maps:put(
            % 0,
            % #clock{
            %     label = "g",
            %     is_global = true,
            %     value = #clock_value{
            %         is_abs = true,
            %         upper_bound = 0
            %     }
            % },
            maps:new()
        % )
    ),

    ?SHOW("fsm, edges:\n\t~p,\n\nfsm, nodes:\n\t~p,\n\nfsm, recmap:\n\t~p.\n",[to_map(Edges),Nodes,RecMap]),

    {Edges, Nodes, RecMap}.

%% @doc processes the actions and labels names and sets the event,
%% variable name and action accordingly. When the name starts with r_ that
%% represents a receive action, with a s_ it represents a send action
-spec args(atom()) -> {atom(), string(), atom()}.
args(Param) ->
    Str = atom_to_list(Param),
    Recv = string:find(Str, "r_"),
    Send = string:find(Str, "s_"),

    ?VSHOW("\n\n\nparam: ~p\nstr: ~p\nrecv: ~p\nsend: ~p\n\n\n", [Param, Str, Recv, Send]),

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
    ?VSHOW("param: ~p.", [Param]),
    {Act, Var, Event, TransType} = args(Param),
    #edge_data{event = {Act, list_to_atom(Var)}, event_type = Event, trans_type = TransType}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom(), interleave:time_constraint()) -> {atom(), string(), atom()}.
data(Param, Constraint) ->
    ?VSHOW("time constraint, param: ~p; constraint: ~p.", [Param, Constraint]),
    {Act, Var, Event, TransType} = args(Param),
    #edge_data{
        event = {Act, list_to_atom(Var)},
        event_type = Event,
        trans_type = TransType,
        timeout = #{ref=>Constraint}
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

%% @doc 
to_fsm(issue_timeout, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  Index = PrevIndex + 1,
  Edge = #edge{
      from = PrevVis,
      to = Index,
      edge_data = #edge_data{trans_type=issue_timeout},
      is_silent = false,
      is_delay = false,
      is_custom_end = false,
      is_internal_timeout_to_supervisor = true
  },
  Edges1 = Edges ++ [Edge],
  Nodes1 = maps:put(PrevVis, fatal_timeout_state(), Nodes),
  to_fsm(endP, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

%% @doc 
to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    EdgeData = data(Act),
    Edge = #edge{
        from = PrevVis,
        to = Index,
        edge_data = EdgeData,
        is_silent = false,
        is_delay = false,
        is_custom_end = false
    },
    Edges1 = Edges ++ [Edge],
    %% determine 
    case EdgeData#edge_data.trans_type of
      recv -> Nodes1 = maps:put(PrevVis, recv_state(), Nodes);
      send -> Nodes1 = maps:put(PrevVis, send_state(), Nodes)
    end,
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

%% @doc generic timeout state (only called after main construct)
to_fsm({aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % +1,
    Index = PrevIndex+1,
    %% check if timeout is '?EQ_LIMIT_MS'
    case Timeout of
      '?EQ_LIMIT_MS' -> Timeout1 = atom_to_list(Timeout);
      _ -> Timeout1 = Timeout
    end,
    %% PrevVis contains index of node to transition from
    Edge = #edge{
        from = PrevVis,
        to = Index,
        edge_data = #edge_data{timeout=#{ref=>Timeout1}},
        is_silent = true,
        is_timeout = true,
        is_delay = false,
        is_custom_end = false
    },
    Edges1 = Edges ++ [Edge],
    %% TODO figure out why we do the below:
    % Nodes1 = maps:put(PrevVis, after_state(), Nodes),
    ?VSHOW(
        "\n[{aft, Timeout, Q}]:...\nEdges:\n\t~p\tNodes:\n\t~p\n\tIndex: ~p\n\tPrevIndex: ~p\n\tPrevVis: ~p\n\tEndIndex: ~p\n\tQ:\n\t~p.\n",
        [Edges, Nodes, Index, PrevIndex, PrevVis, EndIndex,Q]
    ),
    Nodes1 = Nodes,% maps:put(Index, after_state(), Nodes),
    ?VSHOW("\n\n[{aft, Timeout, Q}], Nodes1:\n\t~p.\n", [Nodes1]),
    ?VSHOW("\n\n[{aft, Timeout, Q}], Edges1:\n\t~p.\n", [Edges1]),
    {Edges2, Nodes2, RecMap2, Index2, PrevVis2, EndIndex2, Clocks2} = to_fsm(
        Q, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
    ),
    % ?VSHOW("\n\n[{aft, Timeout, Q}], QResult: (~p)\n\t~p.\n", [size(QResult),QResult]),
    %  = QResult,
    ?VSHOW("\n\n[{aft, Timeout, Q}], Q:\n\t ~p.\n", [Q]),
    ?VSHOW("\n\n[{aft, Timeout, Q}], Nodes2:\n\t~p.\n", [Nodes2]),
    ?VSHOW("\n\n[{aft, Timeout, Q}], Edges2:\n\t~p.\n", [Edges2]),

    % timer:sleep(2000),
    {Edges2, Nodes2, RecMap2, Index2, PrevVis2, EndIndex2, Clocks2};

%% @doc single act with timeout
to_fsm({act, Act, P, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
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
          is_delay = false,
          is_choice = true,
          is_custom_end = false
      },
      Edges1 = Edges ++ [Edge1],
      Nodes1 = maps:put(PrevVis, recv_after_state(), Nodes),
      {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = to_fsm(
          P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
      ),
      ?VSHOW(
          "\n[recv {act, Act, P, aft, Timeout, Q}]:...\n\tEdges2: ~p;\n\tNodes2: ~p;\n\tPrevIndex2: ~p;\n\tPrevVis2: ~p;\n\tEndIndex2: ~p.\n",
          [Edges2, Nodes2, PrevIndex2, _PrevVis2, EndIndex2]
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
      ?VSHOW(
          "\n[recv {act, Act, P, aft, Timeout, Q}]:...\n\tEdges3: ~p;\n\tNodes3: ~p;\n\tPrevIndex3: ~p;\n\tPrevVis3: ~p;\n\tEndIndex3: ~p.\n",
          [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
      ),
      {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};
    send ->
      Edge1 = #edge{
          from = PrevVis,
          to = Index,
          edge_data = EdgeData,
          is_silent = false,
          is_delay = false,
          is_choice = false,
          is_custom_end = false
      },
      % Edge1 = #edge{from = PrevVis, to = Index, edge_data = EdgeData, is_silent = false, is_delay = true},
      Edges1 = Edges ++ [Edge1],
      Nodes1 = maps:put(PrevVis, send_after_state(), Nodes),
      {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = to_fsm(
          P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks
      ),
      % ?VSHOW("\n[2send {act, Act, P, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n", [Edges2, Nodes2, PrevIndex2, PrevVis2, EndIndex2]),
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
      ?VSHOW(
          "\n[send {act, Act, P, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
          [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
      ),
      {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};
    _Else ->
      ?SHOW("\n[unhandled trans_type] act aft: ~p.", [[{act, Act, P}, {aft, Timeout, Q}]]),
      %% TEMP: act as normal
      Edge = #edge{
          from = PrevVis,
          to = Index,
          edge_data = data(Act),
          is_silent = false,
          is_delay = false,
          is_choice = false,
          is_custom_end = false
      },
      Edges1 = Edges ++ [Edge],
      Nodes1 = maps:put(PrevVis, unknown_state(), Nodes),
      to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks)
  end;

%% @doc branching receive
to_fsm({branch, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, branch_state(), Nodes),
    % case lists:last(Branches) of
    %   {_, _} ->
    % FutureEndState = [],
    BranchingFSM=lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}=F) ->
            I1 = I + 1,
            EdgeData = data(Label),
            ?VSHOW("\n\nbranch, (I1=>~p) EI: ~p, BranchFSM:\n\tbefore: ~p.\n",[I1, EI,F]),
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = EdgeData#edge_data{trans_type=recv},
                is_silent = false,
                is_delay = false,
                is_choice = true,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            BranchFSM = to_fsm(P1, E1, N, R, I1, I1, EI, CI),
            % {_, _, _, _, _, _, LaterEndState, _} = BranchFSM,
            % case LatedEndState==-1 of
            %   true -> ok;
            %   _ -> 
            ?VSHOW("branch, (I1=>~p) EI: ~p, BranchFSM:\n\tafter: ~p.\n",[I1, EI,BranchFSM]),
            BranchFSM
        end,
        {Edges, Nodes1, RecMap, PrevIndex, PrevIndex, EndIndex, Clocks},
        Branches
    ),
    ?VSHOW("branching fsm:\n\t~p.\n",[BranchingFSM]),
    BranchingFSM;

%% @doc branching receive with timeout
to_fsm({branch, Branches, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    Nodes1 = maps:put(PrevVis, branch_after_state(), Nodes),
    %% fully explore the other branches first
    {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            EdgeData = data(Label),
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = EdgeData#edge_data{trans_type=recv},
                is_silent = false,
                is_delay = false,
                is_choice = true,
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
        {aft, Timeout, Q}, Edges2, Nodes2, RecMap2, PrevIndex2, PrevVis, EndIndex2, Clocks2
    ),
    ?VSHOW(
        "\n[{branch, Branches, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
        [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
    ),
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};

%% @doc output selection
to_fsm({select, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, select_state(), Nodes),

    lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            EdgeData = data(Label),
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = EdgeData#edge_data{trans_type=send},
                is_silent = false,
                is_delay = false,
                is_choice = true,
                is_custom_end = false
            },
            E1 = E ++ [Edge],
            to_fsm(P1, E1, N, R, I1, I1, EI, CI)
        end,
        {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks},
        Branches
    );

%% @doc output selection with timeout
to_fsm({select, Branches, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    Nodes1 = maps:put(PrevVis, select_after_state(), Nodes),
    %% fully explore the other branches first
    {Edges2, Nodes2, RecMap2, PrevIndex2, _PrevVis2, EndIndex2, Clocks2} = lists:foldl(
        fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
            I1 = I + 1,
            EdgeData = data(Label),
            Edge = #edge{
                from = PrevVis,
                to = I1,
                edge_data = EdgeData#edge_data{trans_type=send},
                is_silent = false,
                is_delay = false,
                is_choice = true,
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
        {aft, Timeout, Q}, Edges2, Nodes2, RecMap2, PrevIndex2, PrevVis, EndIndex2, Clocks2
    ),
    ?VSHOW(
        "\n[{select, Branches, aft, Timeout, Q}]:...\n\tEdges: ~p;\n\tNodes: ~p;\n\tPrevIndex: ~p;\n\tPrevVis: ~p;\n\tEndIndex: ~p.\n",
        [Edges3, Nodes3, PrevIndex3, PrevVis3, EndIndex3]
    ),
    {Edges3, Nodes3, RecMap3, PrevIndex3, PrevVis3, EndIndex3, Clocks3};

%% @doc 
to_fsm({rec, BoundVar, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    RecMap1 = maps:put(BoundVar, PrevVis, RecMap),
    %% ! swapping around for convenience
    % RecMap1 = maps:put(PrevVis, BoundVar, RecMap),
    to_fsm(P, Edges, Nodes, RecMap1, PrevIndex, PrevVis, EndIndex, Clocks);

%% @doc 
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
  case (EndIndex =:= -1) of
    true -> %% end state has not been found yet! this is the end state
      Nodes1 = maps:put(PrevVis, custom_end_state(), Nodes),
      Index = PrevIndex + 1,
      EndIndex1 = PrevVis,
      Nodes2 = maps:put(Index, end_state(), Nodes1),
      Edges1 = Edges ++ [#edge{ from = PrevVis,
                                to = Index,
                                edge_data = #edge_data{timeout = 0, comments = ["% is end"]},
                                is_silent = true,
                                is_delay = false,
                                is_custom_end = true }];

    false -> %% the end state has already been found, see EndIndex
      Index = PrevIndex,
      EndIndex1 = EndIndex,
      LastEdge = lists:last(Edges),
      Edge = LastEdge#edge{to = EndIndex},
      Edges1 = lists:droplast(Edges) ++ [Edge],
      Nodes2 = Nodes
  end,
  %% reached termination
  {Edges1, Nodes2, RecMap, Index, PrevVis, EndIndex1, Clocks};

%% @doc error state
to_fsm(error, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) -> to_fsm({error, unspecified_error}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

%% @doc error state
to_fsm({error, Reason}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  Index = PrevIndex+1,
  % Nodes1 = maps:put(Index, error_state(), Nodes),
  %% force go to end in order to add to edge the error reason
  % case (EndIndex =:= -1) of
  %   true -> %% end state has not been found yet! this is the end state
      Nodes1 = maps:put(PrevVis, error_state(), Nodes),
      % Nodes2 = maps:put(Index, end_state(), Nodes1),
      Edges1 = Edges ++ [#edge{ from = PrevVis,
                                to = Index,
                                edge_data = #edge_data{error_reason=Reason},
                                is_silent = true,
                                is_delay = false,
                                is_error = true,
                                is_custom_end = false }],

  %   false -> %% the end state has already been found, see EndIndex
  %   Nodes2 = maps:put(PrevVis, error_state(), Nodes),
  %   Edges1 = Edges ++ [#edge{ from = PrevVis,
  %                             to = EndIndex,
  %                             edge_data = #edge_data{error_reason=Reason},
  %                             is_silent = true,
  %                             is_delay = false,
  %                             is_error = true,
  %                             is_custom_end = false }]
  %     % LastEdge = lists:last(Edges),
  %     % Edge = LastEdge#edge{to = EndIndex},
  %     % Edges1 = lists:droplast(Edges) ++ [Edge],
  %     % Nodes2 = Nodes
  % end,
  %% reached termination
  % {Edges1, Nodes2, RecMap, PrevIndex, PrevVis, EndIndex, Clocks};
  to_fsm(endP, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

%% @doc 
to_fsm({timer, Name, Duration, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % add new state that is a timer definition, with silent transition to next state
    Nodes1 = maps:put(PrevVis, timer_start_state(), Nodes),
    %% get index of P
    Index = PrevIndex + 1,
    %% create edge from current timer_state_state to P
    Edge = #edge{ from = PrevVis,
                  to = Index,
                  edge_data = #edge_data{timer=#{duration=>Duration,name=>Name}},
                  is_silent = true,
                  is_delay = false,
                  is_timer = true,
                  is_custom_end = false },
    %% add edge to edges
    Edges1 = Edges ++ [Edge],
    %% move to P
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

%% @doc 
to_fsm({delay, Duration, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  % add time-consuming silent transition to next state
  Nodes1 = maps:put(PrevVis, delay_state(), Nodes),
  %% get index of P
  Index = PrevIndex + 1,
  %% create edge from current timer_state_state to P
  Edge = #edge{ from = PrevVis,
                to = Index,
                edge_data = #edge_data{delay=#{ref=>Duration}}, %% since delays can use timers too
                is_silent = true,
                is_delay = true,
                  is_timer = false,
                  is_custom_end = false },
  %% add edge to edges
  Edges1 = Edges ++ [Edge],
  %% move to P
  to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

%% @doc
%% TODO add both kinds of iff protocols here

%% @doc 
to_fsm({if_timer, Name, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % add new state that is a if_state
    Nodes1 = maps:put(PrevVis, if_then_else_state(), Nodes),
    %% get index of P
    Index = PrevIndex + 1,
    %% create edge from current if_timer to P
    Edge = #edge{ from = PrevVis,
                  to = Index,
                  edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>false}},
                  is_silent = true,
                  is_if = true,
                  is_else = false,
                  is_delay = false,
                  is_timer = false,
                  is_custom_end = false },
    %% add edge to edges
    Edges1 = Edges ++ [Edge],
    %% move to P
    {QEdges, QNodes, QRecMap, QPrevIndex, _QPrevVis, QEndIndex, QClocks} = to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks),
    %% get index of Q
    QIndex = QPrevIndex + 1,
    %% create edge from current timer_state_state to Q
    QEdge = #edge{ from = PrevVis,
                  to = QIndex,
                  edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>false}},
                  is_silent = true,
                  is_if = false,
                  is_else = true,
                  is_delay = false,
                  is_timer = false,
                  is_custom_end = false },
    %% add edge to edges
    QEdges1 = QEdges ++ [QEdge],
    %% move to Q
    to_fsm(error, QEdges1, QNodes, QRecMap, QIndex, QIndex, QEndIndex, QClocks);

%% @doc 
to_fsm({if_not_timer, Name, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % add new state that is a if_state
    Nodes1 = maps:put(PrevVis, if_then_else_state(), Nodes),
    %% get index of P
    Index = PrevIndex + 1,
    %% create edge from current if_timer to P
    Edge = #edge{ from = PrevVis,
                  to = Index,
                  edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>true}},
                  is_silent = true,
                  is_if = true,
                  is_else = false,
                  is_delay = false,
                  is_timer = false,
                  is_custom_end = false },
    %% add edge to edges
    Edges1 = Edges ++ [Edge],
    %% move to P
    {QEdges, QNodes, QRecMap, QPrevIndex, _QPrevVis, QEndIndex, QClocks} = to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks),
    %% get index of Q
    QIndex = QPrevIndex + 1,
    %% create edge from current timer_state_state to Q
    QEdge = #edge{ from = PrevVis,
                  to = QIndex,
                  edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>true}},
                  is_silent = true,
                  is_if = false,
                  is_else = true,
                  is_delay = false,
                  is_timer = false,
                  is_custom_end = false },
    %% add edge to edges
    QEdges1 = QEdges ++ [QEdge],
    %% move to Q
    to_fsm(error, QEdges1, QNodes, QRecMap, QIndex, QIndex, QEndIndex, QClocks);

%% @doc 
to_fsm({if_timer, Name, P, 'else', Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  % add new state that is a if_then_else_state
  Nodes1 = maps:put(PrevVis, if_then_else_state(), Nodes),
  %% get index of P
  Index = PrevIndex + 1,
  %% create edge from current if_timer to P
  Edge = #edge{ from = PrevVis,
                to = Index,
                edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>false}},
                is_silent = true,
                is_if = true,
                is_else = false,
                is_delay = false,
                is_timer = false,
                is_custom_end = false },
  %% add edge to edges
  Edges1 = Edges ++ [Edge],
  %% move to P
  {QEdges, QNodes, QRecMap, QPrevIndex, _QPrevVis, QEndIndex, QClocks} = to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks),
  %% get index of Q
  QIndex = QPrevIndex + 1,
  %% create edge from current timer_state_state to Q
  QEdge = #edge{ from = PrevVis,
                to = QIndex,
                edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>false}},
                is_silent = true,
                is_if = false,
                is_else = true,
                is_delay = false,
                is_timer = false,
                is_custom_end = false },
  %% add edge to edges
  QEdges1 = QEdges ++ [QEdge],
  %% move to Q
  to_fsm(Q, QEdges1, QNodes, QRecMap, QIndex, QIndex, QEndIndex, QClocks);

%% @doc 
to_fsm({if_not_timer, Name, P, 'else', Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  % add new state that is a if_then_else_state
  Nodes1 = maps:put(PrevVis, if_then_else_state(), Nodes),
  %% get index of P
  Index = PrevIndex + 1,
  %% create edge from current if_timer to P
  Edge = #edge{ from = PrevVis,
                to = Index,
                edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>true}},
                is_silent = true,
                is_if = true,
                is_else = false,
                is_delay = false,
                is_timer = false,
                is_custom_end = false },
  %% add edge to edges
  Edges1 = Edges ++ [Edge],
  %% move to P
  {QEdges, QNodes, QRecMap, QPrevIndex, _QPrevVis, QEndIndex, QClocks} = to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks),
  %% get index of Q
  QIndex = QPrevIndex + 1,
  %% create edge from current timer_state_state to Q
  QEdge = #edge{ from = PrevVis,
                to = QIndex,
                edge_data = #edge_data{if_stmt=#{is_timer=>true,ref=>Name,is_not=>true}},
                is_silent = true,
                is_if = false,
                is_else = true,
                is_delay = false,
                is_timer = false,
                is_custom_end = false },
  %% add edge to edges
  QEdges1 = QEdges ++ [QEdge],
  %% move to Q
  to_fsm(Q, QEdges1, QNodes, QRecMap, QIndex, QIndex, QEndIndex, QClocks);

%% @doc unhandled protocol
to_fsm(S, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
  ?SHOW("~p, unhandled protocol: ~p.", [?FUNCTION_NAME, S]),
  case S of 
    {_, _, P} -> to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
    {_, _, _, _, _, P} -> to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks)
  end.
