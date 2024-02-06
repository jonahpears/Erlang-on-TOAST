%% @doc Process a protocol structure into fsm edges and nodes
-module(build_fsm).

-export([to_fsm/1]).

-include("reng.hrl").

standard_state() -> standard_state.
choice_state() -> choice_state.
mixed_choice_state() -> mixed_choice_state.
end_state() -> end_state.
%% @doc wrapper function that initializes the main to_fsm function
%% takes a protocol and returns a list of transitions/edges and nodes/states
-spec to_fsm(interleave:protocol()) -> {list(), map()}.
to_fsm(P) ->
  Edge = #edge{from = 0, to = 1, edge_data = #edge_data{event = init, event_type = init}},
  {Edges, Nodes, _, _, _, _, _} = to_fsm(P, 
                                      [Edge], 
                                      maps:put(0, init_state, maps:new()), 
                                      maps:new(), 1, 1, -1, 
                                      maps:put(0, #clock{label = "g", is_global = true, value = #clock_value{is_abs = true, upper_bound = 0}}, maps:new())),
  {Edges, Nodes}.

%% @doc processes the actions and labels names and sets the event,
%% variable name and action accordingly. When the name starts with r_ that
%% represents a receive action, with a s_ it represents a send action
-spec args(atom()) -> {atom(), string(), atom()}.
args(Param) ->
  Str = atom_to_list(Param),
  Recv = string:find(Str, "r_"),
  Send = string:find(Str, "s_"),
  if
    Recv =:= Str ->
      Event = cast,
      Var = lists:last(string:split(Str, "r_")),
      Act = list_to_atom("receive_"++ Var);
    Send =:= Str ->
      Event = internal,
      Var = lists:last(string:split(Str, "s_")),
      Act = list_to_atom("send_" ++ Var);
    true ->
      Event = cast,
      Var = Str,
      Act = list_to_atom("act_" ++ Var)
   end,
   {Act, string:titlecase(Var), Event}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom()) -> {atom(), string(), atom()}.
data(Param) ->
  {Act, Var, Event} = args(Param),
  #edge_data{event = {Act, list_to_atom(Var)}, event_type = Event}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom(), interleave:time_constraint()) -> {atom(), string(), atom()}.
data(Param, Constraint) ->
  {Act, Var, Event} = args(Param),
  #edge_data{event = {Act, list_to_atom(Var)}, event_type = Event, guard = Constraint}.

%% @doc transform the protocol to a list of transitions between states and a
%% map of nodes, easier to work with in generate
-spec to_fsm(interleave:protocol(), list(), map(), map(), integer(), integer(),
  integer(), map()) -> {list(), map(), map(), integer(), integer(), integer(), map()}. % ! added clocks
to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Edge = #edge{from = PrevVis, to = Index, edge_data = data(Act)},
    Edges1 = Edges ++ [Edge],
    Nodes1 = maps:put(PrevVis, standard_state(), Nodes),
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Clocks);

to_fsm({act, Act, P, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % Index = PrevIndex + 1,
    % Edge = #edge{from = PrevVis, to = Index, edge_data = data(Act)},
    % Edges1 = Edges ++ [Edge],
    %
    %% check what Q is
    % TODO add Timeout guard to immediate action within Q and inverse to {Act, P}
    Act1 = {Act, P, {leq,Timeout}},
    case Q of
      {act, Act_Q, Next_Q} ->
        Choice = [Act1,{Act_Q, Next_Q, {neg,{leq,Timeout}}}],
        to_fsm({mixed_choice, Choice}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
      
      {act, Act_Q, Next_Q, aft, Inner_Timeout, Inner_Q} ->
        Choice = [Act1,{Act_Q, Next_Q, {neg,{leq,Timeout}}}],
        %% send to {branch} to unfold any timeouts
        to_fsm({branch, Choice, aft, Inner_Timeout, Inner_Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
      
      {branch, Branches_Q} ->
        Branches_Q1 = lists:map(fun(Elem) -> 
          case Elem of
            {Branch_Msg, Branch_P} -> {Branch_Msg, Branch_P, {neg,{leq,Timeout}}};
            _Else -> io:format("\n[unhandled Branches_Q] act aft:\n\tact: ~p,\n\tQ: ~p,\n\tElem: ~p.\n", [Act1,Branches_Q,Elem])
          end end, Branches_Q),
        Choice = [Act1] ++ Branches_Q1,
        to_fsm({mixed_choice, Choice}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

      {branch, Branches_Q, aft, Inner_Timeout, Inner_Q} ->
        Branches_Q1 = lists:map(fun(Elem) -> 
          case Elem of
            {Branch_Msg, Branch_P} -> {Branch_Msg, Branch_P, {neg,{leq,Timeout}}};
            _Else -> io:format("\n[unhandled Branches_Q aft] act aft:\n\tact: ~p,\n\tQ: ~p,\n\tElem: ~p.\n", [Act1,Branches_Q,Elem])
          end end, Branches_Q),
        Choice = [Act1] ++ Branches_Q1,
        %% send to {branch} to unfold any timeouts
        to_fsm({branch, Choice, aft, Inner_Timeout, Inner_Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
    
     _Else ->
        %% skip Q, only handle P
        io:format("\n[unhandled Q] act aft:\n\tact: ~p,\n\tQ: ~p.\n", [{Act, P},Q]),
        to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks)
    end;
    %
    % Choice = [{Act, P}, Q], 
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    % lists:foldl(fun({Label, P1}, {E, N, R, I, _, EI}) ->
    %   I1 = I + 1,
    %   Edge = #edge{from = PrevVis, to = I1, edge_data = data(Label)},
    %   E1 = E ++ [Edge],
    %   to_fsm(P1, E1, N, R, I1, I1, EI) end,
    %   {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex}, Choice);

to_fsm({branch, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    case lists:last(Branches) of
      {_, _} -> 
        lists:foldl(fun({Label, P1}, {E, N, R, I, _, EI, CI}) ->
          I1 = I + 1,
          Edge = #edge{from = PrevVis, to = I1, edge_data = data(Label)},
          E1 = E ++ [Edge],
          to_fsm(P1, E1, N, R, I1, I1, EI, CI) end,
          {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks}, Branches);
      {_, _, _} ->
        lists:foldl(fun({Label, P1, Constraint}, {E, N, R, I, _, EI, CI}) ->
          I1 = I + 1,
          EdgeData = data(Label, Constraint),
          Edge = #edge{from = PrevVis, to = I1, edge_data = EdgeData},
          E1 = E ++ [Edge],
          to_fsm(P1, E1, N, R, I1, I1, EI, CI) end,
          {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks}, Branches)
    end;

to_fsm({branch, Branches, aft, Timeout, Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    % Index = PrevIndex + 1,
    % Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    % Choice = Branches ++ [Q], 
    % io:format("\nbranch, aft: ~p.\n", [Choice]),
    % lists:foldl(fun({Label, P1}, {E, N, R, I, _, EI}) ->
    %   I1 = I + 1,
    %   Edge = #edge{from = PrevVis, to = I1, edge_data = data(Label)},
    %   E1 = E ++ [Edge],
    %   to_fsm(P1, E1, N, R, I1, I1, EI) end,
    %   {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex}, Choice);
    %% check what Q is
    % TODO add Timeout guard to immediate action within Q and inverse to Branches
    %% add timeout constraint to all current branches
    Branches1 = lists:map(fun(Elem) -> 
      case Elem of
        {Branch_Msg, Branch_P} -> {Branch_Msg, Branch_P, {leq,Timeout}};
        % ! TODO unsure on the below case, Constraint?
        {Branch_Msg, Branch_P, Branch_Constraint} -> {Branch_Msg, Branch_P, {leq,Timeout+Branch_Constraint}};
        _Else -> io:format("\n[unhandled Branches] branch aft:\n\tbranches: ~p,\n\tQ: ~p,\n\tElem: ~p.\n", [Branches,Q,Elem])
      end end, Branches),

    case Q of
      {act, Act_Q, Next_Q} ->
        Choice = Branches1 ++ [{Act_Q,Next_Q, {neg,{leq,Timeout}}}],
        to_fsm({mixed_choice, Choice}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

      {act, Act_Q, Next_Q, aft, Inner_Timeout, Inner_Q} ->
        % ! TODO unsure on the below case, Constraint?
        Choice = Branches1 ++ [{Act_Q, Next_Q, {neg,{leq,Timeout}}}],
        %% send to {branch} to unfold any timeouts
        to_fsm({branch, Choice, aft, Inner_Timeout, Inner_Q}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
      
      {branch, Branches_Q} ->
        Branches_Q1 = lists:map(fun(Elem) -> 
          case Elem of
            {Branch_Msg, Branch_P} -> {Branch_Msg, Branch_P, {leq,Timeout}};
            % ! TODO unsure on the below case, Constraint?
            {Branch_Msg, Branch_P, Branch_Constraint} -> {Branch_Msg, Branch_P, {leq,Timeout+Branch_Constraint}};
            _Else -> io:format("\n[unhandled Branches_Q] branch aft:\n\tbranches: ~p,\n\tElem: ~p.\n", [Branches_Q,Elem])
          end end, Branches_Q),
        Choice = Branches1 ++ Branches_Q1,
        to_fsm({mixed_choice, Choice}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);
      _Else ->
        %% skip Q, only handle Branches
        io:format("\n[unhandled Q] branch aft:\n\tbranches: ~p,\n\tQ: ~p.\n", [Branches1,Q]),
        to_fsm(Branches1, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks)
    end;

to_fsm({mixed_choice, Choice}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    io:format("\n[info] mixed-choice: ~p.\n", [Choice]),
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, mixed_choice_state(), Nodes),
    lists:foldl(fun({Label, P1, Constraint}, {E, N, R, I, _, EI, CI}) ->
      io:format("\n[info] mc.foldl: ~p\n\t::~p.\n", [{Label, P1, Constraint},{E, N, R, I, EI, CI}]),
      I1 = I + 1,
      EdgeData = data(Label, Constraint),
      Edge = #edge{from = PrevVis, to = I1, edge_data = EdgeData},
      E1 = E ++ [Edge],
      to_fsm(P1, E1, N, R, I1, I1, EI, CI) end,
      {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Clocks}, Choice);

to_fsm({rec, BoundVar, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    RecMap1 = maps:put(BoundVar, PrevVis, RecMap),
    to_fsm(P, Edges, Nodes, RecMap1, PrevIndex, PrevVis, EndIndex, Clocks);

to_fsm({rvar, Var}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    RecIndex = maps:get(Var, RecMap),
    LastEdge = lists:last(Edges),
    Edge = LastEdge#edge{to=RecIndex},
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
         Nodes1 = maps:put(PrevVis, end_state(), Nodes),
        {Edges, Nodes1, RecMap, PrevIndex, PrevVis, PrevVis, Clocks};
      EndIndex =/= -1 ->
         LastEdge = lists:last(Edges),
         Edge = LastEdge#edge{to = EndIndex},
         Edges1 = lists:droplast(Edges) ++ [Edge],
        {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks}
    end;

to_fsm({assert, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{assert, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

to_fsm({require, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{require, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes,  RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

to_fsm({consume, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{consume, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks);

to_fsm({set, Name, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Clocks) ->
    %% TODO apply reset on
    %% ! (1: last act if above),
    %% ! (2: all incoming actions to state if rec)
    ClockName = "c_" ++ Name,
    Clocks1 = maps:put(ClockName, #clock{label = Name, is_global = false, 
                       value = #clock_value{is_abs = true, upper_bound = 0}}, Clocks),
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
