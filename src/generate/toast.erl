-module(toast).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-type payload () :: undefined | any().

-type label () :: atom().

-type msg() :: label() | {label(), payload()}.

-type dir() :: send | recv.

-type clock() :: string().

-type dbc() :: eq | geq | leq | gtr | les.

%% special global clock time, able to be reset
-type deltax() :: {x, dbc(), number()}.

%% simplifying from the beginning, list of conjunctions
-type delta() :: {clock(), dbc(), number()}
              %  | {delta(), conj, delta()}
              %  | {neg, delta()}
               | true | deltax().

-type resets() :: [clock()].

-type interact () :: {dir(), msg(), [delta()], resets(), toast()}.

%% used in choice below
-type region () :: {dir(), {deltax(),delta()}, [ {msg(), resets(), toast() } ]}.

%% interactions in choice already separated into regions according to delta
-type choice () :: [ region() ].

-type toast () :: {choice, choice()} | {action, interact()}
                | {rec, string(), toast()}
                | {rvar, string()}
                | {region, region()} %% used for choice
                | term.


%% @doc to_protocol (adds map for bounds)
to_protocol(TOAST) -> 
  ?assert(validate(TOAST)),
  {Protocol, _, ToBeReset} = to_protocol(TOAST, #{"x"=>0}, 0),

  %% get all to be reset
  Resets = lists:foldl(fun({C,_N,_I}, AccIn) -> AccIn++[C] end, [], ToBeReset),

  {NewProtocol, _} = protocol_timer_resets(Protocol, Resets, ToBeReset),

  NewProtocol.

%% @doc validates TOAST adheres to restrictions
validate(_TOAST) -> true. %% TODO

%% @doc mapp from toast to protocol input.
%% restrictions on toast:
%%  - deltas can not have diagonal constraints (nor negation, but allow >/>=/</<=)
%%  - deltas are in the form of a flattened list with each elem in conjunction
%%  - deltas do not differentiate between (> and >=) and (< and =<)
%%
%%  - for interactions:
%%     - deltas can not have more than one upperbound
%%
%%  - for choices:
%%     - must be given as series of region-grouped choice (non-mixed). 
%%     - two things viable at the same time must use same clock (i.e.: same region)
%%     - the delta of each region follow the same as interaction
%%
-spec to_protocol(toast(), map(), integer()) -> {interleave:protocol(), map(), list()}.

%% @doc region (sub-choice)
to_protocol({region, Region}, Bounds, PrevIndex) ->
  Index = PrevIndex+1,

  {Dir, {DeltaX,Delta}=Deltas, Options} = Region,

  pass;

%% @doc choice
to_protocol({choice, Choice}, Bounds, PrevIndex) ->
  Index = PrevIndex+1,

  pass;

%% @doc interaction
to_protocol({action, Action}, TimerDefs, PrevIndex) ->
  Index = PrevIndex+1,

  {Dir, Msg, Deltas, _Resets, S} = Action,

  Label = get_protocol_label(Dir, Msg),

  %% remove any duplicate resets
  Resets = lists:uniq(_Resets),

  %% wrap next state in necessary resets  
  case length(Resets)==0 of
    true -> %% no differeences to timers
      CurrentTimerDefs = TimerDefs;
    _ -> %% for each clock in resets, start new scope for bounds
      CurrentTimerDefs = lists:foldl(fun(C, AccIn) -> add_reset(C,AccIn)
      end, TimerDefs, Resets)
  end,


  %% get next state
  {NextState, _NextTimerDefs, ToBeReset} = to_protocol(S,CurrentTimerDefs,Index),

  %% add 

  %% for each requested bounded clock, set the ones that appear in resets
  {NewProtocol,StillToBeReset} = protocol_timer_resets(NextState, Resets, ToBeReset),

  %% formulate as protocol
  % _Protocol = {act, Label, NewProtocol},

  %% check if any constraints on current clocks
  Clocks = lists:uniq(get_clocks(Deltas)),

  %% use constraints on actions here to provide bounds for previous clocks
  case length(Clocks)==0 of
    true -> %% no changes on clocks
      NewTimerDefs = TimerDefs,
      NewToBeReset = StillToBeReset,
      Protocol = {act, Label, NewProtocol};
    _ -> %% for each delta, add constraint to NewToBeReset
      NewTimerDefs = TimerDefs,
      {NewToBeReset,ProtocolBack} = lists:foldl(fun(D, {Rs,PBs}=_AccIn) ->
        case D of 
          {C, DBC, N} -> %% add to Rs
            NewRs=Rs++[{C,N,Index}],
            case lists:member(DBC, [eq,geq,gtr]) of
              true -> %% wait for timer to get to correct value before action
                NewPBs=PBs;
              _ -> case lists:member(DBC, [leq,les]) of
                  true -> %% add aft timer 
                    NewPBs=PBs++[get_clock_index(C,Index)];
                  _ -> error(invalid_delta), NewPBs=PBs
                end
            end;
          _ -> %% nothing changes
            NewRs=Rs, NewPBs=PBs
        end,
        {NewRs, NewPBs}
      end, {StillToBeReset, []}, Deltas),
      %% formaulate as protocol
      %% add aft branch if neccessary
      case length(ProtocolBack)==0 of true -> _Protocol = {act, Label, NewProtocol};
        _ -> 
          case length(ProtocolBack)==1 of
            true -> _Protocol = {act, Label, NewProtocol, aft, lists:nth(1,ProtocolBack), error};
            _ -> error(delta_has_more_than_one_upper_bound), _Protocol={act, Label, NewProtocol}
          end
      end,
      _Protocol1 = lists:foldl(fun(D, AccIn) ->
        case D of 
          {C, DBC, _N} -> %% add to Rs
            ClockIndex = get_clock_index(C,Index),
            case lists:member(DBC, [eq,geq,gtr]) of
              true -> %% wait for timer to get to correct value before action
                case AccIn of
                  first_elem -> {delay, ClockIndex, _Protocol};
                  _ -> {delay, ClockIndex, AccIn}
                end;
              _ -> case lists:member(DBC, [leq,les]) of
                  true -> AccIn;
                  _ -> error(invalid_delta)
                end
            end;
          _ -> %% nothing changes
            AccIn
        end
      end, first_elem, Deltas),
      case _Protocol1 of first_elem -> Protocol = _Protocol; _ -> Protocol = _Protocol1 end
  end,


  %% return with updated bounds
  {Protocol, NewTimerDefs, NewToBeReset};

%% @doc recursive definition
to_protocol({rec, Name, TOAST}, Bounds, PrevIndex) -> 
  {Protocol, NextBounds, ToBeReset} = to_protocol(TOAST, Bounds, PrevIndex+1),
  {{rec, Name, Protocol}, NextBounds, ToBeReset};

%% @doc recursive call
to_protocol({rvar, Name}, Bounds, _PrevIndex) -> {{rvar, Name}, Bounds, []};

%% @doc term is end
to_protocol(term, Bounds, _PrevIndex) -> {endP, Bounds, []}.
  



%%%%%%%% helper functions


%%
protocol_timer_resets(Protocol, Resets, ToBeReset) ->
  {P, Rs} = lists:foldl(fun({C,N,I}=_TBR, {_P,_R}=_AccIn) -> 
    case lists:member(C, Resets) of
      true -> %% requested to be reset
        ClockIndex = get_clock_index(C, I),
        case _P of
          first_elem -> P = wrap_timer(ClockIndex, N, Protocol);
          _ -> P = wrap_timer(ClockIndex, N, _P)
        end,
        R = lists:delete(_TBR, _R);
      _ -> %% requested by not to be reset now
        P = _P,
        R = lists:delete(_TBR, _R)
    end,
    {P, R}   
  end, {first_elem, []}, ToBeReset),
  case P of first_elem -> {Protocol, Rs}; _ -> {P, Rs} end.


get_clock_index(Clock, Index) -> list_to_atom(atom_to_list(Clock)++"_"++integer_to_list(Index)).

get_protocol_label(Dir, Msg) ->
  case Dir of
    send -> LabelPrefix = "s_";
    recv -> LabelPrefix = "r_"
  end,
  case Msg of
    {MsgLabel, _Payload} -> Label = atom_to_list(MsgLabel);
    MsgLabel -> Label = atom_to_list(MsgLabel)
  end,
  list_to_atom(LabelPrefix++Label).


get_bound(Clock, Bounds) -> maps:get(Clock,Bounds,unknown_bound).


get_clocks(Delta) -> 
  lists:foldl(fun(D, AccIn) -> 
    case D of
      {C, _DBC, _N} -> AccIn++[C];
      _ -> AccIn
  end end, [], Delta).


get_region_dir({_ID, _DeltaX, Is}=_Region) ->
  IsSend = lists:foldl(fun({Dir, _, _, _, _}=_I, AccIn) -> AccIn and (Dir=:=send) end, true, Is),
  IsRecv = lists:foldl(fun({Dir, _, _, _, _}=_I, AccIn) -> AccIn and (Dir=:=recv) end, true, Is),
  ?assert(IsSend or IsRecv), %% make sure one is true
  ?assert(not(IsSend and IsRecv)), %% make sure one is false
  ?assert(IsSend==(not IsRecv)), %% make sure they are opposite
  case IsSend of true -> send; _ -> recv end.

get_delta_dbc({_Clock, DBC, _Num}=_Delta) -> DBC.

% set_bounds(Clock, Num, Bounds) -> 
%   ?assert(Clock=/=reserved_system_clock),
%   maps:put(Clock, Num, Bounds).
% get_bounds(Clock, Num, Bounds) -> maps:get(Clock, Bounds, maps:get(reserved_system_clock, )).

is_clock_set(Clock, Bounds) -> lists:member(Clock, maps:keys(Bounds)).

% clock_ref(Clock, ResetID, ID) -> atom_to_list(Clock)++"_r"++integer_to_list


%% @doc adds clock num to current reset index
add_clock(Clock, Num, Bounds) -> 
  Def = maps:get(Clock, Bounds, #{r_index=>0}),
  ResetIndex = maps:get(r_index, Def),
  ClockDefs = maps:get(ResetIndex, Def,[]),
  case lists:member(Num, ClockDefs) of
    true -> NewDef = Def; %% do not add if duplicate
    _ -> NewDef = maps:put(ResetIndex,ClockDefs++[Num],Def)
  end,
  maps:put(Clock, NewDef, Bounds).


%% @doc increments reset scope ID for clock
add_reset(Clock, Bounds) -> 
  Def = maps:get(Clock, Bounds, #{r_index=>0}),
  maps:put(Clock, maps:put(r_index, maps:get(r_index, Def)+1, Def)).





%%%%%%% protocol wrappers
wrap_timer(C, N, NextProtocol) -> {timer, C, N, NextProtocol}.




% wrap_choice([], _Bounds) -> {};

% wrap_choice([{_ID,DeltaX,C}=H|T]=_Choice, Bounds) ->
%   % {ID, DeltaX, C} = H,
%   %% get tail
%   {TailProtocol, TailBounds} = wrap_choice(T, Bounds),
%   %% get protocol for head
%   case get_region_dir(H) of
%     send -> Kind = select;%{select, [], aft, -1, Tail};
%     recv -> Kind = branch%{branch, [], aft, -1, Tail}
%   end,

%   %% merge tail bounds with bounds
%   NextBounds = TailBounds, %% TODO

%   %% get protocol for C
%   Options = lists:foldl(fun(O, {CProtocols,CBounds}=AccIn) -> 
%       %% get protocol for current option
%       {_OptionProtocol, OptionBounds} = to_protocol(O, NextBounds),
%       %% remove 'act' from front of protocol tuple for use in branch/select
%       {act, Label, NextProtocol} = _OptionProtocol,
%       OptionProtocol = {Label, NextProtocol},
%       %% add protocol and bounds to result
%       {CProtocols++[OptionProtocol], CBounds++[OptionBounds]}
%   end, {[],[]}, C),
  

%   CurrentBounds = #{},

%   % %% check if delay to be added at front
%   % case DeltaX of
%   %   {x, eq, Num} -> 
%   %     %% check if 
%   %     %% add Num to CurrentBounds
%   %     maps:put(x,Num,CurrentBounds);
%   %   {x, geq, Num} -> 
%   % end,

%   Protocol = {Kind, [], aft, -1, TailProtocol},
  
%   Protocol.
% %%



%%%%%%%% example toast

default_test() -> timed_send_delta_eq.
%% toast:test(default_test())

%% use 
%% toast:test(untimed_send).
%% toast:test(timed_send_delta_eq).

test(Name) ->
  Type = map(Name) ,
  Protocol = to_protocol(Type),
  io:format("\ntype:\t\t~p\n\nprotocol:\t~p.\n",[Type,Protocol]).

map(untimed_send) -> {action, {send, a, [], [], term}};
map(untimed_send_twice) -> {action, {send, b, [], [], map(untimed_send)}};

map(untimed_recv) -> {action, {recv, a, [], [], term}};
map(untimed_recv_twice) -> {action, {recv, b, [], [], map(untimed_recv)}};

map(untimed_send_recv) -> {action, {send, a, [], [], {action, {recv, b, [], [], term}}}};
map(untimed_recv_send) -> {action, {recv, a, [], [], {action, {send, b, [], [], term}}}};

map(untimed_send_loop) -> {rec, "r", {action, {send, a, [], [], {rvar, "r"}}}};
map(untimed_recv_loop) -> {rec, "r", {action, {recv, a, [], [], {rvar, "r"}}}};

map(untimed_send_recv_loop) -> 
  {rec, "r", {action, {send, a, [], [], {action, {recv, b, [], [], {rvar, "r"}}}}}};
map(untimed_recv_send_loop) -> 
  {rec, "r", {action, {recv, a, [], [], {action, {send, b, [], [], {rvar, "r"}}}}}};

map(untimed_nonmixed_choice_send) -> 
  {choice, [map(untimed_send),
            map(untimed_send_twice),
            map(untimed_send_recv)]};

map(untimed_nonmixed_choice_recv) -> 
  {choice, [map(untimed_recv),
            map(untimed_recv_twice),
            map(untimed_recv_send)]};


map(timed_send_delta_eq) -> {action, {send, a, [{x,eq,3}], [], term}};
map(timed_send_delta_geq) -> {action, {send, a, [{x,geq,3}], [], term}};
map(timed_send_delta_gtr) -> {action, {send, a, [{x,gtr,3}], [], term}};
map(timed_send_delta_leq) -> {action, {send, a, [{x,leq,3}], [], term}};
map(timed_send_delta_les) -> {action, {send, a, [{x,les,3}], [], term}};


map(_) -> term.
