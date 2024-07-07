-module(sample).
-compile(export_all).
-compile(nowarn_export_all).
-compile(nowarn_unused_type).

%% used when generating constants, constraints and time delays
-define(CONSTANT_UPPER_BOUND,9).
-define(CONSTANT_LOWER_BOUND,9).
-define(CONSTRAINT_UPPER_BOUND, 5).
-define(CONSTRAINT_LOWER_BOUND, 2).
-define(DELAY_UPPER_BOUND,5.0).

%% @doc helper functions to provide sample labels, clocks, dbc, types
-spec sample(atom()|{atom(),list()}|{atom(),{list(),any()}}) -> list()|map().

sample(labels) -> [a,b,c,d,e,f,g];
sample(clocks) -> [u,v,w,x,y,z];
sample(types) -> [interact,choice,def];
sample(dbc) -> ['gtr','eq'];
sample(dbc_extended) -> sample(dbc)++['geq','les','leq','neq'];

%% @doc sample list of clock valuations
sample({clocks_valuations,{Clocks,Bounds}})
when is_map(Bounds) -> 
  %% unpack bounds
  Lower = maps:get(lower,Bounds,?CONSTRAINT_LOWER_BOUND),
  Upper = maps:get(upper,Bounds,lists:max([Lower+1,?CONSTRAINT_UPPER_BOUND])),
  Global = maps:get(global,Bounds,Upper),
  %% for each clock, set values
  Result = lists:foldl(fun(Clock, In) ->
    %% check if any specific bounds specified
    Value = case maps:get(Clock,Bounds,undefined) of 

      %% no specific bounds given, use Lower and Upper
      undefined -> rand_float_from_range(Lower,Upper);

      %% specific value requested
      #{value:=Val} -> Val;

      %% both bounds
      #{lower:=LowerVal,upper:=UpperVal} -> rand_float_from_range(LowerVal,UpperVal);

      %% upper bounds
      #{upper:=UpperVal} -> rand_float_from_range(Lower,UpperVal);

      %% lower bounds
      #{lower:=LowerVal} -> rand_float_from_range(LowerVal,Upper);

      %% unknown
      _ -> rand_float_from_range(Lower,Upper)

    end,
    %% add to list
    In++[{Clock,Value}]
  end, [], Clocks) ++ [{global,Global}],
  %% return as map
  maps:from_list(Result);
%%

%% @doc sample select random from list
sample({rand_from_list,List}) -> lists:nth(rand:uniform(length(List)),List);

%% @doc sample make list of random things
%% Num is the desired size of the returned list
sample({list_of_rand,{List,Num,IsUnique}}) 
when is_boolean(IsUnique) -> 
  {Result,_Input} = lists:foldl(fun(_, {In,_L}) ->
    %% select random from List (via _L)
    Selected = lists:nth(rand:uniform(length(_L)),_L),
    %% if list needs to be unique, remove option from List
    case IsUnique of 
      true -> NewList = lists:delete(Selected,_L);
      _ -> NewList = _L
    end,
    %% return
    {In++[Selected],NewList}
  end, {[],List}, lists:duplicate(Num,undefined)),
  %% return
  Result;
%%

sample(_) -> undefined.


%% @doc helper functions to provide sample types or constraints
-spec sample(atom(),map()) -> toast:type()|toast:delta().

%% @doc sample continuation (as defined in options)
sample(continuation,Options) ->
  %% check if continuation is specified, else use end
  case maps:get(has_continuation,Options,no_continuation) of 

    %% no continuation, is end
    no_continuation -> maps:get(continuation,Options,'end');

    [Next|T] -> 
      %% has continuation, check if first is defined
      Continuation = case Next of 

        %% undefined continuation, select from random valid ones
        undefined -> sample({rand_from_list,sample(type)});

        %% specific continuation specified
        _ -> Next
          
      end,
      %% check if this is the last continuation
      case length(T)>0 of 

        %% this is not the last 
        true -> sample(Continuation,maps:put(has_continuation,T,Options));

        %% get continuatino, but no more after
        false -> sample(Continuation,maps:put(has_continuation,false,Options))

      end;

      %% unrecognised continuation, use end but warn
      Err -> 
        io:format("\n\nWarning, ~p, unrecognised continuation.\n\tUsing 'end' instead.\n(Options:\t~p.)\n",[Err,Options]),
        'end'
  end;
%%

%% @doc sample constraint
sample(constraints,Options) ->
  %% unpack constraint options
  Spec = maps:get(constraint_spec,Options,#{}),
  Lower = maps:get(lower,Spec,?CONSTANT_LOWER_BOUND),
  Upper = maps:get(upper,Spec,?CONSTANT_UPPER_BOUND),
  %% check if has constraints
  Constraints = case maps:get(has_constraints,Options,no_constraints) of 

    %% no constraints, use true
    no_constraints -> 'true';

    %% make half-random constraints
    {mode,Mode} -> 
      %% extract from args
      Kind = maps:get(kind,Spec,simple),
      Num = maps:get(num,Spec,1),
      Constants = maps:get(constants,Spec,#{}),

      %% depending on kind of sample requested (diagonal or simple)
      case Kind of
        
        %% diagonal constraints
        diagonal -> 

          %% TODO :: expand on this

          io:format("\n\nWarning, selected 'diagonal' constraints, but this is not currently implemented.\nSetting constraint to 'true' instead."),
          'true';

        %% simple 
        _ -> 
          %% get list of unique clocks 
          Clocks = maps:get(clocks,Spec,sample({list_of_rand,{sample(clocks),Num,true}})),
          %% get list of non-unique DBC operators
          DBCs = maps:get(dbcs,Spec,sample({list_of_rand,{sample(dbc),Num,false}})),
    
          %% depending on mode (random or half-random)
          case Mode of 

            %% make random where defaults are not provided
            half_random -> 
              %% for each clock, construct constraint
              {_, _, _Constraints} = lists:foldl(fun(Clock, {Conj, [DBC|T], In}) ->
                %% get clock spec (if any)
                ClockSpec = maps:get(Clock,Constants,undefined),
                % io:format("\nClockSpec (~p): ~p.",[Clock,ClockSpec]),
                %% get value of constant (either rand from default range, or specific to clock)
                {Constant, Negated} = case ClockSpec of 
                  
                  %% use random from range of defaults
                  undefined -> {rand_int_from_range(Lower,Upper), false};

                  %% value for constant provided
                  #{constant:=Val} -> {Val, maps:get(negated,ClockSpec,false)};
                
                  %% lower bound provided 
                  #{lower:=LowerVal} -> {rand_int_from_range(LowerVal,Upper), maps:get(negated,ClockSpec,false)};
                
                  %% lower bound provided 
                  #{upper:=UpperVal} -> {rand_int_from_range(Lower,UpperVal), maps:get(negated,ClockSpec,false)};
                
                  %% unknown
                  _ -> {rand_int_from_range(Lower,Upper), maps:get(negated,ClockSpec,false)}

                end,
                %% create new constraint
                _New = {Clock, DBC, Constant},
                New = case Negated of 
                  true -> {'not', _New};
                  _ -> _New
                end,
                %% if conj with existing (building tree)
                NextIn = case Conj of 
                  true -> {In, 'and', New};
                  _ -> New
                end,
                %% return
                {true, T, NextIn}
              end, {false, DBCs,'true'}, Clocks),
              %% return
              _Constraints;

            %% make random constraint with random clock, DBC and constant
            random ->
              %% get non-unique list of DBC operators
              DBCs = sample({list_of_rand,{sample(dbc),Num,false}}),
              %% for each DBC, construct random constraint
              lists:foldl(fun(DBC, In) ->
                %% create new constraint
                New = {sample({rand_from_list,sample(clocks)}), DBC, rand:uniform(?CONSTRAINT_UPPER_BOUND)},
                %% conj with existing (building tree)
                {In, 'and', New}
              end, 'true', DBCs);

            %% unexpected
            _Err -> 
              io:format("\n\nWarning, ~p, unexpected Mode: '~p', returning 'true'.\n\tOptions: ~p.\n",[?FUNCTION_NAME,Mode,Options]),
              'true'

          end

      end;

    %% has constraints, return them
    _Constraints -> _Constraints
      
  end,
  %% return constraints
  Constraints;
%%

%% @doc sample choice type
sample(choice,Options) ->
  %% unpack num of interactions
  Num = maps:get(choice_size,Options,1),
  %% get choice spec, just assign this option if none is provided
  ChoiceSpec = maps:get(choice_spec,Options,list:duplicate(Num,Options)),
  %% create list of size Num, containing fresh 
  Result = lists:foldl(fun(Spec, In) ->
    In++[sample(interact,Spec)]
  end, [], ChoiceSpec),
  %% return
  Result;
%%

%% @doc random sample interact type
sample(interact,#{mode:=random}=Options) ->

  %% randomise direction (50/50 send/recv)
  Direction = case rand:uniform()<0.5 of true -> send; _ -> recv end,

  %% set random message label
  Labels = sample(labels),
  Msg = {lists:nth(rand:uniform(length(Labels)), Labels), none},

  %% constraints
  Constraints = sample(constraints,Options),

  %% resets
  Clocks = sample(clocks),
  Resets = sample({list_of_rand,{Clocks,rand:uniform(length(Clocks)-1),true}}),
  
  %% check continuation
  S = sample(continuation,Options),

  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc half-random sample interact type
sample(interact,#{mode:=half_random}=Options) ->

  %% if not defined, randomise direction (50/50 send/recv)
  Direction = maps:get(direction,Options,case rand:uniform()<0.5 of true -> send; _ -> recv end),

  %% if not defined, set random message label
  Labels = sample(labels),
  Msg = maps:get(msg,Options,{lists:nth(rand:uniform(length(Labels)), Labels), none}),

  %% constraints
  Constraints = maps:get(constraints,Options,sample(constraints,Options)),

  %% resets
  Clocks = sample(clocks),
  Resets = maps:get(resets,Options,sample({list_of_rand,{Clocks,rand:uniform(length(Clocks)-1),true}})),

  %% check continuation
  S = sample(continuation,Options),

  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc non-random sample interact type
sample(interact,Options) ->
  %% unpack
  Direction = maps:get(direction,Options,send),
  Msg = maps:get(msg,Options,{m,none}),
  Constraints = maps:get(constraints,Options,true),
  Resets = maps:get(resets,Options,[]),

  %% check continuation
  S = sample(continuation,Options),
  
  %% return
  {Direction, Msg, Constraints, Resets, S};
%%

%% @doc unsupported type
sample(Unsupported,_Map) -> 
  io:format("\n\nError, unsupported type: ~p.",[Unsupported]),
  %% return end type
  'end'.
%%


%% @doc returns random int in range Lower <-> Upper
%% @see rand_float_from_range/2
rand_int_from_range(Lower,Upper) -> 
  %% call for float, then floor
  Result = rand_float_from_range(Lower,Upper),
  %% return
  floor(Result).
%%

%% @doc returns random float in range Lower <-> Upper
rand_float_from_range(Lower,Upper) -> 
  %% if same, return
  Result = case (Lower=:=Upper) of 
    true -> Lower;
    
    %% pick random from range
    _ -> lists:min([Lower+rand:uniform(Upper+1)-1,Upper])

  end,
  %% return
  Result.
%%

%% @doc returns true if the constraint features the given Clock
is_clock_constrained(Clock,{'not',Constraints}) -> is_clock_constrained(Clock,Constraints);
is_clock_constrained(Clock,{Constraints1,'and',Constraints2}) -> is_clock_constrained(Clock,Constraints1) or is_clock_constrained(Clock,Constraints2);
is_clock_constrained(Clock1,{Clock2,_DBC,_Constant}) when Clock1=:=Clock2 -> true;
is_clock_constrained(Clock1,{Clock2,'-',Clock3,_DBC,_Constant}) when ((Clock1=:=Clock2) or (Clock1=:=Clock3)) -> true;
is_clock_constrained(_Clock,_Constraints) -> 
  % io:format("\n\nWarning, ~p, unexpected Constraint: ~p. (with clock: ~p)",[?FUNCTION_NAME,_Constraints,_Clock]),
  false.
