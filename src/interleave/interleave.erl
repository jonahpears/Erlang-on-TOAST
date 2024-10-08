%% @doc Interleaving composition and decomposition of protocols
-module(interleave).
-compile(export_all).
-compile(nowarn_export_all).

-type time () :: timeout() | string().
-type upper_bound () :: time() | '?EQ_LIMIT_MS'.

%% @doc Protocol format
%% Notes:
%%   - Recursion variables are represented as string()
%%   - Actions, labels, and assertion names are represented as atom()
%%   - Control branches are represented as a list of atom/protocol pairs
%%   - The atom endP is used to end a protocol because "end" is a reserved keyword.
-type protocol () :: {'act', atom(), protocol()}
                   | {'act', atom(), protocol(), 'aft', upper_bound(), protocol()}
                   | {'branch', [ {atom(), protocol()} ]}
                   | {'branch', [ {atom(), protocol()} ], 'aft', upper_bound(), protocol()}
                   | {'select', [ {atom(), protocol()} ]}
                   | {'select', [ {atom(), protocol()} ], 'aft', upper_bound(), protocol()}
                   | {'aft', upper_bound(), protocol()}
                   | {'timer', string(), number(), protocol()}
                   | {'delay', time(), protocol()}
                  %  | {'roles', [ atom() ], protocol()}
                  %  | {'error', atom()}
                  %  | {'from', atom(), 'to', atom()}
                  %  | {'set', atom(), protocol()}
                  %  | {'delay', time_region(), protocol()}
                   | {'if_timer', string(), protocol()}
                   | {'if_timer', string(), protocol(), 'else', protocol()}
                   | {'if_not_timer', string(), protocol()}
                   | {'if_not_timer', string(), protocol(), 'else', protocol()}
                  %  | {'iff', boolean(), protocol()}
                  %  | {'iff', boolean(), protocol(), 'else', protocol()}
                  %  | {'assert', atom(), protocol()}
                  %  | {'require', atom(), protocol()}
                  %  | {'consume', atom(), protocol()}
                   | {'rec', string(), protocol()}
                   | {'rvar', string()}
                   | 'issue_timeout'
                   | 'error'
                   | {'error', atom()}
                   | 'endP'.
                  %  | 'error'.

% -type time_constraint () :: {number()}
%                           | {'neg', time_constraint()}
%                           | {time_constraint(), 'land', time_constraint()}
%                           % | {'eq', number()}
%                           % | {number(), 'leq', number()}
%                           | {atom(), 'in', time_region()}
%                           | {'tt'}.

% -type time_region () :: {'neg', time_region()}
%                       | {time_region(), 'cup', time_region()}
%                       | {'eq',number()}
%                       | {'leq',number()}
%                       | {'tt'}.

% # Examples
e1() ->
  {act, n, endP}.

e2() ->
  {require, n, {act, x, endP}}.

e3() ->
  {assert, n, {act, y, endP}}.

e4() ->
  {branch, [{l, {act, b, {assert, n, endP}}} ,{r, {act, c, {assert, n, endP}}}]}.

e5() ->
  {branch, [{l, {assert, n, endP}} ,{r, {assert, n, endP}}, {m, {assert, n, endP}}]}.

e6() ->
  {branch, [{l, {require, n, endP}} ,{r, {act, c, endP}}, {m, {assert, n, endP}}]}.

e7() ->
  {act, r_pwd, {branch, [{ok, {assert, n, endP}},{fail, endP}]}}.

bank() ->
  {require, pin, {rec, "t", {branch, [{statement, {act, s_statement, {rvar, "t"}}},
                                    {payment, {assert, pay,{consume, tan,{act, r_details,  {rvar, "t"}}}}},
                                    {logout, {consume, pin, endP}}]
                          }
                  }
  }.

pintan() ->
  {act, r_pin, {branch, [
                          {ok, {assert, pin, {rec, "r", {consume, pay, ctan()}}}},
                          {fail, endP}]
                }
  }.

ctan() ->
   {act, s_id, {act, r_tan, {branch, [{tok, {assert, tan, {rvar, "r"}}},
                                            {tfail, {rvar, "r"}}]
                            }
              }
  }.

bankauth() ->
{act,r_pin,
 {branch,
  [{ok,
    {assert,pin,
     {require,pin,
      {rec,"t",
       {require, keyp,
       {branch,
        [{payment,
          {assert,pay,
           {consume,pay,
            {act,s_id,
             {act,r_tan,
              {branch,
               [{tok,{assert,tan,{consume,tan,{act,r_details,{rvar,"t"}}}}},
                {tfail,{rvar,"t"}}]}}}}}},
         {statement,{act,s_statement,{rvar,"t"}}},
         {logout,{consume,pin,endP}}]}}}}}},
   {fail,endP}]}
   }.

bankauthsimple() ->
{act,r_pin,
 {branch,
  [{ok,
      {rec,"t",
          {branch,
              [ {payment, {assert, keyp,  {require, tb, {act,s_id, {act,r_tan, {branch,
                                                    [{tok,{assert,tan,{consume,tan,{act,r_details,{rvar,"t"}}}}},
                                                    {tfail,{rvar,"t"}}]
                                                                                }
                          }
                          }}
                }},
                {statement,{act,s_statement,{rvar,"t"}}},
                {logout,{act,s_statement,{rvar,"t"}}}
               ]
          }
}},
 {fail,endP}]}
   }.



keycard() -> {rec, "y", {require, keyp, {branch, [{tan, {assert, tb, {rvar, "y"}}},
                         {keycard, {rvar, "y"}}
                                  ]
                        }}
          }.


pin() ->
  {act, r_pin, {branch, [{ok, {assert, pin, endP}},
                              {fail, endP}]
                }
  }.

tan() ->
  {require, pin, {rec, "r", {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, "r"}}},
                                                              {fail, {rvar, "r"}}]
                                                    }
                                      }
                          }
                  }
  }.

agent1() -> {branch, [{r_ua_set_ua_set, {assert, n, {assert, set, {act, r_ua_coord, {assert, coord, {act, s_au_state, endP}}}}}},
                               {r_ua_get, {assert, n, {assert, get,{act, s_au_snap, {assert, snap, endP}}}}},
                               {r_ua_close,{assert, n, {assert, close, endP}}}]
            }.

agent2() -> {consume, n, {branch, [{s_ai_set, {consume, set, {act, s_ai_coord, {consume, coord, {act, r_ia_state, endP}}}}},
                               {s_ai_get, {consume, get, {act, r_ia_snap, {consume, snap, endP}}}},
                               {s_ai_close, {consume, close, endP}}]
            }}.



userAgent() -> {rec, "r", {branch, [{ua_r_set, {act, ua_r_coord, {assert, set, {rvar, "r"}}}},
                                    {ua_r_get, {assert, get, {consume, snap, {act, au_s_snap, {rvar, "r"}}}}},
                                    {ua_r_close, {assert, close, endP}}
]}}.

agentInstrument() -> {rec, "t", {branch, [{ai_s_set, {consume, set, {act, ai_s_coord, {rvar, "t"}}}},
                                          {ai_s_get, {consume, get, {act, ai_r_snap, {assert, snap, {rvar, "t"}}}}},
                                          {ui_s_close, {consume, close, endP}}
]}}.


r1() -> {rec, "t1", {act, f, {rvar, "t1"}}}.

r2() -> {rec, "t2", {act, g, {rvar, "t2"}}}.

nr1() -> {rec, "t1", {act, a, {rec, "t2", {branch, [
                                             {b1, {rvar, "t1"}},
                                             {b2, {rvar, "t2"}}
                                             ]}}}}.

nr2() -> {rec, "t3", {act, c, {rec, "t4", {branch, [
                                             {d1, {rvar, "t3"}},
                                             {d2, {rvar, "t4"}}
                                             ]}}}}.

nrend() -> {rec, "t3", {act, c, {rec, "t4", {branch, [
                                             {d1, {rvar, "t3"}},
                                             {d2, {rvar, "t4"}},
                                             {d3, endP}
                                             ]}}}}.
                                             
r3_1() ->  {act, r_pwd, {assert, login, {rec, "t1", {branch, [
                                                               {void, {rvar, "t1"}},
                                                               {quit, {act, quit, {assert, n, {consume, login, endP}}}}
                                                               ]
    }}}}.
    
r3_2() ->  {rec, "t", {branch, [
                               {balance, {require, login, {act, bal, {rvar, "t"}}}},
                               {logout, {consume, n, endP}}
                               ]
           }}.





                              

%% @doc Pretty print protocols
-spec pprint(protocol()) -> string().
pprint({act, Act, P}) ->
  atom_to_list(Act) ++ "." ++ pprint(P);
pprint({branch, Branches}) ->
  "{" ++ pprintBranches(Branches) ++ "}";
pprint({assert, N, P}) ->
  "assert(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({require, N, P}) ->
  "require(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({consume, N, P}) ->
  "consume(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({rec, BoundVar, P}) ->
  "nu " ++ BoundVar ++ " . (" ++ pprint(P) ++ ")";
pprint({rvar, Var}) ->
  Var;
pprint(endP) ->
  "end".

%% @doc Auxiliary printers
%% Prints a branch
pprintBranch({Label, P}) -> atom_to_list(Label) ++ " : " ++ pprint(P).
% Prints a list of branches
pprintBranches([])     -> "";
pprintBranches([B])    -> pprintBranch(B);
pprintBranches([B|BS]) -> pprintBranch(B) ++ "; " ++ pprintBranches(BS).

% power set
power([]) -> [[]];
power([H|T]) -> PT = power(T),
power(H, PT, PT).

power(_, [], Acc) -> Acc;
power(X, [H|T], Acc) -> power(X, T, [[X|H]|Acc]).


filterSet(Data) when is_list(Data) ->
    Pred = fun(Element) -> Element /= [] end,
    lists:filter(Pred, Data).




% Finds the subset of J without empty set
jBranch(J) ->
  filterSet(power(J)).

% Returns true if a bad combo i.e., it has at least an empty branch
% badJCombo1(A) ->
%   Results = for(A, fun({_,{branch, Si}}) ->
%       case Si of
%         [] -> true;
%          _ -> false
%       end
%     end),
%   lists:member(true, Results).

badJCombo1(A) ->
  F = fun({_,{branch, Si}}) ->
    case Si of
      [] -> true;
      _ -> false
    end
  end,
  lists:any(F, A).

% Returns true if a bad combo i.e., there is an element in I that is not in any branch Ji
badJCombo2(A, Indices) ->
  F = fun({_,{branch, Js}}) -> Js end,
  Bras = lists:map(F, A),
  F1 = fun({Label, _}) -> Label end,
  Labels = lists:map(F1, lists:flatten(Bras)),

  case lists:usort(Labels) =:= lists:usort(Indices) of
    true -> false;
    _ -> true
  end.


com1() -> {branch, [{a, {consume, a, endP}}, {b, {consume, b, endP}}, {c, endP} ] }.
com2() -> {branch, [{aa, {assert, a, endP}}, {bb, {assert, b, endP}}] }.

test({branch, LiSi2}) -> jBranch(LiSi2).



%% @doc Strip assertions
-spec strip(protocol()) -> protocol().
strip({act, N, P}) -> {act, N, strip(P)};
strip({assert, _, P}) -> strip(P);
strip({require, _, P}) -> strip(P);
strip({consume, _, P}) -> strip(P);
strip({branch, LiSi}) ->
  {branch, for(LiSi, fun({Li, Si}) -> {Li, strip(Si)} end)};
strip({rec, BV3, P}) -> {rec, BV3, strip(P)};
strip(P) -> P.

stripSet([]) -> [];
%change
% stripSet([X|XX]) -> [strip(X)] ++ stripSet(XX).
stripSet([X|XX]) -> [strip(X) | stripSet(XX)].


%% Substitute element in list
-spec lsubst([{string(), boolean()}], {string(), boolean()}) -> [{string(), boolean()}].
lsubst([],_) -> [];
lsubst([{T,false}|Tail],{T,false}) -> [{T,true}|Tail];
lsubst([ H | T ], F ) -> [H] ++ lsubst(T, F).



%% @doc Substitution
% -spec subst(protocol(), string(), string(), [string()]) -> protocol().
subst({act, Act, P}, BV1, BV2, A) -> {act, Act, subst(P, BV1, BV2, A)};
subst({assert, N, P}, BV1, BV2, A) -> {assert, N, subst(P, BV1, BV2, A)};
subst({require, N, P}, BV1, BV2, A) -> {require, N, subst(P, BV1, BV2, A)};
subst({consume, N, P}, BV1, BV2, A) -> {consume, N, subst(P, BV1, BV2, A)};
subst({branch, LiSi}, BV1, BV2, A) ->
  % {branch, for(LiSi, fun({Li, Si}) ->
  %   {Li, subst(Si, BV1, BV2, A)} end)};
  {branch, lists:map(fun({Label, Br}) ->
      {Label, subst(Br, BV1, BV2, A)} end, LiSi)};

subst({rec, BV3, P}, BV1, BV2, A) ->
  case lists:member(BV1, A) of
    true -> {rec, BV3, subst(P, BV1, BV2, A)};
    false -> {rec, BV3, subst(P, BV1, BV2, [BV3|A])}
  end;
subst({rvar, BV1}, BV1, BV2, A) ->
  case lists:member(BV1, A) of
    true -> {rvar, BV1};
    false -> {rvar, BV2}
  end;
subst({rvar, BV3}, _, _, _) -> {rvar, BV3};
subst(endP, _ , _ , _ ) -> endP.


%function that changes all rvar into standardized ones -- numbers
-spec recUnify(protocol(),number()) -> protocol().
recUnify({rec, BV, P}, NBV) -> {rec, integer_to_list(NBV), subst(recUnify(P, NBV+1), BV, integer_to_list(NBV), [])};
recUnify({rvar, BV}, _) -> {rvar, BV};
recUnify(endP, _) -> endP;

recUnify({act, Act, P}, NBV) -> {act, Act, recUnify(P, NBV)};
recUnify({assert, N, P}, NBV) -> {assert, N, recUnify(P, NBV)};
recUnify({require, N, P}, NBV) -> {require, N, recUnify(P, NBV)};
recUnify({consume, N, P}, NBV) -> {consume, N, recUnify(P, NBV)};
recUnify({branch, Brs}, NBV) ->
  F = fun({Label, Br}) ->
    {Label, recUnify(Br, NBV)} end,
  {branch, lists:map(F, Brs)}.

%nub
%function that makes it pretty

%% @doc Assertedness
% WIP: defaults to well-asserted
-spec asserted([atom()], protocol()) -> [atom()] | 'illAsserted'.
asserted(A , endP) -> A;
asserted(A, {rvar, _}) -> A;
asserted(A, {act, _, P}) -> asserted(A, P);
asserted(A, {branch, LiSi}) ->
  Abranches = for(LiSi, fun({_,Si}) -> asserted(A, Si) end),
  case listAsserted(Abranches) of
    true -> listIntersect(Abranches);
    false -> 'illAsserted'
  end;
asserted(A, {require, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(A, P);
    false -> 'illAsserted'
  end;
asserted(A, {consume, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(lists:delete(N,A), P);
    false -> 'illAsserted'
  end;
asserted(A, {assert, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(A, P);
    false -> asserted([N|A], P)
  end;
asserted(A, {rec, _, P}) ->
  case asserted(A, P) of
    illAsserted -> illAsserted;
    B -> [B|A]
  end.
wellAsserted(A, PS) ->
  case asserted(A, PS) of
    illAsserted -> false;
    _           -> true
  end.

%% @doc Helper functions for assertedness
listAsserted([A|Alist]) ->
  case A of
    illAsserted -> false;
    _ -> listAsserted(Alist)
  end;
listAsserted([]) -> true.

listIntersect(A) ->
  sets:to_list(sets:intersection(for(A, fun(X) -> sets:from_list(X) end))).

%% @doc Helper functions of binders
%%Predicate on whether protocol P has all its free variables in environment N
bound(P, N) ->
  case P of
    {act, _, R} -> bound(R,N);
    {assert, _, R} -> bound(R,N);
    {require, _, R} -> bound(R,N);
    {consume, _, R} -> bound(R,N);
    {branch, LiSi} -> lists:all(fun(X) -> X end, for(LiSi, fun({_, Si})-> bound(Si,N) end) );
    {rec, T, R} -> bound(R, [T | N]);
    {rvar, T} -> lists:member(T,N);
    endP -> true
  end.

%% @doc Interleaving
%% Helper for plumbing non-determinstic results (represented as lists)
%% into functions which are non-determinstic (return a list of results)
-spec bind([A], fun((A) -> [B])) -> [B].
bind([], _F)    -> [];
bind([X|XS], F) -> F(X) ++ bind(XS, F).


%% @doc Basically just flip map
-spec for([A], fun((A) -> B)) -> [B].
for(XS, F) -> lists:map(F, XS).

%% @doc Remove duplicate elements
-spec nub([A]) -> [A].
nub(X) -> nub(X, []).
nub([], Clean) -> lists:reverse(Clean);
nub([X|Xs], Clean) ->
    case lists:member(X, Clean) of
        true -> nub(Xs, Clean);
        false -> nub(Xs, [X|Clean])
    end.

%% @doc Remove empty list elements from a list
filter_empty(Data) when is_list(Data) ->
  Pred = fun(Element) -> Element /= [] end,
  lists:filter(Pred, Data). 




%% @doc Compute a covering of a set (with two partitions)
twoCovering([])  -> [];
twoCovering([A]) -> [{[A], []}, {[], [A]}];
twoCovering([A|AS]) -> bind(twoCovering(AS), fun({XS, YS}) -> [{[A|XS], YS}, {XS, [A|YS]}] end).


%finds if there are used variables in recursion environment
-spec unused([{string(),boolean()}])  -> boolean().
 unused([]) -> true;
 unused([{_,true}|_]) -> false;
 unused([{_,false}|T]) -> unused(T).

 -spec subUsedW([{string(),boolean()}])  -> [{string(),boolean()}].
 subUsedW([]) -> [];
 subUsedW([{_,false}|T]) ->  subUsedW(T);
 subUsedW(X) -> X.
 subUsed(L) -> lists:reverse(subUsedW(lists:reverse(L))).


-spec subUnusedW([{string(),boolean()}])  -> [{string(),boolean()}].
subUnusedW([]) -> [];
subUnusedW([{T,false}|L]) -> [{T,false}|subUnusedW(L)];
subUnusedW([{_,true}|_]) -> [].
subUnused(L) -> lists:reverse(subUnusedW(lists:reverse(L))).


%% @doc Take the largest list in a list of lists
maximalPossibility(XS) -> maximalPoss(XS, []).
maximalPoss([], Max) -> Max;
maximalPoss([XS|XSS], Max) when length(XS) >= length(Max) -> maximalPoss(XSS, XS);
maximalPoss([_|XSS], Max)  -> maximalPoss(XSS, Max).



recUnified(Comps) -> lists:map(fun(C) -> recUnify(C, 1) end, Comps).

% Top-level
-spec interleave(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleave(S1, S2) -> nub(recUnified(interleaveTop(strong, [], [], [], S1, S2))).


-spec interleaveWeak(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveWeak(S1, S2) -> nub(recUnified(interleaveTop(weak, [], [], [], S1, S2))).

-spec interleaveAll(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveAll(S1, S2) ->
    nub(recUnified(interleaveTop(all, [], [], [], S1, S2))).

-spec interleaveCorrelating(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveCorrelating(S1, S2) ->
    nub(recUnified(interleaveTop(correlating, [], [], [], S1, S2))).


%% @doc n-way Cartesian product
-spec nCartesian([[A]]) -> [[A]].
nCartesian([]) -> [];
%% XS is one list, [XS] is the list of one list of lists
nCartesian([XS]) -> lists:map(fun (X) -> [X] end, XS);
nCartesian([XS|XSS]) -> bind(XS, fun(X) -> bind(nCartesian(XSS), fun(YS) -> [[X|YS]] end) end).

%% @doc Takes
%%   - a list TL of recursion variables [string()] bound on the left
%%   - a list TR of recursion variables [string()] bound on the right
%%   - a list of atoms for the asserted names
%%   - left protocol
%%   - right protocol
%% This function should be used in all recursive calls since it also implements
%% the symmetry rule, where as interleaveMain does the main, asymmetrical work
-spec interleaveTop(atom(), [{string(),boolean()}], [{string(),boolean()}], [atom()], protocol(), protocol()) -> [protocol()].
%% [sym] rule
interleaveTop(WeakFlag, TL, TR, A, S1, S2) ->
  interleaveMain(WeakFlag, TL, TR, A, S1, S2) ++
    interleaveMain(WeakFlag, TR, TL, A, S2, S1).

%% @doc Asymmetrical (left-biased) rules
-spec interleaveMain(atom(), [{string(),boolean()}], [{string(),boolean()}], [atom()], protocol(), protocol()) -> [protocol()].
%% [end] rule
interleaveMain(_, _, _, _, endP, endP) -> [endP];
%% [act] rule
interleaveMain(WeakFlag, TL, TR, A, {act, P, S1}, S2) ->
  LS = interleaveTop(WeakFlag, TL, TR, A, S1, S2),
  lists:map(fun(S) -> {act, P, S} end, LS);

%% [require] rule
interleaveMain(WeakFlag, TL, TR, A, {require, N, S1}, S2) ->
  % io:format("S1 ~p~n", [S1]),
  % io:format("S2 ~p~n", [S2]),
  % io:format("N ~p~n", [lists:member(N, A)]),

  case lists:member(N, A) of
    true ->
      % Induct
      for(interleaveTop(WeakFlag, TL, TR, A, S1, S2)
        , fun(S) -> {require, N, S} end);
    false -> [] % Fail
  end;
%% [consume] rule
interleaveMain(WeakFlag, TL, TR, A, {consume, N, S1}, S2) ->
  case lists:member(N, A) of
    true ->
      % Induct
      for(interleaveTop(WeakFlag, TL, TR, lists:delete(N, A), S1, S2)
        , fun(S) -> {consume, N, S} end);
    false -> [] % Fail
  end;
%% [assert] rule
interleaveMain(WeakFlag, TL, TR, A, {assert, P, S1}, S2) ->
  for(interleaveTop(WeakFlag, TL, TR, [P|A], S1, S2)
      , fun(S) -> {assert, P, S} end);

%% [bra] rule
%% if for branches S0, S1, S2 we get the following possible interleavings with S2
%%   S0'_0, S0'_1
%%   S1'_0, S1'_1, S1'_2
%%   S2'_0, S2'_1, S3'_2
%% then nCartesian takes all possible combinations

%% LiSi is the list of label-protocol pairs
interleaveMain(_, _, _, _, {branch, []}, _) -> errorEmptyBranch;


interleaveMain(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ->
  case WeakFlag of
    strong -> intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2});
    weak -> intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2});
    correlating ->
      intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2})  ++ intCorrelating(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2});
    all -> intCorrelating(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ++ intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2})
  end;

interleaveMain(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2) ->
  case WeakFlag of
    strong -> intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2);
    weak -> intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2);
    correlating ->
      intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2);
    all -> intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2)  ++ intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2)
  end;



%% [rec1]
interleaveMain(WeakFlag, TL, TR, A, {rec, BV1, S1}, {rec, BV2, S2}) ->
  % Top(S1) not a recursion
  case S1 of
    {rec, _, _} -> [];
    _ -> for(
          interleaveTop(WeakFlag, TL ++ [{BV1, false}], TR, A, S1, {rec, BV2, S2})
          , fun(S) ->
          case wellAsserted(A, {rec, BV1, S}) of
                        true -> {rec, BV1, S};
                        false -> []
                      end
            end)
  end;




 %% [rec3]
interleaveMain(_, _, _, A, {rec, BV1, S1}, endP) ->
  case wellAsserted(A, {rec, BV1, S1}) and bound({rec, BV1, S1},[]) of
    true -> [{rec, BV1, S1}];
    false -> []
  end;

 %% [rec2]
interleaveMain(WeakFlag, TL , TR, A, {rec, BV1, S1}, S2) ->
 case S1 of
    % TOP check
    {rec, _, _} -> [];
    _ -> lists:append(for(subUnused(TR), fun({S,B})->
            interleaveTop(WeakFlag, TL, lsubst(TR,{S,B}), A, subst(S1, BV1, S, []), S2) end))
  end;





%% [call]
interleaveMain(_, TL, TR , _, {rvar, BV1}, {rvar, BV1}) ->
  case lists:member({BV1,true}, TL) or lists:member({BV1,true}, TR) of
    true -> [{rvar, BV1}];
    false -> []
  end;
%% check top and well assertedness
interleaveMain(_, _, _, _, _, _) -> [].


 %% [bra]
intStrong(WeakFlag, TL, TR, A, {branch, LiSi}, S2) ->
  Covering = [{LiSi, []}],
  Possibilities = for(Covering,
    fun ({Ia, Ib}) ->
    % Good parition if all Sb are well asserted
    case lists:all(fun ({_, Sib}) -> wellAsserted(A, Sib) end, Ib) of
      % Good parition
      true -> AllCombinations = nCartesian(for(Ia,
                    fun ({Li, Si}) ->
                    % Find all intereleavings for Si with S2 - put with its label
                    % with possible weakening modes
                    for(interleaveTop(WeakFlag, TL, TR, A, Si, S2),
                          fun(Sip) -> {Li, Sip} end)
                    end)),
        for(AllCombinations, fun(LiSip) -> {branch, LiSip ++ Ib} end);

      % Bad partition Ib is not all well-asserted
      false -> []
    end
  end),
  lists:concat(Possibilities).

  %% [wbra]
intWeak(WeakFlag, TL, TR, A, {branch, LiSi}, S2) ->
  Covering = twoCovering(LiSi),
  Possibilities = for(Covering,
    fun ({Ia, Ib}) ->
    % Good parition if all Sb are well asserted
    case lists:all(fun ({_, Sib}) -> wellAsserted(A, Sib) end, Ib) of
      % Good parition
      true -> AllCombinations = nCartesian(for(Ia,
                    fun ({Li, Si}) ->
                    % Find all intereleavings for Si with S2 - put with its label
                    % with possible weakening modes
                    for(interleaveTop(WeakFlag, TL, TR, A, Si, S2),
                          fun(Sip) -> {Li, Sip} end)
                    end)),
        for(AllCombinations, fun(LiSip) -> {branch, LiSip ++ Ib} end);

      % Bad partition Ib is not all well-asserted
      false -> []
    end
  end),
  maximalPossibility(Possibilities).


%% [cbra]
intCorrelating(WeakFlag, TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ->
  I = for(LiSi2, fun({Li, _}) -> Li end),
  RightSubsets = jBranch(LiSi2),

  % Meat
  LeftAndRightSubsetCombos =
     % For each {li : Si}
     for(LiSi1, fun({Li, Si}) ->
        % For each subset of the {lj , Sj} branches
        lists:foldl(fun(Subset, X) ->
          % associate with Li a branch...
          %... all the possibile unique {lj, Sj} pairs where Si and Sj compose
        Br = nub([{Lj, S} || {Lj, Sj} <- Subset, S <- interleaveTop(WeakFlag, TL, TR, A, Si, Sj)]),
        % skip empty lists 
        case Br of 
          [] -> X;
          _ -> [{Li, {branch, Br}} | X]
        end  
        end, [], RightSubsets)
      end),

  % Now choose all combiations across branches
  Results = for((nCartesian(LeftAndRightSubsetCombos)), fun (Branches) ->
                % check all branches of J are covered in the I branches overall (in the paper U_{j\in J} = J)
                 case badJCombo2(Branches, I) of
                            true -> [];
                            false -> {branch, Branches}
                          end
             end),
%remove empty list
filter_empty(Results).


% Factorization - ongoing work
%[Fprex1]
fact({act, A, S1}, {act, A, S2}) ->
  fact(S1,S2);

fact({assert, A, S1}, {assert, A, S2}) ->
  fact(S1,S2);

fact({consume, A, S1}, {consume, A, S2}) ->
  fact(S1,S2);

fact({require, A, S1}, {require, A, S2}) ->
  fact(S1,S2);

%[Fprex2]
fact({act, A, S1}, S2) ->
  {act, A, fact(S1,S2)};

fact({assert, A, S1}, S2) ->
  {assert, A, fact(S1,S2)};

fact({consume, A, S1}, S2) ->
  {consume, A, fact(S1,S2)};

fact({require, A, S1}, S2) ->
  {require, A, fact(S1,S2)};

%[Fbra1] with I = J
fact({branch, LiSi } , {branch, RiSi}) ->
  L = bramatch(LiSi,RiSi),
  S = lists:last(L),
  case lists:all(fun(X) -> (X == S) end, L)  of
      true -> S;
      false -> L
  end;


%[Fbra2]
fact({branch, LiSi } , S) ->
  {branch , for(LiSi, fun({A,R}) -> {A,fact(R,S)} end) };


fact({rec, T1, S1}, {rec, T1, S2}) -> fact(S1,S2);

fact({rec, T1, S1}, {rec, T2, S2}) -> fact(S1,subst(S2, T1, T2, []));

fact({rec, T, S}, _) -> {rec, T, S};


fact({rvar, T1}, {rvar, T1}) -> {rvar, T1};


fact(S, {rvar, _}) -> S;

fact(endP, _) -> endP;

fact(_, endP) -> endP.

bramatch([{A,S}],[{A,T}]) -> [fact(S,T)];
bramatch([{A,S}|B1],[{A,T}|B2]) -> [fact(S,T) | bramatch(B1,B2)];
bramatch(_,_)-> noP.




%% timeout branch 

e4t() ->
  {branch, [{l, {act, b, {assert, n, endP}}} ,{r, {act, c, {assert, n, endP}}}], aft, t, endP}.

e5t() ->
  {branch, [{l, {assert, n, endP}} ,{r, {assert, n, endP}}, {m, {assert, n, endP}}], aft, t, endP}.

e6t() ->
  {branch, [{l, {require, n, endP}} ,{r, {act, c, endP}}, {m, {assert, n, endP}}], aft, t, endP}.

e7t() ->
  {act, r_pwd, {branch, [{ok, {assert, n, endP}},{fail, endP}], aft, t, endP}}.

e10t() ->
  {rec, "y", {act, a, {branch, [{l, {act, b, {require, n, endP}}}
                               ,{r, {rvar, "y"}}]}, aft, t, endP}}.

bankt() ->
  {require, pin, {rec, "t", {branch, [{statement, {act, s_statement, {rvar, "t"}}},
                                    {payment, {assert, pay,{consume, tan,{act, r_details,  {rvar, "t"}}}}},
                                    {logout, {consume, pin, endP}}], aft, t, endP
                          }
                  }
  }.

pintant() ->
  {act, r_pin, {branch, [
                          {ok, {assert, pin, {rec, "r", {consume, pay, ctan()}}}},
                          {fail, endP}], aft, t, endP
                }
  }.

ctant() ->
   {act, s_id, {act, r_tan, {branch, [{tok, {assert, tan, {rvar, "r"}}},
                                            {tfail, {rvar, "r"}}], aft, t, endP
                            }
              }
  }.
