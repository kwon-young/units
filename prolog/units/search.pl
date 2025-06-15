:- module(search, [common_expr/6, iterative_deepening/2]).

:- use_module(utils).

:- meta_predicate common_expr(2, +, ?, +, ?, -).

%% common_expr(:ChildParentGoal, +Expr1, -Factor1, +Expr2, -Factor2, -CommonExpr) is nondet.
%
%  Finds a common base expression `CommonExpr` for two input expressions `Expr1` and
%  `Expr2` (typically units or quantities), along with their respective scaling factors
%  `Factor1` and `Factor2`.
%
%  The relationship established is that the term `Expr1*Factor1`,
%  the term `Expr2*Factor2`, and the term `CommonExpr` are all considered equivalent.
%
%  This predicate is tabled to memoize its results.
%  It employs an iterative deepening approach to search for the closest common ancestor
%  of `Expr1` and `Expr2`.
%  The search expands definitions (guided by the `ChildParentGoal` predicate,
%  e.g., for unit parents or quantity parents) to establish this commonality.
%
%  @param ChildParentGoal A meta-argument (predicate name) that defines how to expand
%              or find parents of elements within the expressions (e.g.,
%              `unit_parent` for units, `alias_or_child_quantity_parent`
%              for quantities).
%  @param Expr1 The first input expression (e.g., `si:metre`, `isq:speed`).
%  @param Factor1 The numerical scaling factor associated with `Expr1` in the context of the common relationship.
%  @param Expr2 The second input expression.
%  @param Factor2 The numerical scaling factor associated with `Expr2` in the context of the common relationship.
%  @param CommonExpr The common base expression derived from `Expr1` and `Expr2`.
common_expr(ChildParentGoal, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) :-
   common_expr_(ChildParentGoal, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

:- table common_expr_/6.

common_expr_(ChildParentGoal, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) :-
   parse_normalize_factors(Unit1, F1),
   parse_normalize_factors(Unit2, F2),
   once(iterative_deepening(1,
      {F1, NewF1, ChildParentGoal, NewUnits, F2, NewF2}/[N]>>common_factors(
         F1, NewF1, ChildParentGoal, NewUnits, N, F2, NewF2))),
   msort(NewUnits, SortedNewUnits),
   maplist(generate_expression, [NewF1, NewF2, SortedNewUnits],
           [NewFactor1, NewFactor2, NewUnit]),
   debug(common_expr, "~p*~p = ~p*~p = ~p~n", [Unit1, NewFactor1, Unit2, NewFactor2, NewUnit]).

:- meta_predicate iterative_deepening(+, 1).

%% iterative_deepening(+InitialLimit, :Goal) is nondet.
%
%  Executes `Goal` using an iterative deepening search strategy.
%
%  `Goal` is a meta-predicate that is expected to take an additional argument,
%  `DepthLimit-Flag`, where `DepthLimit` is the current maximum search depth and
%  `Flag` is a term `n(Status)`. `Goal` should unify `Status` with `depth_limit_exceeded`
%  if it fails due to reaching `DepthLimit`.
%
%  `iterative_deepening/2` starts by calling `Goal` with `InitialLimit`.
%  If `Goal` succeeds, `iterative_deepening/2` succeeds.
%  If `Goal` fails and `Status` is `depth_limit_exceeded`, the `Limit` is incremented,
%  and `Goal` is called again with the new `Limit`.
%  This process repeats until `Goal` succeeds or fails for a reason other than
%  `depth_limit_exceeded`.
%
%  This predicate is useful for searches where the solution depth is unknown and
%  a breadth-first-like exploration is desired without its memory overhead.
%
%  @param InitialLimit The starting depth limit for the search.
%  @param Goal The goal to execute. It must be a predicate accepting a `Limit-Flag` pair
%              as an argument, where `Flag` is `n(Status)` used to signal if the depth limit was hit.
iterative_deepening(Limit, Goal) :-
   debug(iterative_deepening, "Depth ~p~n", [Limit]),
   N = n(no),
   (  call(Goal, Limit-N)
   -> true
   ;  (  N = n(depth_limit_exceeded)
      -> Limit1 is Limit + 1,
         iterative_deepening(Limit1, Goal)
      ;  fail
      )
   ).

not_factor(X-_) :-
   \+ number(X),
   \+ X == pi.

select_(E, L, R) :-
   (  select(E, L, R)
   ;  L = R
   ).

common_factors(L1, R1, ChildParentGoal, L, N, L2, R2) :-
   exclude(ground, L1, Vars1),
   foldl(select_, Vars1, L2, _),
   exclude(ground, L2, Vars2),
   foldl(select_, Vars2, L1, _),
   partition(not_factor, L1, Unit1, Factor1),
   normalize_factors(Unit1, NUnit1),
   partition(not_factor, L2, Unit2, Factor2),
   normalize_factors(Unit2, NUnit2),
   ord_intersection(NUnit1, NUnit2, CommonUnits, Unit2Only),
   ord_subtract(NUnit1, NUnit2, Unit1Only),
   append(CommonUnits, R, L),
   append(Factor1, R11, R1),
   append(Factor2, R22, R2),
   expand_either_factors(Unit1Only, R11, ChildParentGoal, R, N, Unit2Only, R22).
expand_either_factors([], [], _, [], _-N, [], []) :-
   setarg(1, N, no).
expand_either_factors(L1, R1, ChildParentGoal, L, Limit-N, L2, R2) :-
   (  Limit > 0
   -> Limit1 is Limit - 1
   ;  nb_setarg(1, N, depth_limit_exceeded),
      fail
   ),
   (  phrase(select_factor(L1, R1, ChildParentGoal, L, Limit1-N), L2, R2)
   ;  phrase(select_factor(L2, R2, ChildParentGoal, L, Limit1-N), L1, R1)
   ).
select_factor(L1, R1, ChildParentGoal, L, N) -->
   select(A),
   {ground(A)},
   expand_factors(ChildParentGoal, A),
   common_factors(L1, R1, ChildParentGoal, L, N).

expand_factors(ChildParentGoal, A), Factors -->
   { expand_factor(ChildParentGoal, A, Factors) }.
expand_factor(ChildParentGoal, Child-N, Factors) :-
   call(ChildParentGoal, Child, Parent),
   parse_normalize_factors(Parent**N, Factors).
