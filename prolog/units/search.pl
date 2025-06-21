:- module(search, [common_expr/6, iterative_deepening/2]).

:- use_module(utils).
:- use_module(quantity, [quantity_dimensions/2]).
:- use_module(unit_defs, [all_unit_kind/2]).
:- use_module('../units.pl').

:- meta_predicate common_expr(2, +, -, +, -, -).

%% common_expr(:ChildParentGoal, +Expr1, -Factor1, +Expr2, -Factor2, -CommonExpr) is nondet.
%
%  Finds a common base expression `CommonExpr` for two input expressions `Expr1` and
%  `Expr2` (typically units or quantities), along with their respective scaling factors
%  `Factor1` and `Factor2`.
%
%  The relationship established is that `Expr1*Factor1 = Expr2*Factor2 = CommonExpr`.
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
      {F1, NewF1, ChildParentGoal, NewUnits, F2, NewF2}/[N]>>partition_factors(
         F1, NewF1, ChildParentGoal, NewUnits, N, F2, NewF2))),
   normalize_factors(NewUnits, SortedNewUnits),
   maplist(generate_expression, [NewF1, NewF2, SortedNewUnits],
           [NewFactor1, NewFactor2, NewUnit]).

:- meta_predicate iterative_deepening(+, 1).

%% iterative_deepening(+InitialLimit, :Goal) is nondet.
%
%  Executes `Goal` using an iterative deepening search strategy.
%
%  `Goal` is expected to take an additional argument, `DepthLimit-Flag` where
%  `DepthLimit` is the maximum search depth and `Flag` is a term `n(Status)`.
%  `Goal` should set (with `nb_set/2`) `Status` to `depth_limit_exceeded` 
%  if it fails due to reaching `DepthLimit`.
%  In this case, `Goal` is called again with `DepthLimit + 1`.
%  If `Goal`, this predicate fails.
%
%  Note: We don't use `call_with_depth_limit/2` because the use of exception to
%  signal that the depth limit is reached will cut existing choice points
%  which should be explored.
%  Moreover, `Goal` is free to count depth level freely.
%
%  @param InitialLimit The starting depth limit for the search.
%  @param Goal The goal to execute.
%              It must be a predicate accepting a `Limit-Flag` pair as an argument.
%              `Flag` is `n(Status)` used to signal if the depth limit was hit.
iterative_deepening(Limit, Goal) :-
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

get_dimension(Type, U-E, Dim-(U-E)) :-
    (   var(U)
    -> Dim = var
    ;   not_factor(U-E)
    -> (   Type = _:unit_parent
        ->  all_unit_kind(U**E, K)
        ;   K = U**E
        ),
        quantity_dimensions(K, Dim)
    ;   Dim = 1
    ).
get_dimensions(Type, L, D) :-
    maplist(get_dimension(Type), L, D).

partition_factors(L1, R1, ChildParentGoal, L, N, L2, R2) :-
    get_dimensions(ChildParentGoal, L1, G1),
    get_dimensions(ChildParentGoal, L2, G2),
    partition_factors_(G1, R1, ChildParentGoal, L, N, G2, R2).

partition_factors_(L1, R1, ChildParentGoal, L, N, L2, R2) :-
    select(Dim-F1, L1, L11),
    selectchk(Dim-F2, L2, L22),
    common_factors([F1], R11, ChildParentGoal, LL, N, [F2], R22),
    !,
    append(R11, R111, R1),
    append(R22, R222, R2),
    append(LL, LLL, L),
    partition_factors_(L11, R111, ChildParentGoal, LLL, N, L22, R222).
partition_factors_(L1, R1, ChildParentGoal, L, N, L2, R2) :-
    pairs_values(L1, V1),
    pairs_values(L2, V2),
    common_factors(V1, R1, ChildParentGoal, L, N, V2, R2).

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
   nb_setarg(1, N, no).
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
   partition_factors(L1, R1, ChildParentGoal, L, N).

expand_factors(ChildParentGoal, A), Factors -->
   { expand_factor(ChildParentGoal, A, Factors) }.
expand_factor(ChildParentGoal, Child-N, Factors) :-
   call(ChildParentGoal, Child, Parent),
   parse_normalize_factors(Parent**N, Factors).

:- begin_tests(search, [timeout(1), setup(abolish_all_tables)]).

test(partition_factors) :-
    common_expr(units:unit_parent, si:metre, F1, usc:inch, F2, C),
    F1 == 1,
    F2 == 9144/(10000*3*12),
    C == si:metre.

test(partition_factors2) :-
    common_expr(units:alias_or_child_quantity_parent,
                isq:length/isq:time**2, 1, isq:acceleration, 1, C),
    C == isq:length/isq:time**2.

test(common_expr_var) :-
    common_expr(units:unit_parent, si:metre/T, F1, usc:inch/si:hour, F2, C),
    T = si:hour,
    F1 == 1,
    F2 == 9144/(10000*3*12),
    C == si:metre/si:hour.

test(common_expr_hard) :-
    common_expr(units:unit_parent,
                usc:foot*usc:pound_force/si:second, F1,
                si:watt, F2, C),
    F1 == 9144*45359237*980665/(10000*3*100000000*100000),
    F2 == 1,
    C == si:kilogram*si:metre**2/si:second**3.

:- end_tests(search).
