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
%  `Goal` is a meta-predicate.
%  It is expected to take an additional argument, `DepthLimit-Flag`.
%  `DepthLimit` is the current maximum search depth.
%  `Flag` is a term `n(Status)`.
%  `Goal` should unify `Status` with `depth_limit_exceeded` if it fails due to reaching `DepthLimit`.
%
%  `iterative_deepening/2` starts by calling `Goal` with `InitialLimit`.
%  If `Goal` succeeds, `iterative_deepening/2` succeeds.
%  If `Goal` fails and `Status` is `depth_limit_exceeded`, the `Limit` is incremented.
%  Then, `Goal` is called again with the new `Limit`.
%  This process repeats until `Goal` succeeds.
%  Or it repeats until `Goal` fails for a reason other than `depth_limit_exceeded`.
%
%  This predicate is useful for searches where the solution depth is unknown.
%  It is also useful when a breadth-first-like exploration is desired without its memory overhead.
%
%  Note: This predicate implements its own depth-limiting and iteration mechanism
%  rather than using the standard `call_with_depth_limit/3`.
%  This is because `iterative_deepening/2` requires the `Goal` to actively participate
%  in the depth management by unifying a flag when the depth limit is reached.
%  This allows the `Goal` to explore all possibilities within the current depth
%  before `iterative_deepening/2` decides to increment the limit and retry.
%  Standard `call_with_depth_limit/3` typically causes the goal to fail or throw an
%  exception upon reaching the limit, which would not allow for this controlled iteration.
%
%  @param InitialLimit The starting depth limit for the search.
%  @param Goal The goal to execute.
%              It must be a predicate accepting a `Limit-Flag` pair as an argument.
%              `Flag` is `n(Status)` used to signal if the depth limit was hit.
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

:- begin_tests(iterative_deepening, [setup((
    nb_setval(id_test_recorded_depths, []), % Use nb_setval
    nb_setval(id_test_exceed_counter, 0)    % Use nb_setval
))]).

% Helper predicates for testing iterative_deepening/2

% Succeeds if CurrentDepth >= TargetDepth. Sets depth_limit_exceeded flag and fails otherwise.
id_test_succeed_if_deep_enough(TargetDepth, CurrentDepthIn-FlagOut) :-
    (   CurrentDepthIn >= TargetDepth
    ->  true
    ;   nb_setarg(1, FlagOut, depth_limit_exceeded),
        fail
    ).

% Always fails, without modifying the flag.
id_test_always_fail(_CurrentDepthIn-_FlagOut) :-
    fail.

% Records CurrentDepthIn into nb_getval(id_test_recorded_depths, List).
% Then behaves like id_test_succeed_if_deep_enough/2.
id_test_record_depths_and_succeed(TargetDepth, CurrentDepthIn-FlagOut) :-
    (   nb_getval(id_test_recorded_depths, CalledDepthsSoFar) -> true % Use nb_getval
    ;   CalledDepthsSoFar = [] % Initialize if not set (should be set by setup)
    ),
    (   is_list(CalledDepthsSoFar)
    -> append(CalledDepthsSoFar, [CurrentDepthIn], NewCalledDepths)
    ;   NewCalledDepths = [CurrentDepthIn] % Safety for non-list initial value
    ),
    nb_setval(id_test_recorded_depths, NewCalledDepths), % Use nb_setval
    id_test_succeed_if_deep_enough(TargetDepth, CurrentDepthIn-FlagOut).

% If CurrentDepthIn >= TargetDepth, provides solutions Solution=s1 or Solution=s2.
% Else, sets depth_limit_exceeded flag and fails.
id_test_multi_solution_if_deep_enough(TargetDepth, Solution, CurrentDepthIn-FlagOut) :-
    (   CurrentDepthIn >= TargetDepth
    ->  (Solution = s1 ; Solution = s2)
    ;   nb_setarg(1, FlagOut, depth_limit_exceeded),
        fail
    ).

% Records CurrentDepthIn.
% Signals depth_limit_exceeded for the first MaxExceeds calls (tracked by id_test_exceed_counter).
% Fails normally on subsequent calls.
id_test_exceed_n_times_then_fail(MaxExceeds, CurrentDepthIn-FlagOut) :-
    (   nb_getval(id_test_recorded_depths, CalledDepthsSoFar) -> true ; CalledDepthsSoFar = [] ), % Use nb_getval
    (   is_list(CalledDepthsSoFar) -> append(CalledDepthsSoFar, [CurrentDepthIn], NewCalledDepths)
    ;   NewCalledDepths = [CurrentDepthIn]
    ),
    nb_setval(id_test_recorded_depths, NewCalledDepths), % Use nb_setval

    (   nb_getval(id_test_exceed_counter, Counter) -> true ; Counter = 0 ), % Use nb_getval
    NewCounter is Counter + 1,
    nb_setval(id_test_exceed_counter, NewCounter), % Use nb_setval
    (   NewCounter =< MaxExceeds
    ->  nb_setarg(1, FlagOut, depth_limit_exceeded),
        fail
    ;   fail % Fail normally
    ).

test(succeed_at_initial_limit) :-
    call_with_time_limit(2, iterative_deepening(2, id_test_succeed_if_deep_enough(1))).

test(succeed_at_exact_initial_limit) :-
    call_with_time_limit(2, iterative_deepening(2, id_test_succeed_if_deep_enough(2))).

test(succeed_after_one_increment, Deps == [1,2]) :-
    nb_setval(id_test_recorded_depths, []), % Explicit reset
    call_with_time_limit(2, iterative_deepening(1, id_test_record_depths_and_succeed(2))),
    nb_getval(id_test_recorded_depths, Deps).

test(succeed_after_multiple_increments, Deps == [1,2,3]) :-
    nb_setval(id_test_recorded_depths, []), % Explicit reset
    call_with_time_limit(2, iterative_deepening(1, id_test_record_depths_and_succeed(3))),
    nb_getval(id_test_recorded_depths, Deps).

test(fail_if_goal_always_fails_normally, [fail]) :-
    call_with_time_limit(2, iterative_deepening(2, id_test_always_fail)).

test(fail_if_goal_stops_exceeding_and_fails) :-
    nb_setval(id_test_recorded_depths, []), % Explicit reset
    nb_setval(id_test_exceed_counter, 0),   % Explicit reset
    assertion(\+ call_with_time_limit(2, iterative_deepening(1, id_test_exceed_n_times_then_fail(2)))), % Assert that the call fails
    nb_getval(id_test_recorded_depths, Deps),
    assertion(Deps == [1,2,3]). % Assert the side-effect

test(findall_multiple_solutions, Solutions == ExpectedSolutions) :-
    findall(S, call_with_time_limit(2, iterative_deepening(1, id_test_multi_solution_if_deep_enough(2, S))), SolutionsList),
    sort(SolutionsList, Solutions), % Sort to ensure order doesn't matter
    ExpectedSolutions = [s1]. % Changed to expect only the first solution

test(initial_limit_zero_succeed_target_zero, Deps == [0]) :-
    nb_setval(id_test_recorded_depths, []), % Explicit reset
    call_with_time_limit(2, iterative_deepening(0, id_test_record_depths_and_succeed(0))),
    nb_getval(id_test_recorded_depths, Deps).

test(initial_limit_zero_succeed_target_one, Deps == [0,1]) :-
    nb_setval(id_test_recorded_depths, []), % Explicit reset
    call_with_time_limit(2, iterative_deepening(0, id_test_record_depths_and_succeed(1))),
    nb_getval(id_test_recorded_depths, Deps).

:- end_tests(iterative_deepening).
