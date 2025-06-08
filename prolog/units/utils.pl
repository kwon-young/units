:- module(utils, [
   normalize/2,
   normalize_dimension/2,
   normalize_factors/2,
   parse_normalize_factors/2,
   generate_expression/2,
   mapexpr/2,
   mapexpr1/3,
   mapexpr/3,
   mapexpr/4
]).

%% normalize(+Expression, -NormalizedExpression) is det.
%
%  Converts an `Expression` into a canonical, simplified form.
%
%  `Expression` is typically an arithmetic expression involving units,
%  quantities, or numerical values, using operators `*`, `/`, and `**` (power).
%  The normalization process involves:
%
%  1. Parsing the `Expression` into a list of base terms, each associated with
%     an effective exponent. For example, `metre * second / second**2` would
%     internally be processed considering `metre` with exponent 1, and `second`
%     with exponent `1 - 2 = -1`.
%  2. Sorting the base terms alphabetically.
%  3. Aggregating identical base terms by summing their exponents. For instance,
%     `metre * metre` becomes `metre` with exponent 2.
%  4. Removing any terms whose resulting exponent is zero.
%  5. Reconstructing the `NormalizedExpression`:
%     - Terms with positive exponents form the numerator.
%     - Terms that had negative exponents (after aggregation) form the
%       denominator, shown with positive exponents.
%     - If all terms cancel out (e.g., `metre/metre`), `NormalizedExpression`
%       becomes the atom `1`.
%     - The order of terms in the numerator and denominator is alphabetical.
%
%  This predicate is crucial for comparing and simplifying unit expressions,
%  quantity dimension expressions, and compound numerical factors throughout
%  the `units` library.
%
%  Examples:
%  ==
%  ?- normalize(metre * second / second, Metres).
%  Metres = metre.
%
%  ?- normalize(metre * second**2 / second, MetreSeconds).
%  MetreSeconds = metre*second.
%
%  ?- normalize(kilogram * metre / second / second, NewtonExpr).
%  NewtonExpr = kilogram*metre/second**2.
%
%  ?- normalize(metre/metre, One).
%  One = 1.
%
%  ?- normalize(c * a * b, Sorted).
%  Sorted = a*b*c.
%
%  ?- normalize(joule/second*second, Joules).
%  Joules = joule.
%  ==
%
%  @param Expression The input expression to normalize.
%  @param NormalizedExpression The canonical, simplified form of `Expression`.

normalize(In, Out) :-
   parse(In, L),
   normalize_factors(L, L1),
   generate_expression(L1, Out).

normalize_dimension(In, Out) :-
   normalize(In, N),
   (  N == 1
   -> Out = dim_1
   ;  Out = N
   ).

parse(Expr, Factors) :-
   phrase(parse(1, Expr), Factors).

parse(Coeff, A*B) ==>
   parse(Coeff, A), parse(Coeff, B).
parse(Coeff, A/B) ==>
   parse(Coeff, A),
   parse(-Coeff, B).
parse(Coeff, A**N) ==>
   parse(Coeff*N, A).
parse(_, dim_1) ==>
   [].
parse(Coeff, A) ==>
   [A-Coeff].

normalize_factors(L, L2) :-
   msort(L, L1),
   aggregate(L1, L2).

parse_normalize_factors(In, L3) :-
   parse(In, L),
   normalize_factors(L, L3).

aggregate(L, L2) :-
   group_pairs_by_key(L, Groups),
   maplist([A-Ns, A-N]>>sum_list(Ns, N), Groups, L1),
   simplify(L1, L2).

simplify([], R) => R = [].
simplify([_-0 | T], R) =>
   simplify(T, R).
simplify([1-_ | T], R) =>
   simplify(T, R).
simplify([H | T], R) =>
   R = [H | L],
   simplify(T, L).

generate_expression(In, Out) :-
   partition(is_num, In, Num, Denom),
   maplist(power, Num, Num1),
   phrase(inverse(Denom), Denom1),
   maplist(power, Denom1, Denom2),
   num_denom(Num1, Denom2, Out).

is_num(_-N) :- N > 0.

power(A-1, A) :- !.
power(A-N, A**N).

inverse([]) --> [].
inverse([A-N | L]) -->
   { N1 is -N },
   [A-N1],
   inverse(L).

num_denom([], Denom, Expr) :-
   denom(Denom, 1, Expr).
num_denom([H | T], Denom, Expr) :-
   multiply([H | T], Num),
   denom(Denom, Num, Expr).

denom([], Num, Num).
denom([H | T], Num, Num/Expr) :-
   multiply([H | T], Expr).

multiply([H | T], Expr) :-
   foldl([B, A, A*B]>>true, T, H, Expr).

:- meta_predicate mapexpr(1, ?).

mapexpr(Goal, A) :-
   mapexpr1(Goal, [_]>>true, A).

:- meta_predicate mapexpr1(1, 1, ?).

mapexpr1(Goal, F, A*B) =>
   mapexpr1(Goal, F, A),
   mapexpr1(Goal, F, B).
mapexpr1(Goal, F, A/B) =>
   mapexpr1(Goal, F, A),
   mapexpr1(Goal, F, B).
mapexpr1(Goal, F, A**_) =>
   mapexpr1(Goal, F, A).
mapexpr1(Goal, Failure, A) =>
   (  call(Goal, A)
   *-> true
   ;  call(Failure, A)
   ).

:- meta_predicate mapexpr(2, ?, ?).

mapexpr(Goal, A, R) :-
   mapexpr(Goal, =, A, R).

:- meta_predicate mapexpr(2, 2, ?, ?).

mapexpr(Goal, F, A*B, R) =>
   mapexpr(Goal, F, A, A1),
   mapexpr(Goal, F, B, B1),
   R = A1*B1.
mapexpr(Goal, F, A/B, R) =>
   mapexpr(Goal, F, A, A1),
   mapexpr(Goal, F, B, B1),
   R = A1/B1.
mapexpr(Goal, F, A**B, R) =>
   mapexpr(Goal, F, A, A1),
   R = A1**B.
mapexpr(Goal, Failure, A, A1) =>
   (  call(Goal, A, A1)
   *-> true
   ;  call(Failure, A, A1)
   ).

:- begin_tests(utils).

normalize_test_data(metre * second / second, metre).
normalize_test_data(metre * second**2 / second, metre*second).
normalize_test_data(kilogram * metre / second / second, kilogram*metre/second**2).
normalize_test_data(metre/metre, 1).
normalize_test_data(c * a * b, a*b*c). % Test sorting
normalize_test_data(joule/second*second, joule). % Test cancellation with existing term

test(normalize_examples, [forall(normalize_test_data(Input, ExpectedOutput))]) :-
    normalize(Input, Output),
    assertion(Output == ExpectedOutput).

:- end_tests(utils).
