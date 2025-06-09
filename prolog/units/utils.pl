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
%  Converts an arithmetic `Expression` involving multiplication (`*`),
%  division (`/`), and exponentiation (`**`) into a canonical, simplified form.
%  This predicate is crucial for ensuring that semantically equivalent expressions
%  (e.g., `metre*second/second` and `metre`) are represented identically.
%  This facilitates reliable comparison and manipulation of units, quantities,
%  and numerical factors.
%
%  The `NormalizedExpression` will have its terms sorted alphabetically.
%  Terms in the numerator appear with positive exponents.
%  If the exponent is 1, no exponent is shown.
%  Terms in the denominator are represented with positive exponents after the `/` operator.
%
%  Edge Cases:
%  * If all terms in the expression cancel out (e.g., `metre/metre`),
%    `NormalizedExpression` will be the atom `1`.
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

%% normalize_dimension(+Expression, -NormalizedDimension) is det.
%
%  Normalizes a quantity dimension `Expression`.
%  It behaves like normalize/2, but maps a dimensionless result (i.e., `1`)
%  to the atom `dim_1`. This atom is the canonical representation for
%  dimensionless quantities, used by predicates like quantity_dimensions/2.
%  For other expressions, its behavior is identical to normalize/2.
%
%  Examples:
%  ==
%  ?- normalize_dimension(isq:dim_length/isq:dim_length, Dim).
%  Dim = dim_1.
%
%  ?- normalize_dimension(isq:dim_length*isq:dim_time/isq:dim_time, Dim).
%  Dim = isq:dim_length.
%  ==
%
%  @param Expression The input dimension expression.
%  @param NormalizedDimension Canonical form of `Expression`; `1` becomes `dim_1`.
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

%% normalize_factors(+FactorList, -NormalizedFactorList) is det.
%
%  Takes a list of `Term-Exponent` pairs (`FactorList`) and converts it
%  into a canonical, simplified form (`NormalizedFactorList`).
%  This is a key internal step in the normalization process.
%
%  The process involves:
%  1. Sorting the `FactorList`. `msort/2` is used, which sorts based on
%     the standard order of terms (the `Term` part of the pair first).
%  2. Aggregating factors: Identical `Term`s are combined by summing their
%     `Exponent`s. For example, `[metre-1, metre-1]` becomes `[metre-2]`.
%  3. Simplification:
%     - Terms with an exponent of `0` are removed (e.g., `[second-0]` is removed).
%     - Terms that are the atom `1` (e.g., `[1-3]`) are removed, as they
%       represent dimensionless numerical factors that are handled elsewhere or
%       become part of the overall numerical coefficient of an expression.
%
%  Example:
%  ==
%  ?- normalize_factors([metre-1, second-1, metre-1, second- -1, foo-0, 1-3], Norm).
%  Norm = [metre-2].
%
%  ?- normalize_factors([a-1, b-2, a- -1], Norm).
%  Norm = [b-2].
%  ==
%
%  @param FactorList A list of pairs, where each pair is `Term-Exponent`.
%                    `Term` is typically an atom (like a unit or quantity name)
%                    and `Exponent` is a number.
%  @param NormalizedFactorList The processed list of `Term-Exponent` pairs,
%                              sorted, aggregated, and simplified.
normalize_factors(L, L2) :-
   msort(L, L1),
   aggregate(L1, L2).

%% parse_normalize_factors(+Expression, -NormalizedFactorList) is det.
%
%  Combines parsing and factor normalization.
%  Convenience predicate equivalent to calling parse/2 followed by normalize_factors/2.
%
%  @param Expression The input expression to parse and normalize.
%  @param NormalizedFactorList The canonical list of `Term-Exponent` pairs.
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

%% generate_expression(+FactorList, -Expression) is det.
%
%  Converts a `FactorList` of `Term-Exponent` pairs into a standard
%  Prolog arithmetic `Expression`. This is the inverse operation of
%  parse/2 combined with normalize_factors/2.
%
%  The process involves:
%  1. Partitioning the `FactorList` into numerator terms (positive exponents)
%     and denominator terms (negative exponents).
%  2. Converting each term to its power form (e.g., `Term-1` becomes `Term`,
%     `Term-N` becomes `Term**N`). Denominator exponents are made positive.
%  3. Joining numerator terms with `*`.
%  4. Joining denominator terms with `*`.
%  5. Forming the final `Expression`:
%     - If only numerator terms: `Num1*Num2*...`
%     - If only denominator terms: `1/(Den1*Den2*...)`
%     - If both: `(Num1*Num2*...)/(Den1*Den2*...)`
%     - If `FactorList` is empty (e.g., after all terms cancel): `1`.
%
%  Examples:
%  ==
%  ?- generate_expression([metre-1, second- -2], Expr).
%  Expr = metre/second**2.
%
%  ?- generate_expression([kilogram-1, metre-1, second- -2], Expr).
%  Expr = kilogram*metre/second**2.
%
%  ?- generate_expression([joule-1], Expr).
%  Expr = joule.
%
%  ?- generate_expression([], Expr).
%  Expr = 1.
%
%  ?- generate_expression([second- -1], Expr).
%  Expr = 1/second.
%  ==
%
%  @param FactorList A list of `Term-Exponent` pairs, typically sorted and
%                    simplified by normalize_factors/2.
%  @param Expression The resulting Prolog arithmetic expression.
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

normalize_factors_test_data([metre-1, second-1, metre-1, second- -1, foo-0, 1-3], [metre-2]).
normalize_factors_test_data([a-1, b-2, a- -1], [b-2]).

test(normalize_factors_examples, [forall(normalize_factors_test_data(Input, ExpectedOutput))]) :-
    normalize_factors(Input, Output),
    assertion(Output == ExpectedOutput).

:- end_tests(utils).
