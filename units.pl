:- use_module(library(dcg/high_order)).
:- use_module(si, []).
:- use_module(isq, []).

:- op(100, yf,  []).
:- op(100, xfy, :).
:- op(700, xfx, qis).

parse(A*B) ==>
   parse(A), parse(B).
parse(A/B) ==>
   parse(A),
   { phrase(parse(B), L) },
   sequence(inverse, L).
parse((A*B)^N) ==>
   parse(A^N*B^N).
parse((A/B)^N) ==>
   parse(A^N/B^N).
parse((A^N1)^N2) ==>
   { N is N1 + N2 },
   parse(A^N).
parse(A^N) ==>
   [A-N].
parse(A) ==>
   [A-1].

inverse(A-N) -->
   { N1 is -N },
   [A-N1].

normalize_pair(A-N, R) => R = A-N.
normalize_pair(A, R) => R = A-1.

aggregate(L, L1) :-
   maplist(normalize_pair, L, Pairs),
   group_pairs_by_key(Pairs, Groups),
   maplist([A-Ns, A-N]>>sumlist(Ns, N), Groups, L1).

simplify_(0-_, Res) => Res = 0.
simplify_(0-_, Res) => Res = 0.
simplify_(_-0, _) => fail.
simplify_(1-_, _) => fail.
simplify_(A-1, Res) => Res = A.
simplify_(A-N, Res) => Res = A^N.
simplify_(A, Res) => Res = A.
simplify(L, L1) :-
   convlist(simplify_, L, L1).

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

is_num(_-N) => N > 0.
is_num(_) => true.

normalize(In, Out) :-
   phrase(parse(In), L),
   msort(L, L1),
   aggregate(L1, L2),
   generate_expression(L2, Out).

generate_expression(In, Out) :-
   partition(is_num, In, Num, Denom),
   simplify(Num, Num1),
   phrase(sequence(inverse, Denom), Denom1),
   simplify(Denom1, Denom2),
   num_denom(Num1, Denom2, Out).

parse_normalize_factors(In, L3) :-
   phrase(parse(In), L),
   normalize_factors(L, L3).
normalize_factors(L, L3) :-
   msort(L, L1),
   aggregate(L1, L2),
   simplify(L2, L3).

implicitly_convertible__(1/kind(A), kind(1/A), _).
implicitly_convertible__(kind(A)/kind(B), kind(A/B), _).
implicitly_convertible__(kind(A)*kind(B), kind(A*B), _).
implicitly_convertible__(From, To, _) :-
   isq:quantity_parent(From, To),
   \+ isq:base_dimension(To, _).
implicitly_convertible__(From, To, Kinds) :-
   isq:quantity_parent(To, From),
   isq:kind(To),
   isq:quantity_kinds(To, ToKinds),
   % should be direct descendent or ascendent, no siblings
   forall(
      (member(ToKind, ToKinds), member(FromKind, Kinds)),
      (  isq:quantity_root(ToKind, FromKind)
      ;  isq:quantity_root(FromKind, ToKind)
      )
   ).
implicitly_convertible__(From, To, _) :-
   isq:quantity_formula(To, From).
implicitly_convertible__(kind(Q), Q, _).
implicitly_convertible__(kind(From), To, _) :-
   isq:quantity_parent(To, From).
implicitly_convertible__(From, To, _) :-
   isq:alias(To, From).
implicitly_convertible__(From, To, Kinds) :-
   From =.. [Name | Args],
   select(Arg, Args, Arg1, Args1),
   implicitly_convertible__(Arg, Arg1, Kinds),
   (  Name == kind
   -> isq:kind(Arg1)
   ;  true
   ),
   To =.. [Name | Args1].

:- table implicitly_convertible_/3.

implicitly_convertible_(Q, Q, _).
implicitly_convertible_(From, To, Kinds) :-
   implicitly_convertible__(From, New, Kinds),
   implicitly_convertible_(New, To, Kinds).

implicitly_convertible(From, To) :-
   quantity_kinds(From, Kinds),
   implicitly_convertible_(From, To, Kinds).

:- table explicitly_convertible/2.

explicitly_convertible(From, To) :-
   implicitly_convertible(From, To).
explicitly_convertible(From, To) :-
   implicitly_convertible(To, From).

quantity_kinds(kind(From), Kinds) =>
   quantity_kinds(From, Kinds).
quantity_kinds(From, Kinds) =>
   isq:quantity_kinds(From, Kinds).

:- table common_quantity(_, _, po(closest/2)).

closest(Q1, Q2) :-
   implicitly_convertible(Q1, Q2),
   \+ isq:alias(Q1, _).

common_quantity(Q1, Q2, Q) :-
   implicitly_convertible(Q1, Q),
   implicitly_convertible(Q2, Q).


same_kind(Q1, Q2) :-
   debug(same_kind, "~p, ~p~n", [Q1, Q2]),
   common_quantity(Q1, Q2, _).


common_unit(Unit1, NewUnit1, Unit2, NewUnit2) :-
   parse_normalize_factors(Unit1, F1),
   parse_normalize_factors(Unit2, F2),
   once(common_factors(F1, NewF1, F2, NewF2)),
   generate_expression(NewF1, NewUnit1),
   generate_expression(NewF2, NewUnit2).

is_number(N), number(N) => true.
is_number(N^_), number(N) => true.
is_number(_) => fail.

common_factors(L1, R1, L2, R2) :-
   partition(is_number, L1, N1, S1),
   partition(is_number, L2, N2, S2),
   ord_intersection(S1, S2, S, L22),
   ord_subtract(S1, S2, L11),
   append(N1, S, NS1),
   append(NS1, R11, R1),
   append(N2, S, NS2),
   append(NS2, R22, R2),
   common_factors_(L11, R11, L22, R22).
common_factors_([], [], [], []).
common_factors_([H1 | T1], R1, L2, R2) :-
   (  common_factors__([H1 | T1], R1, L2, R2)
   ;  common_factors__(L2, R2, [H1 | T1], R1)
   ).
common_factors__(L1, R1, L2, R2) :-
   select(A, L1, L11),
   expand_factors(A, L11, L111),
   common_factors(L111, R1, L2, R2).

expand_factors(A, L1, R1) :-
   expand_factor(A, Formula),
   parse_normalize_factors(Formula, Factors),
   append(Factors, L1, FactorsL1),
   normalize_factors(FactorsL1, R1).
expand_factor(Unit^N, R) =>
   expand_factor(Unit, Formula),
   R = Formula^N.
expand_factor(System:Unit, R), System:unit(Unit, _, Formula), Formula \== System:Unit =>
   R = Formula.
expand_factor(System:Symbol, R), System:unit(Unit, Symbol, _) =>
   R = System:Unit.
expand_factor(_, _) => fail.



eval(Expr) :-
   eval_(Expr, R),
   call(R.v).
qis(Result, ExprIn) :-
   eval_(ExprIn, ExprOut),
   V is ExprOut.v,
   Result = q{v: V, q: ExprOut.q, u: ExprOut.u}.
eval_(A == B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   (  common_quantity(A1.q, B1.q, Q)
   -> common_unit(A1.u, U1, B1.u, U2),
      eval_(A1.v * U1, A2),
      eval_(B1.v * U2, B2),
      V = (A2.v =:= B2.v),
      R = q{v: V, q: Q, u: A2.u}
   ;  domain_error(A1, B1)
   ).
eval_(A*B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   normalize(A1.q*B1.q, Q),
   normalize(A1.u*B1.u, U),
   normalize(A1.v*B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(A/B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   normalize(A1.q/B1.q, Q),
   normalize(A1.u/B1.u, U),
   normalize(A1.v/B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(A+B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   (  common_quantity(A1.q, B1.q, Q)
   -> common_unit(A1.u, U1, B1.u, U2),
      eval_(A1.v * U1, A2),
      eval_(B1.v * U2, B2),
      normalize(Q, Q1),
      normalize(U2, U),
      R = q{v: A2.v+B2.v, u: U, q: Q1}
   ;  domain_error(A1, B1)
   ).
eval_(A^N, R) =>
   eval_(A, A1),
   normalize(A1.q^N, Q),
   normalize(A1.u^N, U),
   normalize(A1.v^N, V),
   R = q{v: V, q: Q, u: U}.
eval_(System:Unit, R), System:unit(Unit) =>
   System:unit_kind(Unit, Kind),
   normalize(Kind, Kind1),
   R = q{v: 1, q: kind(Kind1), u: System:Unit}.
eval_(Number, R), number(Number) =>
   R = q{v: Number, q: 1, u: 1}.
eval_(Quantity[Unit], R), isq:quantity_parent(Quantity, _) =>
   eval_(Unit, Sub),
   same_kind(Sub.q, Quantity),
   R = q{v: 1, q: Quantity, u: Sub.u}.
eval_(Quantity, R), is_dict(Quantity, q) =>
   R = Quantity.

M.in(Unit) := R :-
   eval_(Unit, Q),
   (  implicitly_convertible(Q.q, M.q)
   -> common_unit(M.u, U1, Q.u, U2),
      eval_(U1/U2, ConversionFactor),
      normalize(M.v*ConversionFactor.v, V),
      R = q{v: V, q: M.q, u: Q.u}
   ;  domain_error(M, Unit)
   ).

M.as(Quantity) := R :-
   (  explicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).

:- table same_unit/2.

same_unit(U, U).
same_unit(System:U1, System:U2) :-
   System:alias(U1, U2).
same_unit(System:U1, System:U2) :-
   System:unit(U1, U2, _).
same_unit(U1, U2) :-
   same_unit(U2, U1).

same_units(U1, U2) :-
   parse_normalize_factors(U1, F1),
   parse_normalize_factors(U2, F2),
   maplist(same_unit, F1, F2).

qmust_be(Quantity[Unit], Q) =>
   qmust_be(Quantity, Q),
   (  same_units(Unit, Q.u)
   -> true
   ;  domain_error(Unit, Q.u)
   ).
qmust_be(Quantity, Q) =>
   (  implicitly_convertible(Q.q, Quantity)
   -> true
   ;  domain_error(Quantity, Q.q)
   ).

:- begin_tests(units).

test('si:metre == si:metre') :-
   eval(si:metre == si:metre).

test('si:kilo(metre) == si:kilo(metre)') :-
   eval(si:kilo(metre) == si:kilo(metre)).

test('si:kilogram == si:kilo(gram)') :-
   eval(si:kilogram == si:kilo(gram)).

test('si:kg == si:kilo(gram)') :-
   eval(si:kg == si:kilo(gram)).

test('10*(si:kilo(metre)) == 5*2*(si:kilo(metre))') :-
   eval(10*(si:kilo(metre)) == 5*2*(si:kilo(metre))).

test('10*(si:kilo(metre)) / 2 == 5*(si:kilo(metre))') :-
   eval(10*(si:kilo(metre)) / 2 == 5*(si:kilo(metre))).

test('1 * (si:hour) == 3600 * (si:second)') :-
   eval(1 * (si:hour) == 3600 * (si:second)).

test('1 * (si:kilo(metre)) + 1 * (si:metre) == 1001 * (si:metre)') :-
   eval(1 * (si:kilo(metre)) + 1 * (si:metre) == 1001 * (si:metre)).

test('1 * (si:kilo(metre)) / (1 * (si:second)) == 1000 * (si:metre) / (si:second)') :-
   eval(1 * (si:kilo(metre)) / (1 * (si:second)) == 1000 * (si:metre) / (si:second)).

test('2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) == 4 * (si:kilo(metre))') :-
   eval(2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) == 4 * (si:kilo(metre))).

test('2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) == 1 * (si:hour)') :-
   eval(2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) == 1 * (si:hour)).

test('2 * m * (3 * m) == 6 * m2') :-
   eval(2 * (si:metre) * (3 * (si:metre)) == 6 * (si:metre)^2).

test('10 * km / (5 * km) == 2') :-
   eval(10 * (si:kilo(metre)) / (5 * (si:kilo(metre))) == 2).

test('1000 / (1 * s) == 1 * kHz') :-
   eval(1000 / (1 * (si:second)) == 1 * (si:kilo(hertz))).

test('1001 / (1 * s) == 1 * kHz', [fail]) :-
   eval(1001 / (1 * (si:second)) == 1 * (si:kilo(hertz))).

test('implicitly_convertible(width, length)') :-
   implicitly_convertible(width, length).

test('implicitly_convertible(radius, width)') :-
   implicitly_convertible(radius, width).

test('implicitly_convertible(radius, length)') :-
   implicitly_convertible(radius, length).

test('implicitly_convertible(length, width)', [fail]) :-
   implicitly_convertible(length, width).

test('implicitly_convertible(width, radius)', [fail]) :-
   implicitly_convertible(width, radius).

test('implicitly_convertible(length, radius)', [fail]) :-
   implicitly_convertible(length, radius).

test('implicitly_convertible(height, width)', [fail]) :-
   implicitly_convertible(height, width).

test('implicitly_convertible(time, length)', [fail]) :-
   implicitly_convertible(time, length).

test('implicitly_convertible(mass*length^2/time^2, energy)') :-
   implicitly_convertible(mass*length^2/time^2, energy).

test('implicitly_convertible(mass*height^2/time^2, energy)') :-
   implicitly_convertible(mass*height^2/time^2, energy).

test('implicitly_convertible(energy, mechanical_energy)', [fail]) :-
   implicitly_convertible(energy, mechanical_energy).

test('implicitly_convertible(mass*length^2/time^2, mechanical_energy)', [fail]) :-
   implicitly_convertible(mass*length^2/time^2, mechanical_energy).

% test('implicitly_convertible(energy, gravitational_potential_energy)', [fail]) :-
%    implicitly_convertible(energy, gravitational_potential_energy).
%
% test('implicitly_convertible(mass*length^2/time^2, gravitational_potential_energy)', [fail]) :-
%    implicitly_convertible(mass*length^2/time^2, gravitational_potential_energy).

test('implicitly_convertible(mass*speed^2, kinetic_energy)') :-
   implicitly_convertible(mass*speed^2, kinetic_energy).

test('implicitly_convertible(length, height)', [fail]) :-
   implicitly_convertible(length, height).

test('implicitly_convertible(kind(length), height)') :-
   implicitly_convertible(kind(length), height).

test('implicitly_convertible(kind(length)/kind(time), kind(length/time))') :-
   implicitly_convertible(kind(length)/kind(time), kind(length/time)).

test('implicitly_convertible(frequency, activity)', [fail]) :-
   implicitly_convertible(frequency, activity).

test('common_quantity(width, height, length)') :-
   common_quantity(width, height, Common),
   Common == length.

test('common_quantity(thickness, radius, width)') :-
   common_quantity(thickness, radius, Common),
   Common == width.

test('common_quantity(distance, path_length, path_length)') :-
   common_quantity(distance, path_length, Common),
   Common == path_length.

test('explicitly_convertible(length, width)') :-
   explicitly_convertible(length, width).

test('explicitly_convertible(width, radius)') :-
   explicitly_convertible(width, radius).
   
test('explicitly_convertible(length, radius)') :-
   explicitly_convertible(length, radius).

test('explicitly_convertible(height, width)', [fail]) :-
   explicitly_convertible(height, width).

test('explicitly_convertible(time, length)', [fail]) :-
   explicitly_convertible(time, length).

test('explicitly_convertible(energy, mechanical_energy)') :-
   explicitly_convertible(energy, mechanical_energy).

test('explicitly_convertible(mass*length^2/time^2, mechanical_energy)') :-
   explicitly_convertible(mass*length^2/time^2, mechanical_energy).

% test('explicitly_convertible(energy, gravitational_potential_energy)') :-
%    explicitly_convertible(energy, gravitational_potential_energy).

avg_speed(Distance, Time, Speed) :-
   S qis Distance / Time,
   Speed = S.as(speed).

test('avg_speed') :-
   avg_speed(220 * distance[si:kilo(metre)], 2 * si:hour, Speed),
   qmust_be(speed[si:kilo(metre)/si:hour], Speed).

:- end_tests(units).
