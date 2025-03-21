:- use_module(library(dcg/high_order)).
:- use_module(units_utils).
:- use_module(si, []).
:- use_module(isq, []).

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

:- table system_call/3.

system_call(Type, Goal, Module:Arg) :-
   system(Type, Module),
   call(Module:Goal, Arg).

:- table system_call/4.

system_call(Type, Goal, Module:Arg, Arg1) :-
   system(Type, Module),
   call(Module:Goal, Arg, Arg1).

quantity_call(Goal, Arg) :-
   system_call(quantity, Goal, Arg).
quantity_call(Goal, Arg, Arg1) :-
   system_call(quantity, Goal, Arg, Arg1).

unit_call(Goal, Arg) :-
   system_call(unit, Goal, Arg).
unit_call(Goal, Arg, Arg1) :-
   system_call(unit, Goal, Arg, Arg1).

derived_quantity(_*_) => true.
derived_quantity(_/_) => true.
derived_quantity(_^_) => true.
derived_quantity(_) => fail.

:- table root/1.

root(BaseQuantity) :-
   quantity_call(base_dimension, BaseQuantity, _).
root(Quantity) :-
   quantity_call(quantity_parent, Quantity, DerivedQuantity),
   derived_quantity(DerivedQuantity).

:- table kind/1.

kind(Root) :-
   quantity_call(kind, Root).
kind(Root) :-
   root(Root).

:- table quantity_kind/2.

quantity_kind(Kind, Kind) :-
   kind(Kind).
quantity_kind(Alias, Kind) :-
   quantity_call(alias, Alias, Kind),
   kind(Kind).
quantity_kind(Quantity, Kind) :-
   quantity_call(quantity_parent, Quantity, Parent),
   quantity_kind(Parent, Kind).

mapexpr(Goal, A*B, R) =>
   mapexpr(Goal, A, A1),
   mapexpr(Goal, B, B1),
   R = A1*B1.
mapexpr(Goal, A/B, R) =>
   mapexpr(Goal, A, A1),
   mapexpr(Goal, B, B1),
   R = A1/B1.
mapexpr(Goal, A^B, R) =>
   mapexpr(Goal, A, A1),
   R = A1^B.
mapexpr(_, kind(A), R) =>
   R = A.
mapexpr(Goal, A, A1) =>
   (  call(Goal, A, A1)
   *-> true
   ;  A1 = A
   ).
mapexpr(Goal, A*B) =>
   mapexpr(Goal, A),
   mapexpr(Goal, B).
mapexpr(Goal, A/B) =>
   mapexpr(Goal, A),
   mapexpr(Goal, B).
mapexpr(Goal, A^_) =>
   mapexpr(Goal, A).
mapexpr(Goal, A) =>
   (  call(Goal, A)
   *-> true
   ;  true
   ).

derived_quantity_kind(Quantity, Kind) :-
   mapexpr(quantity_kind, Quantity, Kind).

kind_ancestor(Kind, Ancestor) :-
   quantity_call(quantity_parent, Kind, Parent),
   derived_quantity_kind(Parent, Ancestor).

:- table kind_ancestors/2.

kind_ancestors(Kind, Kind).
kind_ancestors(Kind, Ancestor) :-
   kind_ancestor(Kind, Parent),
   kind_ancestors(Parent, Ancestor).

quantity_character(Quantity, Character) :-
   quantity_call(quantity_character, Quantity, Character).
quantity_character(AB, Character) :-
   subsumes_term(A*B, AB),
   AB = A*B,
   quantity_character(A, AC),
   quantity_character(B, BC),
   character_op(AC*BC, Character).
quantity_character(AB, Character) :-
   subsumes_term(A/B, AB),
   AB = A/B,
   quantity_character(A, AC),
   quantity_character(B, BC),
   character_op(AC/BC, Character).
quantity_character(Quantity, Character) :-
   quantity_call(quantity_formula, Quantity, Parent),
   quantity_character(Parent, Character).
quantity_character(Quantity, Character) :-
   \+ quantity_call(quantity_formula, Quantity, _),
   \+ quantity_call(quantity_character, Quantity, _),
   quantity_call(quantity_parent, Quantity, Parent),
   quantity_character(Parent, Character).

character_op(isq:real_scalar*isq:real_scalar, isq:real_scalar).
character_op(isq:real_scalar/isq:real_scalar, isq:real_scalar).
character_op(isq:real_scalar*isq:vector, isq:vector).
character_op(isq:vector*isq:real_scalar, isq:vector).
character_op(isq:real_scalar/isq:vector, isq:vector).
character_op(isq:vector/isq:real_scalar, isq:vector).
character_op(isq:real_scalar^_, isq:real_scalar).
character_op(isq:vector^_, isq:vector).

implicitly_convertible__(1/kind(A), kind(1/A), _).
implicitly_convertible__(kind(A)/kind(B), kind(A/B), _).
implicitly_convertible__(kind(A)*kind(B), kind(A*B), _).
implicitly_convertible__(From, To, _) :-
   quantity_call(quantity_parent, From, To).
implicitly_convertible__(From, ToKind, FromKind) :-
   quantity_call(quantity_parent, ToKind, From),
   kind(ToKind),
   % should be direct descendent or ascendent, no siblings
   (  kind_ancestors(ToKind, FromKind)
   ;  kind_ancestors(FromKind, ToKind)
   ).
implicitly_convertible__(From, To, _) :-
   quantity_call(quantity_formula, To, From).
implicitly_convertible__(kind(From), To, _) :-
   quantity_kind(To, From).
implicitly_convertible__(From, To, _) :-
   quantity_call(alias, To, From).
implicitly_convertible__(From, To, Kinds) :-
   From =.. [Name | Args],
   select(Arg, Args, Arg1, Args1),
   implicitly_convertible__(Arg, Arg1, Kinds),
   To =.. [Name | Args1].

:- table implicitly_convertible_/3.

implicitly_convertible_(Q, Q, _).
implicitly_convertible_(From, To, Kinds) :-
   implicitly_convertible__(From, New, Kinds),
   implicitly_convertible_(New, To, Kinds).

implicitly_convertible(From, To) :-
   derived_quantity_kind(From, Kind),
   implicitly_convertible_(From, To, Kind).

:- table explicitly_convertible/2.

explicitly_convertible(From, To) :-
   implicitly_convertible(From, To).
explicitly_convertible(From, To) :-
   implicitly_convertible(To, From).

:- table common_quantity(_, _, po(implicitly_convertible/2)).

common_quantity(Q1, Q2, R) :-
   implicitly_convertible(Q1, Q),
   implicitly_convertible(Q2, Q),
   mapexpr(quantity_call(alias), Q, R).

% unit(Unit, Symbol, Formula) :-
%    unit_(Unit, Symbol, Formula).
% unit(PrefixUnit, Symbol, Formula) :-
%    \+ compound(Symbol),
%    prefix(Prefix, PrefixSymbol, PrefixValue),
%    PrefixUnit =.. [Prefix, Unit],
%    unit_(Unit, UnitSymbol, _),
%    atom_concat(PrefixSymbol, UnitSymbol, Symbol),
%    Formula = PrefixValue * (si:Unit).
% unit(Alias, Symbol, Formula) :-
%    alias(Alias, Unit),
%    unit(Unit, Symbol, Formula).
%
% :- table unit_kind/2.
%
% unit_kind(Unit, Kind) :-
%    unit_kind_(Unit, Kind),
%    !.
% unit_kind(Unit, Kind) :-
%    (  unit(Unit, _, Formula)
%    ;  unit(_, Unit, Formula)
%    ),
%    formula_kind(Formula, Kind).
% unit_kind(N, 1) :-
%    number(N).
%
% formula_kind(A*B, R) =>
%    formula_kind(A, QA),
%    formula_kind(B, QB),
%    R = QA*QB.
% formula_kind(A/B, R) =>
%    formula_kind(A, QA),
%    formula_kind(B, QB),
%    R = QA/QB.
% formula_kind(A^N, R) =>
%    formula_kind(A, QA),
%    R = QA^N.
% formula_kind(System:Unit, Kind) =>
%    System:unit_kind(Unit, Kind).
% formula_kind(Unit, Kind) =>
%    si:unit_kind(Unit, Kind).
%
% :- table unit/1.
%
% unit(Unit) :-
%    unit(Unit, _, _).
% unit(Symbol) :-
%    unit(_, Symbol, _).
%
%
%
% common_unit(Unit1, NewUnit1, Unit2, NewUnit2) :-
%    parse_normalize_factors(Unit1, F1),
%    parse_normalize_factors(Unit2, F2),
%    once(common_factors(F1, NewF1, F2, NewF2)),
%    generate_expression(NewF1, NewUnit1),
%    generate_expression(NewF2, NewUnit2).
%
% is_number(N), number(N) => true.
% is_number(N^_), number(N) => true.
% is_number(_) => fail.
%
% common_factors(L1, R1, L2, R2) :-
%    partition(is_number, L1, N1, S1),
%    partition(is_number, L2, N2, S2),
%    ord_intersection(S1, S2, S, L22),
%    ord_subtract(S1, S2, L11),
%    append(N1, S, NS1),
%    append(NS1, R11, R1),
%    append(N2, S, NS2),
%    append(NS2, R22, R2),
%    common_factors_(L11, R11, L22, R22).
% common_factors_([], [], [], []).
% common_factors_([H1 | T1], R1, L2, R2) :-
%    (  common_factors__([H1 | T1], R1, L2, R2)
%    ;  common_factors__(L2, R2, [H1 | T1], R1)
%    ).
% common_factors__(L1, R1, L2, R2) :-
%    select(A, L1, L11),
%    expand_factors(A, L11, L111),
%    common_factors(L111, R1, L2, R2).
%
% expand_factors(A, L1, R1) :-
%    expand_factor(A, Formula),
%    parse_normalize_factors(Formula, Factors),
%    append(Factors, L1, FactorsL1),
%    normalize_factors(FactorsL1, R1).
% expand_factor(Unit^N, R) =>
%    expand_factor(Unit, Formula),
%    R = Formula^N.
% expand_factor(System:Unit, R), System:unit(Unit, _, Formula), Formula \== System:Unit =>
%    R = Formula.
% expand_factor(System:Symbol, R), System:unit(Unit, Symbol, _) =>
%    R = System:Unit.
% expand_factor(_, _) => fail.
%
%
%
% eval(Expr) :-
%    eval_(Expr, R),
%    call(R.v).
% qis(Result, ExprIn) :-
%    eval_(ExprIn, ExprOut),
%    V is ExprOut.v,
%    Result = q{v: V, q: ExprOut.q, u: ExprOut.u}.
% eval_(A == B, R) =>
%    eval_(A, A1),
%    eval_(B, B1),
%    (  common_quantity(A1.q, B1.q, Q)
%    -> common_unit(A1.u, U1, B1.u, U2),
%       eval_(A1.v * U1, A2),
%       eval_(B1.v * U2, B2),
%       V = (A2.v =:= B2.v),
%       R = q{v: V, q: Q, u: A2.u}
%    ;  domain_error(A1, B1)
%    ).
% eval_(A*B, R) =>
%    eval_(A, A1),
%    eval_(B, B1),
%    normalize(A1.q*B1.q, Q),
%    normalize(A1.u*B1.u, U),
%    normalize(A1.v*B1.v, V),
%    R = q{v: V, q: Q, u: U}.
% eval_(A/B, R) =>
%    eval_(A, A1),
%    eval_(B, B1),
%    normalize(A1.q/B1.q, Q),
%    normalize(A1.u/B1.u, U),
%    normalize(A1.v/B1.v, V),
%    R = q{v: V, q: Q, u: U}.
% eval_(A+B, R) =>
%    eval_(A, A1),
%    eval_(B, B1),
%    (  common_quantity(A1.q, B1.q, Q)
%    -> common_unit(A1.u, U1, B1.u, U2),
%       eval_(A1.v * U1, A2),
%       eval_(B1.v * U2, B2),
%       normalize(Q, Q1),
%       normalize(U2, U),
%       R = q{v: A2.v+B2.v, u: U, q: Q1}
%    ;  domain_error(A1, B1)
%    ).
% eval_(A^N, R) =>
%    eval_(A, A1),
%    normalize(A1.q^N, Q),
%    normalize(A1.u^N, U),
%    normalize(A1.v^N, V),
%    R = q{v: V, q: Q, u: U}.
% eval_(System:Unit, R), System:unit(Unit) =>
%    System:unit_kind(Unit, Kind),
%    normalize(Kind, Kind1),
%    R = q{v: 1, q: kind(Kind1), u: System:Unit}.
% eval_(Number, R), number(Number) =>
%    R = q{v: Number, q: 1, u: 1}.
% eval_(Quantity[Unit], R), isq:quantity_parent(Quantity, _) =>
%    eval_(Unit, Sub),
%    implicitly_convertible(Sub.q, Quantity),
%    R = q{v: 1, q: Quantity, u: Sub.u}.
% eval_(Quantity, R), is_dict(Quantity, q) =>
%    R = Quantity.
%
% M.in(Unit) := R :-
%    eval_(Unit, Q),
%    (  implicitly_convertible(Q.q, M.q)
%    -> common_unit(M.u, U1, Q.u, U2),
%       eval_(U1/U2, ConversionFactor),
%       normalize(M.v*ConversionFactor.v, V),
%       R = q{v: V, q: M.q, u: Q.u}
%    ;  domain_error(M, Unit)
%    ).
%
% M.as(Quantity) := R :-
%    (  explicitly_convertible(M.q, Quantity)
%    -> R = M.put(q, Quantity)
%    ;  domain_error(M.q, Quantity)
%    ).
%
% :- table same_unit/2.
%
% same_unit(U, U).
% same_unit(System:U1, System:U2) :-
%    System:alias(U1, U2).
% same_unit(System:U1, System:U2) :-
%    System:unit(U1, U2, _).
% same_unit(U1, U2) :-
%    same_unit(U2, U1).
%
% same_units(U1, U2) :-
%    parse_normalize_factors(U1, F1),
%    parse_normalize_factors(U2, F2),
%    maplist(same_unit, F1, F2).
%
% qmust_be(Quantity[Unit], Q) =>
%    qmust_be(Quantity, Q),
%    (  same_units(Unit, Q.u)
%    -> true
%    ;  domain_error(Unit, Q.u)
%    ).
% qmust_be(Quantity, Q) =>
%    (  implicitly_convertible(Q.q, Quantity)
%    -> true
%    ;  domain_error(Quantity, Q.q)
%    ).
%
:- begin_tests(units).
%
% test('si:metre == si:metre') :-
%    eval(si:metre == si:metre).
%
% test('si:kilo(metre) == si:kilo(metre)') :-
%    eval(si:kilo(metre) == si:kilo(metre)).
%
% test('si:kilogram == si:kilo(gram)') :-
%    eval(si:kilogram == si:kilo(gram)).
%
% test('si:kg == si:kilo(gram)') :-
%    eval(si:kg == si:kilo(gram)).
%
% test('10*(si:kilo(metre)) == 5*2*(si:kilo(metre))') :-
%    eval(10*(si:kilo(metre)) == 5*2*(si:kilo(metre))).
%
% test('10*(si:kilo(metre)) / 2 == 5*(si:kilo(metre))') :-
%    eval(10*(si:kilo(metre)) / 2 == 5*(si:kilo(metre))).
%
% test('1 * (si:hour) == 3600 * (si:second)') :-
%    eval(1 * (si:hour) == 3600 * (si:second)).
%
% test('1 * (si:kilo(metre)) + 1 * (si:metre) == 1001 * (si:metre)') :-
%    eval(1 * (si:kilo(metre)) + 1 * (si:metre) == 1001 * (si:metre)).
%
% test('1 * (si:kilo(metre)) / (1 * (si:second)) == 1000 * (si:metre) / (si:second)') :-
%    eval(1 * (si:kilo(metre)) / (1 * (si:second)) == 1000 * (si:metre) / (si:second)).
%
% test('2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) == 4 * (si:kilo(metre))') :-
%    eval(2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) == 4 * (si:kilo(metre))).
%
% test('2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) == 1 * (si:hour)') :-
%    eval(2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) == 1 * (si:hour)).
%
% test('2 * m * (3 * m) == 6 * m2') :-
%    eval(2 * (si:metre) * (3 * (si:metre)) == 6 * (si:metre)^2).
%
% test('10 * km / (5 * km) == 2') :-
%    eval(10 * (si:kilo(metre)) / (5 * (si:kilo(metre))) == 2).
%
% test('1000 / (1 * s) == 1 * kHz') :-
%    eval(1000 / (1 * (si:second)) == 1 * (si:kilo(hertz))).
%
% test('1001 / (1 * s) == 1 * kHz', [fail]) :-
%    eval(1001 / (1 * (si:second)) == 1 * (si:kilo(hertz))).

implicitly_convertible_data(isq:width, isq:length).
implicitly_convertible_data(isq:radius, isq:width).
implicitly_convertible_data(isq:radius, isq:length).
implicitly_convertible_data(isq:mass*isq:length^2/isq:time^2, isq:energy).
implicitly_convertible_data(isq:mass*isq:height^2/isq:time^2, isq:energy).
implicitly_convertible_data(isq:mass*isq:speed^2, isq:kinetic_energy).
implicitly_convertible_data(kind(isq:length), isq:height).
implicitly_convertible_data(kind(isq:length)/kind(isq:time), kind(isq:length/isq:time)).

test('implicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   implicitly_convertible(Q1, Q2).

% not implicitly convertible that are explicitly convertible
explicitly_convertible_data(isq:length, isq:width).
explicitly_convertible_data(isq:width, isq:radius).
explicitly_convertible_data(isq:length, isq:radius).
explicitly_convertible_data(isq:energy, isq:mechanical_energy).
explicitly_convertible_data(isq:length, isq:height).
explicitly_convertible_data(isq:mass*isq:length^2/isq:time^2, isq:mechanical_energy).

test('not_implicitly_convertible', [forall(explicitly_convertible_data(Q1, Q2)), fail]) :-
   implicitly_convertible(Q1, Q2).

common_quantity_data(isq:width, isq:height, isq:length).
common_quantity_data(isq:thickness, isq:radius, isq:width).
common_quantity_data(isq:distance, isq:path_length, isq:path_length).

test('common_quantity', [forall(common_quantity_data(Q1, Q2, Q))]) :-
   common_quantity(Q1, Q2, Q3),
   Q3 == Q.

test('explicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   explicitly_convertible(Q1, Q2).

test('explicitly_convertible', [forall(explicitly_convertible_data(Q1, Q2))]) :-
   explicitly_convertible(Q1, Q2).

not_explicitly_convertible_data(isq:height, isq:width).
not_explicitly_convertible_data(isq:time, isq:length).
not_explicitly_convertible_data(isq:frequency, isq:activity).
not_explicitly_convertible_data(isq:mass*isq:height^2/isq:time^2, isq:mechanical_energy).

test('not_explicitly_convertible', [forall(not_explicitly_convertible_data(Q1, Q2)), fail]) :-
   explicitly_convertible(Q1, Q2).

% avg_speed(Distance, Time, Speed) :-
%    S qis Distance / Time,
%    Speed = S.as(speed).
%
% test('avg_speed') :-
%    avg_speed(220 * distance[si:kilo(metre)], 2 * si:hour, Speed),
%    qmust_be(speed[si:kilo(metre)/si:hour], Speed).
%
:- end_tests(units).
