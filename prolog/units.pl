:- module(units, [qeval/1, qmust_be/2, eval_/2]).
:- reexport([units/units_utils]).

user:portray(Q) :-
   is_dict(Q, q),
   !,
   format("~p * ~p[~p]", [Q.v, Q.q, Q.u]).

:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).

:- use_module(units/units_utils).
:- use_module(units/q).
:- use_module(units/isq, []).
:- use_module(units/si, []).
:- use_module(units/international, []).


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
   { N is N1 * N2 },
   parse(A^N).
parse(A^N) ==>
   [A-N].
parse(A) ==>
   [A-1].

inverse(A-N) -->
   { N1 is -N },
   [A-N1].

aggregate(L, L2) :-
   group_pairs_by_key(L, Groups),
   maplist([A-Ns, A-N]>>sum_list(Ns, N), Groups, L1),
   simplify(L1, L2).

identity(_-0, _) => fail.
identity(1-_, _) => fail.
identity(A, R) => R = A.
simplify(L, L1) :-
   convlist(identity, L, L1).

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

normalize(In, Out) :-
   phrase(parse(In), L),
   normalize_factors(L, L1),
   generate_expression(L1, Out).

is_num(_-N) => N > 0.

power(A-1, Res) => Res = A.
power(A-N, Res) => Res = A^N.

generate_expression(In, Out) :-
   partition(is_num, In, Num, Denom),
   maplist(power, Num, Num1),
   phrase(sequence(inverse, Denom), Denom1),
   maplist(power, Denom1, Denom2),
   num_denom(Num1, Denom2, Out).

parse_normalize_factors(In, L3) :-
   phrase(parse(In), L),
   normalize_factors(L, L3).
normalize_factors(L, L2) :-
   msort(L, L1),
   aggregate(L1, L2).

:- table system_call/3.

system_call(Type, Goal, Module:Arg) :-
   system(Type, Module),
   call(Module:Goal, Arg).

:- table system_call/4.

system_call(Type, Goal, Module:Arg, Arg1) :-
   system(Type, Module),
   call(Module:Goal, Arg, Arg1).

:- table system_call/5.

system_call(Type, Goal, Module:Arg, Arg1, Arg2) :-
   system(Type, Module),
   call(Module:Goal, Arg, Arg1, Arg2).

quantity_call(Goal, Arg) :-
   system_call(quantity, Goal, Arg).
quantity_call(Goal, Arg, Arg1) :-
   system_call(quantity, Goal, Arg, Arg1).

unit_call(Goal, Arg) :-
   system_call(unit, Goal, Arg).
unit_call(Goal, Arg, Arg1) :-
   system_call(unit, Goal, Arg, Arg1).
unit_call(Goal, Arg, Arg1, Arg2) :-
   system_call(unit, Goal, Arg, Arg1, Arg2).

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
mapexpr(Goal, F, A^B, R) =>
   mapexpr(Goal, F, A, A1),
   R = A1^B.
% mapexpr(_, _, kind(A), R) =>
%    R = A.
mapexpr(Goal, Failure, A, A1) =>
   (  call(Goal, A, A1)
   *-> true
   ;  call(Failure, A, A1)
   ).

quantity_kind_(kind(A), R) =>
   R = A.
quantity_kind_(Kind, R) =>
   quantity_kind(Kind, R).
derived_quantity_kind(Quantity, Kind) :-
   mapexpr(quantity_kind_, Quantity, Kind).

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

implicitly_convertible__(A^2, A*A, _).
implicitly_convertible__(A/(B*C), A/B*1/C, _).
implicitly_convertible__(A*1/B, A/B, _).
% implicitly_convertible__(1/kind(A), kind(1/A), _).
% implicitly_convertible__(kind(A)/kind(B), kind(A/B), _).
% implicitly_convertible__(kind(A)*kind(B), kind(A*B), _).
implicitly_convertible__(From, To, _) :-
   \+ quantity_call(kind_, From),
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
   derived_kind_quantity(From, To).
implicitly_convertible__(From, To, _) :-
   quantity_call(alias, From, To).
implicitly_convertible__(From, To, Kinds) :-
   From =.. [Name | Args],
   Name \= :,
   select(Arg, Args, Arg1, Args1),
   implicitly_convertible__(Arg, Arg1, Kinds),
   To =.. [Name | Args1].

derived_kind_quantity(Kind, Quantity) :-
   mapexpr([K, Q]>>quantity_kind(Q, K), Kind, Quantity).

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

:- table common_quantity(_, _, po(best_common/2)).

best_common(Q1, Q2) :-
   implicitly_convertible(Q1, Q2),
   format("~p better than ~p~n", [Q1, Q2]).

common_quantity(Q1, Q2, NR) :-
   implicitly_convertible(Q1, Q),
   implicitly_convertible(Q2, Q),
   mapexpr(quantity_call(alias), Q, R),
   normalize(R, NR).

:- table unit_kind/2.

unit_kind(Unit, R) :-
   (  unit_call(unit_kind, Unit, Kind)
   *-> R = kind(Kind)
   ;  unit_call(unit, Unit, _, Formula),
      mapexpr(unit_kind, [_, 1]>>true, Formula, Kind),
      normalize(Kind, R)
   ).

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) :-
   parse_normalize_factors(Unit1, F1),
   parse_normalize_factors(Unit2, F2),
   once(iterative_deepening(1, F1, NewF1, F2, NewF2, NewUnits)),
   maplist(generate_expression, [NewF1, NewF2, NewUnits],
           [NewFactor1, NewFactor2, NewUnit]).

iterative_deepening(Limit, L1, R1, L2, R2, L) :-
   N = n(no),
   (  common_factors(L1, R1, Limit-N, L, L2, R2)
   -> true
   ;  (  N = n(depth_limit_exceeded)
      -> Limit1 is Limit + 1,
         iterative_deepening(Limit1, L1, R1, L2, R2, L)
      ;  fail
      )
   ).

is_unit(U-_) :-
   ground(U),
   unit_call(unit, U, _).

common_factors(L1, R1, N, L, L2, R2) :-
   partition(is_unit, L1, Unit1, Factor1),
   partition(is_unit, L2, Unit2, Factor2),
   ord_intersection(Unit1, Unit2, CommonUnits, Unit2Only),
   ord_subtract(Unit1, Unit2, Unit1Only),
   append(CommonUnits, R, L),
   append(Factor1, R11, R1),
   append(Factor2, R22, R2),
   expand_either_factors(Unit1Only, R11, N, R, Unit2Only, R22).
expand_either_factors([], [], _-N, [], [], []) :-
   setarg(1, N, no).
expand_either_factors([H1 | L1], R1, Limit-N, L, L2, R2) :-
   (  Limit > 0
   -> Limit1 is Limit - 1
   ;  nb_setarg(1, N, depth_limit_exceeded),
      fail
   ),
   (  phrase(select_factor([H1 | L1], R1, Limit1-N, L), L2, R2)
   ;  phrase(select_factor(L2, R2, Limit1-N, L), [H1 | L1], R1)
   ).
select_factor(L1, R1, N, L) -->
   select(A),
   expand_factors(A),
   normalize_factors,
   common_factors(L1, R1, N, L).

expand_factors(A), Factors -->
   { expand_factor(A, Factors) }.
expand_factor(Alias-N, Factors) :-
   unit_call(alias, Alias, Unit),
   parse_normalize_factors(Unit^N, Factors).
expand_factor(Unit-N, Factors) :-
   unit_call(unit, Unit, _, Formula),
   parse_normalize_factors(Formula^N, Factors).
   
comparable(AB, R) :-
   AB =.. [Op, A, B],
   eval_(A, A1),
   eval_(B, B1),
   (  common_quantity(A1.q, B1.q, Q)
   -> common_unit(A1.v*A1.u, AV, B1.v*B1.u, BV, U),
      V =.. [Op, AV, BV],
      R = q{v: V, u: U, q: Q}
   ;  domain_error(A1, B1)
   ).

normalize_kind(kind(A)/kind(B), R) =>
   normalize(A/B, AB),
   R = kind(AB).
normalize_kind(kind(A)*kind(B), R) =>
   normalize(A*B, AB),
   R = kind(AB).
normalize_kind(kind(A)^N, R) =>
   normalize(A^N, AN),
   R = kind(AN).
normalize_kind(kind(A)/B, R) =>
   normalize(A/B, R).
normalize_kind(A/kind(B), R) =>
   normalize(A/B, R).
normalize_kind(kind(A)*B, R) =>
   normalize(A*B, R).
normalize_kind(A*kind(B), R) =>
   normalize(A*B, R).
normalize_kind(A, R) =>
   normalize(A, R).

qeval(Expr) =>
   eval_(Expr, Q),
   call(Q.v).
eval_({ExprIn}, R) =>
   eval_(ExprIn, ExprOut),
   R = ExprOut.put(v, {ExprOut.v}).
eval_(Result is ExprIn, R) =>
   eval_(ExprIn, ExprOut),
   R = ExprOut.put(v, V is ExprOut.v),
   Result = ExprOut.put(v, V).
eval_(+A, R) =>
   eval_(A, A1),
   R = A1.put(v, +A1.v).
eval_(-A, R) =>
   eval_(A, A1),
   R = A1.put(v, -A1.v).
eval_(A+B, R) =>
   comparable(A+B, R).
eval_(A-B, R) =>
   comparable(A-B, R).
eval_(A==B, R) =>
   comparable(A==B, R).
eval_(A=:=B, R) =>
   comparable(A=:=B, R).
eval_(A=\=B, R) =>
   comparable(A=\=B, R).
eval_(A<B, R) =>
   comparable(A<B, R).
eval_(A>B, R) =>
   comparable(A>B, R).
eval_(A=<B, R) =>
   comparable(A=<B, R).
eval_(A>=B, R) =>
   comparable(A>=B, R).
eval_(A*B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   normalize_kind(A1.q*B1.q, Q),
   normalize(A1.u*B1.u, U),
   normalize(A1.v*B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(A/B, R) =>
   eval_(A, A1),
   eval_(B, B1),
   normalize_kind(A1.q/B1.q, Q),
   normalize(A1.u/B1.u, U),
   normalize(A1.v/B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(A^N, R) =>
   eval_(A, A1),
   normalize_kind(A1.q^N, Q),
   normalize(A1.u^N, U),
   normalize(A1.v^N, V),
   R = q{v: V, q: Q, u: U}.
eval_(in(Expr, Unit), R) =>
   eval_(Expr, M),
   eval_(Unit, Q),
   (  common_quantity(Q.q, M.q, _)
   -> common_unit(M.u, F1, Q.u, F2, _),
      normalize(M.v*F1/F2, V1),
      V is V1,
      R = q{v: V, q: M.q, u: Q.u}
   ;  domain_error(M, Unit)
   ).
eval_(as(Expr, Quantity), R) =>
   eval_(Expr, M),
   quantity_call(quantity, Quantity),
   (  explicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(cast(Expr, Quantity), R) =>
   eval_(Expr, M),
   (  common_quantity(M.q, Quantity, _)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(X, R), var(X) =>
   R = q{v: X, q: 1, u: 1}.
eval_(Module:Unit, R), unit_call(unit, Module:Unit, _) =>
   unit_kind(Module:Unit, Kind),
   R = q{v: 1, q: Kind, u: Module:Unit}.
eval_(Unit, R), unit_call(unit, Module:Unit, _) =>
   unit_kind(Module:Unit, Kind),
   R = q{v: 1, q: Kind, u: Module:Unit}.
eval_(Module:UnitSymbol, R), unit_call(unit, Module:Unit, UnitSymbol) =>
   ModuleUnit = Module:Unit,
   unit_kind(Module:Unit, Kind),
   R = q{v: 1, q: Kind, u: ModuleUnit}.
eval_(UnitSymbol, R), unit_call(unit, Unit, UnitSymbol) =>
   unit_kind(Unit, Kind),
   R = q{v: 1, q: Kind, u: Unit}.
eval_(QuantityExpr[UnitExpr], R) =>
   eval_(QuantityExpr, Quantity),
   eval_(UnitExpr, Unit),
   implicitly_convertible(Unit.q, Quantity.q),
   R = q{v: 1, q: Quantity.q, u: Unit.u}.
eval_(Quantity, R), quantity_call(quantity, Quantity) =>
   R = q{v: 1, q: Quantity, u: 1}.
eval_(kind(Quantity), R), quantity_call(quantity, Quantity) =>
   R = q{v: 1, q: kind(Quantity), u: 1}.
eval_(pi, R) =>
   R = q{v: pi, q: 1, u: 1}.
eval_(Q, R), is_dict(Q, q) =>
   R = Q.
eval_(N, R), number(N) =>
   R = q{v: N, q: 1, u: 1}.

:- table same_/3.

same_(unit, U-N, U-N) :-
   unit_call(unit, U, _).
same_(quantity, U-N, U-N) :-
   quantity_call(quantity, U).
same_(quantity, kind(U)-N, kind(U)-N) :-
   quantity_call(quantity, U).
same_(Type, U1-N, U2-N) :-
   system_call(Type, alias, U1, U2).
same_(Type, U1, U2) :-
   same_(Type, U2, U1).

same(Type, U1, U2) :-
   parse_normalize_factors(U1, F1),
   parse_normalize_factors(U2, F2),
   maplist(same_(Type), F1, F2).

qmust_be(A1/B1, A2/B2) =>
   qmust_be(A1, A2),
   qmust_be(B1, B2).
qmust_be(Quantity[Unit], Q2) =>
   eval_(Quantity[Unit], Q1),
   (  same(quantity, Q1.q, Q2.q)
   -> true
   ;  domain_error(Quantity[Unit], Q2)
   ),
   (  same(unit, Unit, Q2.u)
   -> true
   ;  domain_error(Unit, Q2.u)
   ).
qmust_be(Quantity, Q2), quantity_call(quantity, Quantity) =>
   (  same(quantity, Quantity, Q2.q)
   -> true
   ;  domain_error(Quantity, Q2.q)
   ).
qmust_be(Unit, Q2) =>
   eval_(Unit, Q1),
   (  same(unit, Q1.u, Q2.u)
   -> true
   ;  domain_error(Unit, Q2.u)
   ).

:- begin_tests(units).

qeval_data(si:metre =:= si:metre).
qeval_data(si:kilo(metre) =:= si:kilo(metre)).
qeval_data(si:kilogram =:= si:kilo(gram)).
qeval_data(si:kg =:= si:kilo(gram)).
qeval_data(10*(si:kilo(metre)) =:= 5*2*(si:kilo(metre))).
qeval_data(10*(si:kilo(metre)) / 2 =:= 5*(si:kilo(metre))).
qeval_data(1 * (si:hour) =:= 3600 * (si:second)).
qeval_data(1 * (si:kilo(metre)) + 1 * (si:metre) =:= 1001 * (si:metre)).
qeval_data(1 * (si:kilo(metre)) / (1 * (si:second)) =:= 1000 * (si:metre) / (si:second)).
qeval_data(2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) =:= 4 * (si:kilo(metre))).
qeval_data(2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) =:= 1 * (si:hour)).
qeval_data(2 * (si:metre) * (3 * (si:metre)) =:= 6 * (si:metre)^2).
qeval_data(10 * (si:kilo(metre)) / (5 * (si:kilo(metre))) =:= 2).
qeval_data(1000 / (1 * (si:second)) =:= 1 * (si:kilo(hertz))).
qeval_data(1001 / (1 * (si:second)) =\= 1 * (si:kilo(hertz))).
qeval_data(si:metre < si:kilo(metre)).
qeval_data(si:metre =< si:kilo(metre)).
qeval_data(si:metre > si:centi(metre)).
qeval_data(si:metre >= si:centi(metre)).

test('qeval', [forall(qeval_data(Expr))]) :-
   qeval(Expr).

fail_qeval_data(1001 / (1 * (si:second)) =:= 1 * (si:kilo(hertz))).

test('fail_qeval', [forall(fail_qeval_data(Expr)), fail]) :-
   qeval(Expr).

implicitly_convertible_data(isq:width, isq:length).
implicitly_convertible_data(isq:radius, isq:width).
implicitly_convertible_data(isq:radius, isq:length).
implicitly_convertible_data(isq:mass*isq:length^2/isq:time^2, isq:energy).
implicitly_convertible_data(isq:mass*isq:height^2/isq:time^2, isq:energy).
implicitly_convertible_data(isq:mass*isq:speed^2, isq:kinetic_energy).
implicitly_convertible_data(kind(isq:length), isq:height).
% implicitly_convertible_data(kind(isq:length)/kind(isq:time), kind(isq:length/isq:time)).
implicitly_convertible_data(isq:acceleration, isq:speed/isq:time).
implicitly_convertible_data(kind(isq:length/isq:time^2), isq:acceleration).
implicitly_convertible_data(kind(isq:length/isq:time^2), isq:velocity/isq:duration).

test('implicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   implicitly_convertible(Q1, Q2).

% not implicitly convertible that are explicitly convertible
explicitly_convertible_data(isq:length, isq:width).
explicitly_convertible_data(isq:width, isq:radius).
explicitly_convertible_data(isq:length, isq:radius).
explicitly_convertible_data(isq:energy, isq:mechanical_energy).
explicitly_convertible_data(isq:length, isq:height).
explicitly_convertible_data(isq:mass*isq:length^2/isq:time^2, isq:mechanical_energy).
explicitly_convertible_data(isq:angular_measure, 1).
explicitly_convertible_data(isq:speed/isq:time, isq:acceleration).
explicitly_convertible_data(isq:speed/isq:time, isq:acceleration).

test('not_implicitly_convertible', [forall(explicitly_convertible_data(Q1, Q2)), fail]) :-
   implicitly_convertible(Q1, Q2).

common_quantity_data(isq:width, isq:height, isq:length).
common_quantity_data(isq:thickness, isq:radius, isq:width).
common_quantity_data(isq:distance, isq:path_length, isq:path_length).
common_quantity_data(1, 1, 1).
common_quantity_data(kind(isq:length), kind(isq:length), kind(isq:length)).

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

avg_speed(Distance, Time, Speed) :-
   qeval(S is Distance / Time),
   Speed = S.as(isq:speed).

test('avg_speed') :-
   avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, Speed),
   qmust_be(isq:speed[si:kilo(metre)/si:hour], Speed).

test('in as') :-
   qeval(Speed is (m/s in inch/h) as isq:speed),
   qmust_be(isq:speed[international:inch/si:hour], Speed).

test('acceleration') :-
   qeval(Speed is 60 * isq:speed[km/h]),
   qeval(Duration is 8 * s),
   qeval(A is (Speed / Duration in m/s^2)),
   qmust_be((isq:speed/isq:time)[si:metre/si:second^2], A),
   qeval(Acceleration is A as isq:acceleration),
   qmust_be(isq:acceleration[si:metre/si:second^2], Acceleration).


:- end_tests(units).
