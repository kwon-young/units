:- module(units, [
   op(600, xfx, as),
   op(600, xfx, in),
   op(100, yf,  []),
   op(99, xfy, :),
   quantity_dimension/2,
   quantity_parent/2,
   quantity_formula/2,
   alias_quantity/2,
   kind/1,
   unit_symbol/2,
   unit_symbol_formula/3,
   unit_kind/2,
   prefix_unit_symbol/2,
   qeval/1
]).
:- reexport([units/q]).

:- multifile quantity_dimension/2.
:- multifile quantity_parent/2.
:- multifile quantity_formula/2.
:- multifile alias_quantity/2.
:- multifile kind/1.
:- multifile unit_symbol/2.
:- multifile unit_symbol_formula/3.
:- multifile unit_kind/2.
:- multifile prefix_unit_symbol/2.

:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(units/q).
:- use_module(units/isq).
:- use_module(units/si).
:- use_module(units/international).

user:portray(Q) :-
   is_dict(Q, q),
   !,
   format("~p * ~p[~p]", [Q.v, Q.q, Q.u]).

parse(A*B) ==>
   parse(A), parse(B).
parse(1/B) ==>
   { phrase(parse(B), L) },
   sequence(inverse, L).
parse(A/B) ==>
   parse(A),
   { phrase(parse(B), L) },
   sequence(inverse, L).
parse((A*B)**N) ==>
   parse(A**N*B**N).
parse((A/B)**N) ==>
   parse(A**N/B**N).
parse((A**N1)**N2) ==>
   { N is N1 * N2 },
   parse(A**N).
parse(A**N) ==>
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

identity(_-0) => true.
identity(1-_) => true.
identity(_) => fail.
not_identity(A, A) :-
   \+ identity(A).
simplify(L, L1) :-
   convlist(not_identity, L, L1).

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
power(A-N, Res) => Res = A**N.

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

:- meta_predicate rewriteexpr(2, ?, ?).

rewriteexpr(G, A, A1) :-
   call(G, A, A1).
rewriteexpr(G, A, A1) :-
   A =.. [Name | Args],
   select(B, Args, B1, Args1),
   rewriteexpr(G, B, B1),
   A1 =.. [Name | Args1].

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

common_expr(Type, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) :-
   parse_normalize_factors(Unit1, F1),
   parse_normalize_factors(Unit2, F2),
   once(iterative_deepening(1,
      {F1, NewF1, Type, NewUnits, F2, NewF2}/[N]>>common_factors(
         F1, NewF1, Type, NewUnits, N, F2, NewF2))),
   msort(NewUnits, SortedNewUnits),
   maplist(generate_expression, [NewF1, NewF2, SortedNewUnits],
           [NewFactor1, NewFactor2, NewUnit]).

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

is_of(unit, U-_) :-
   ground(U),
   unit(U, _).
is_of(quantity, Q-_) :-
   ground(Q),
   alias_or_quantity(Q).

common_factors(L1, R1, Type, L, N, L2, R2) :-
   partition(is_of(Type), L1, Unit1, Factor1),
   partition(is_of(Type), L2, Unit2, Factor2),
   ord_intersection(Unit1, Unit2, CommonUnits, Unit2Only),
   ord_subtract(Unit1, Unit2, Unit1Only),
   append(CommonUnits, R, L),
   append(Factor1, R11, R1),
   append(Factor2, R22, R2),
   expand_either_factors(Unit1Only, R11, Type, R, N, Unit2Only, R22).
expand_either_factors([], [], _, [], _-N, [], []) :-
   setarg(1, N, no).
expand_either_factors(L1, R1, Type, L, Limit-N, L2, R2) :-
   (  Limit > 0
   -> Limit1 is Limit - 1
   ;  nb_setarg(1, N, depth_limit_exceeded),
      fail
   ),
   (  phrase(select_factor(L1, R1, Type, L, Limit1-N), L2, R2)
   ;  phrase(select_factor(L2, R2, Type, L, Limit1-N), L1, R1)
   ).
select_factor(L1, R1, Type, L, N) -->
   select(A),
   expand_factors(Type, A),
   normalize_factors,
   common_factors(L1, R1, Type, L, N).

expand_factors(Type, A), Factors -->
   { expand_factor(Type, A, Factors) }.
expand_factor(Type, Unit-N, Factors) :-
   (  Type == unit
   -> unit(Unit, _, Formula)
   ;  Type == quantity,
      (  alias_quantity(Unit, Formula)
      ;  quantity_parent(Unit, Formula)
      )
   ),
   parse_normalize_factors(Formula**N, Factors).

:- table alias_quantity_dimension/2.

alias_quantity_dimension(Quantity, Symbol) :-
   quantity_dimension(Quantity, Symbol).
alias_quantity_dimension(Alias, Symbol) :-
   alias_quantity(Alias, Quantity),
   alias_quantity_dimension(Quantity, Symbol).

:- table alias_quantity_parent/2.

alias_quantity_parent(Quantity, Parent) :-
   quantity_parent(Quantity, Parent).
alias_quantity_parent(Alias, Parent) :-
   alias_quantity(Alias, Quantity),
   alias_quantity_parent(Quantity, Parent).

:- table alias_or_quantity/1.

alias_or_quantity(Quantity) :-
   alias_quantity_dimension(Quantity, _).
alias_or_quantity(Quantity) :-
   alias_quantity_parent(Quantity, _).

:- table alias_quantity_formula/2.

alias_quantity_formula(Quantity, Formula) :-
   quantity_formula(Quantity, Formula).
alias_quantity_formula(Alias, Formula) :-
   alias_quantity(Alias, Quantity),
   alias_quantity_formula(Quantity, Formula).

derived_quantity(_*_).
derived_quantity(_/_).
derived_quantity(_**_).

:- table root/1.

root(BaseQuantity) :-
   quantity_dimension(BaseQuantity, _).
root(Quantity) :-
   quantity_parent(Quantity, DerivedQuantity),
   derived_quantity(DerivedQuantity).

:- table root_kind/1.

root_kind(Kind) :-
   kind(Kind).
root_kind(Root) :-
   root(Root).

:- table derived_root_kind/1.

derived_root_kind(Kind) :-
   mapexpr([X, X]>>root_kind(X), [X, _]>>domain_error(root_kind, X), Kind, Kind).

:- table quantity_kind/2.

quantity_kind(kind_of(Kind), Kind).
quantity_kind(Kind, Kind) :-
   root_kind(Kind).
quantity_kind(Quantity, Kind) :-
   alias_or_quantity_parent(Quantity, Parent),
   quantity_kind(Parent, Kind).

derived_quantity_kind(Quantity, Kind) :-
   mapexpr(quantity_kind, [_, 1]>>true, Quantity, Kind).

alias_or_quantity_parent(Q, Q1) :-
   (  alias_quantity_parent(Q, Q1)
   ;  alias_quantity(Q, Q1)
   ).

simplify_kind(Q, R) :-
   phrase(parse(Q), L),
   msort(L, L1),
   group_pairs_by_key(L1, Groups),
   maplist([A-Ns, A-N]>>sum_list(Ns, N), Groups, L2),
   partition(identity, L2, [_|_], L3),
   % L3 = [_|_],
   generate_expression(L3, R).

rewrite_kind(Q, R) :-
   rewriteexpr(alias_or_quantity_parent, Q, Q1),
   simplify_kind(Q1, Q2),
   !,
   (  rewrite_kind(Q2, R)
   ;  Q2 = R
   ).
rewrite_kind(Q, R) :-
   rewriteexpr(alias_or_quantity_parent, Q, Q1),
   rewrite_kind(Q1, R).
optional_rewrite_kind(Q, R) :-
   (  rewrite_kind(Q, K)
   -> derived_quantity_kind(K, R)
   ;  Q = R
   ).

common_quantity(kind_of(Q1), kind_of(Q2), Q) =>
   optional_rewrite_kind(Q1, K1),
   optional_rewrite_kind(Q2, K2),
   common_quantity(K1, K2, Q3),
   (  K1 == Q3 % Q1 is the least specific one
   -> Q = kind_of(K2) % the common kind_of should be the most specific one
   ;  K2 == Q3
   -> Q = kind_of(K1)
   ;  Q = Q3 % Q1 an Q2 are not directly related, their common ancestor is not a kind_of
   ).
   % (  (Q1 == Q3 ; Q2 == Q3)
   % -> Q = kind_of(Q3)
   % ;  Q = Q3
   % ).
common_quantity(kind_of(Q1), Q2, Q) =>
   optional_rewrite_kind(Q1, K1),
   common_quantity(K1, Q2, Q3),
   (  K1 == Q3 % Q1 is more generic than Q2
   -> Q = Q2 % common quantity should by Q2
   ;  Q = Q3
   ).
common_quantity(Q1, kind_of(Q2), Q) =>
   common_quantity(kind_of(Q2), Q1, Q).
common_quantity(Q1, Q2, Q) =>
   common_expr(quantity, Q1, 1, Q2, 1, Q).

same_kind(Q1, Q2) :-
   derived_quantity_kind(Q1, K1),
   derived_quantity_kind(Q2, K2),
   common_quantity(K1, K2, K),
   (  (K1 == K ; K2 == K)
   -> true
   ).

%  From is implicitly convertible to To if:
%  
%  * From is a direct descendent of To: i.e. common_quantity(From, To, To)
%  * the path from From to To does not cross a kind.
%
%  Exceptions:
%
%  * if To is a kind_of and From is not a kind_of, then common_quantity(From, To, From)
%
%
implicitly_convertible(From, To, Explicit) :-
   normalize(To, NormalizedTo),
   mapexpr(alias_quantity, NormalizedTo, AliasNormalizedTo),
   common_quantity(From, AliasNormalizedTo, CommonQuantity),
   (  \+ subsumes_term(kind_of(_), From), AliasNormalizedTo = kind_of(_)
   -> CommonQuantity = From
   ;  CommonQuantity = AliasNormalizedTo
   ),
   (  Explicit == false, quantity_kind(From, FromKind), kind(FromKind)
   -> common_quantity(FromKind, AliasNormalizedTo, FromKind)
   ;  true
   ),
   !.
implicitly_convertible(From, ToKind, Explicit) :-
   root_kind(ToKind),
   alias_quantity_parent(ToKind, Formula),
   implicitly_convertible(From, Formula, Explicit),
   derived_quantity_kind(From, FromKind),
   normalize(FromKind, NormalizedFromKind),
   common_quantity(NormalizedFromKind, ToKind, CommonKind),
   (  (CommonKind == NormalizedFromKind ; CommonKind == ToKind)
   -> true
   ),
   !.
implicitly_convertible(From, To, _) :-
   alias_quantity_formula(To, Formula),
   implicitly_convertible(From, Formula).

implicitly_convertible(From, To) :-
   implicitly_convertible(From, To, false).

:- table explicitly_convertible/2.

explicitly_convertible(From, To) :-
   implicitly_convertible(From, To, true).
explicitly_convertible(From, To) :-
   implicitly_convertible(To, From, true).

normalize_unit(Unit, R), unit(Unit, _) =>
   R = Unit.
normalize_unit(Symbol, R), unit(Unit, Symbol) =>
   R = Unit.
normalize_unit(Unit, R), unit(Module:Unit, _) =>
   R = Module:Unit.
normalize_unit(Module:Symbol, R), unit(Module:Unit, Symbol) =>
   R = Module:Unit.
normalize_unit(Module:PrefixUnit, R),
      PrefixUnit =.. [Prefix, Unit],
      prefix(Module:Prefix, _, _) =>
   normalize_unit(Unit, R1),
   R2 =.. [Prefix, R1],
   R = Module:R2.
normalize_unit(PrefixUnit, R),
      PrefixUnit =.. [Prefix, Unit],
      prefix(Module:Prefix, _, _) =>
   normalize_unit(Unit, R1),
   R2 =.. [Prefix, R1],
   R = Module:R2.
normalize_unit(_, _) => fail.

:- table all_unit_symbol/2.

all_unit_symbol(Unit, Symbol) :-
   (  unit_symbol(Unit, Symbol)
   ;  unit_symbol_formula(Unit, Symbol, _)
   ).

:- table unit/3.

unit(U, S, F) :-
   unit_symbol_formula(U, S, F).
unit(Module:PrefixUnit, Symbol, PrefixFormula*Unit) :-
   \+ compound(Symbol),
   \+ prefix_unit_symbol(Module:PrefixUnit, Symbol),
   prefix(Module:Prefix, PrefixSymbol, PrefixFormula),
   PrefixUnit =.. [Prefix, Unit],
   all_unit_symbol(Unit, UnitSymbol),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol).

:- table unit/2.

unit(U, S) :-
   (  unit_symbol(U, S)
   ;  unit(U, S, _)
   ).

:- table all_unit_kind/2.

all_unit_kind(Unit, kind_of(Kind)) :-
   unit_kind(Unit, Kind),
   !.
all_unit_kind(Unit, R) :-
   unit(Unit, _, Formula),
   mapexpr(all_unit_kind, [_, 1]>>true, Formula, Kind),
   normalize(Kind, NKind),
   normalize_kind(NKind, R).

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) :-
   common_expr(unit, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

comparable(AB, R) :-
   AB =.. [Op, A, B],
   eval_(A, A1),
   eval_(B, B1),
   (  same_kind(A1.q, B1.q), common_quantity(A1.q, B1.q, Q)
   -> (  common_unit(A1.v*A1.u, AV, B1.v*B1.u, BV, U)
      -> V =.. [Op, AV, BV],
         R = q{v: V, u: U, q: Q}
      ;  domain_error(A1.u, B1.u)
      )
   ;  domain_error(A1, B1)
   ).

normalize_kind(kind_of(A)/kind_of(B), R) =>
   normalize(A/B, AB),
   R = kind_of(AB).
normalize_kind(kind_of(A)*kind_of(B), R) =>
   normalize(A*B, AB),
   R = kind_of(AB).
normalize_kind(kind_of(A)**N, R) =>
   normalize(A**N, AN),
   R = kind_of(AN).
normalize_kind(kind_of(A)/1, R) =>
   R = kind_of(A).
normalize_kind(1/kind_of(A), R) =>
   normalize(1/A, AN),
   R = kind_of(AN).
normalize_kind(kind_of(A)*1, R) =>
   R = kind_of(A).
normalize_kind(1*kind_of(A), R) =>
   R = kind_of(A).
normalize_kind(kind_of(A)/B, R) =>
   normalize(A/B, R).
normalize_kind(A/kind_of(B), R) =>
   normalize(A/B, R).
normalize_kind(kind_of(A)*B, R) =>
   normalize(A*B, R).
normalize_kind(A*kind_of(B), R) =>
   normalize(A*B, R).
normalize_kind(A, R) =>
   normalize(A, R).

qeval((A, B)) =>
   qeval(A),
   qeval(B).
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
eval_(A**N, R) =>
   eval_(A, A1),
   normalize_kind(A1.q**N, Q),
   normalize(A1.u**N, U),
   normalize(A1.v**N, V),
   R = q{v: V, q: Q, u: U}.
eval_(A^N, R) =>
   eval_(A**N, R).
eval_(in(Expr, Unit), R) =>
   eval_(Expr, M),
   eval_(Unit, Q),
   (  implicitly_convertible(M.q, Q.q)
   -> common_unit(M.u, F1, Q.u, F2, _),
      normalize(M.v*F1/F2, V1),
      V is V1,
      R = q{v: V, q: M.q, u: Q.u}
   ;  domain_error(M.q, Q.q)
   ).
eval_(as(Expr, Quantity), R), alias_or_quantity(Quantity) =>
   eval_(Expr, M),
   (  implicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(force_as(Expr, Quantity), R), alias_or_quantity(Quantity) =>
   eval_(Expr, M),
   (  explicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(cast(Expr, Quantity), R), alias_or_quantity(Quantity) =>
   eval_(Expr, M),
   (  common_quantity(M.q, Quantity, _)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(X, R), var(X) =>
   R = q{v: X, q: 1, u: 1}.
eval_(UnitOrSymbol, R), normalize_unit(UnitOrSymbol, Unit) =>
   all_unit_kind(Unit, Kind),
   R = q{v: 1, q: Kind, u: Unit}.
eval_(QuantityExpr[UnitExpr], R) =>
   eval_(QuantityExpr, R),
   eval_(UnitExpr, Unit),
   (  implicitly_convertible(Unit.q, R.q)
   -> true
   ;  domain_error(Unit.q, R.q)
   ),
   R.v = Unit.v,
   R.u = Unit.u.
eval_(N, R), number(N) =>
   R = q{v: N, q: 1, u: 1}.
eval_(Quantity, R), alias_or_quantity(Quantity) =>
   R = q{v: _, q: Quantity, u: _}.
eval_(kind_of(Kind), R), derived_root_kind(Kind) =>
   R = q{v: _, q: kind_of(Kind), u: _}.
eval_(pi, R) =>
   R = q{v: pi, q: 1, u: 1}.
eval_(Q, R), is_dict(Q, q) =>
   R = Q.

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
qeval_data(2 * (si:metre) * (3 * (si:metre)) =:= 6 * (si:metre)**2).
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

error_qeval_data(si:hertz =:= si:becquerel).
error_qeval_data(_ is si:hertz + si:becquerel).

test('error_qeval', [forall(error_qeval_data(Expr)), error(domain_error(_, _))]) :-
   qeval(Expr).

implicitly_convertible_data(isq:width, isq:length).
implicitly_convertible_data(isq:radius, isq:width).
implicitly_convertible_data(isq:radius, isq:length).
implicitly_convertible_data(isq:mass*isq:length**2/isq:time**2, isq:energy).
implicitly_convertible_data(isq:mass*isq:height**2/isq:time**2, isq:energy).
implicitly_convertible_data(isq:height**2*isq:mass/isq:time**2, isq:energy).
implicitly_convertible_data(isq:mass*isq:speed**2, isq:kinetic_energy).
implicitly_convertible_data(kind_of(isq:length), isq:height).
implicitly_convertible_data(isq:acceleration, isq:speed/isq:time).
implicitly_convertible_data(kind_of(isq:length/isq:time**2), isq:acceleration).
implicitly_convertible_data(kind_of(isq:length/isq:time**2), isq:velocity/isq:duration).
implicitly_convertible_data(kind_of(isq:time*isq:frequency), isq:rotation).
implicitly_convertible_data(kind_of(isq:time*isq:frequency), kind_of(isq:rotation)).

test('implicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   implicitly_convertible(Q1, Q2).

% not implicitly convertible that are explicitly convertible
explicitly_convertible_data(isq:length, isq:width).
explicitly_convertible_data(isq:width, isq:radius).
explicitly_convertible_data(isq:length, isq:radius).
explicitly_convertible_data(isq:energy, isq:mechanical_energy).
explicitly_convertible_data(isq:length, isq:height).
explicitly_convertible_data(isq:mass*isq:length**2/isq:time**2, isq:mechanical_energy).
explicitly_convertible_data(isq:angular_measure, 1).
explicitly_convertible_data(isq:speed/isq:time, isq:acceleration).

not_implicitly_convertible_data(isq:time*isq:frequency, isq:rotation).

test('not_implicitly_convertible(explicit_data)', [forall(explicitly_convertible_data(Q1, Q2)), fail]) :-
   implicitly_convertible(Q1, Q2).
test('not_implicitly_convertible', [forall(not_implicitly_convertible_data(Q1, Q2)), fail]) :-
   implicitly_convertible(Q1, Q2).

common_quantity_data(isq:width, isq:height, isq:length).
common_quantity_data(isq:thickness, isq:radius, isq:width).
common_quantity_data(isq:distance, isq:path_length, isq:path_length).
common_quantity_data(1, 1, 1).
common_quantity_data(1, isq:rotation, 1).
common_quantity_data(kind_of(isq:length), kind_of(isq:length), kind_of(isq:length)).
common_quantity_data(isq:width, kind_of(isq:length), isq:width).

test('common_quantity', [forall(common_quantity_data(Q1, Q2, Q))]) :-
   common_quantity(Q1, Q2, Q).

test('explicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   explicitly_convertible(Q1, Q2).

test('explicitly_convertible', [forall(explicitly_convertible_data(Q1, Q2))]) :-
   explicitly_convertible(Q1, Q2).

not_explicitly_convertible_data(isq:height, isq:width).
not_explicitly_convertible_data(isq:time, isq:length).
not_explicitly_convertible_data(isq:frequency, isq:activity).
not_explicitly_convertible_data(kind_of(isq:frequency), kind_of(isq:activity)).
not_explicitly_convertible_data(isq:mass*isq:height**2/isq:time**2, isq:mechanical_energy).

test('not_explicitly_convertible', [forall(not_explicitly_convertible_data(Q1, Q2)), fail]) :-
   explicitly_convertible(Q1, Q2).

avg_speed(Distance, Time, Speed) :-
   qeval(Speed is Distance / Time as isq:speed).

test('avg_speed') :-
   avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, _Speed).

test('in as') :-
   qeval(_Speed is (m/s in inch/h) as isq:speed).

as_data(_ is isq:width[m] as isq:length).
as_data(_ is isq:width[m] / isq:time[s] as isq:speed).

test('as', [forall(as_data(Expr))]) :-
   qeval(Expr).

error_as_data(_ is isq:length[m] as isq:width).

test('error_as', [forall(error_as_data(Expr)), error(domain_error(_, _))]) :-
   qeval(Expr).

test('error_in', [error(domain_error(_, _))]) :-
   qeval(_ is si:hertz in si:becquerel).

test('acceleration') :-
   qeval(Speed is 60 * isq:velocity[km/h]),
   qeval(Duration is 8 * s),
   qeval(A is (Speed / Duration) as isq:acceleration),
   qeval(B is A in m/s**2),
   B = q{q: isq:acceleration, u: si:metre/si:second**2, v: _}.

test('clpBNR') :-
   qeval({A * inch =:= 1 * metre}),
   A == 5000r127,
   qeval({B =:= 5000 * gram / (2*gram)}),
   B == 2500,
   qeval({C is 1^2}),
   C == q{q:1, u:1, v:1}.

test('quantity_kind') :-
   quantity_kind(isq:duration, isq:time).

:- end_tests(units).
