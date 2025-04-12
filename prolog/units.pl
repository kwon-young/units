:- module(units, [
   op(600, xfx, as),
   op(600, xfx, in),
   op(500, fy, quantity),
   op(100, yf,  []),
   op(99, xfy, :),
   alias/2,
   dimension_symbol/2,
   kind/1,
   quantity_character/2,
   quantity_formula/2,
   quantity_parent/2,

   prefix/3,
   absolute_point_origin/2,
   no_space_before_unit_symbol/1,
   prefix/3,
   relative_point_origin/2,
   unit_kind/2,
   unit_symbol/2,
   unit_symbol_formula/3,

   qeval/1
]).
:- reexport([units/q]).

:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(library(error)).

:- multifile alias/2.
:- multifile dimension_symbol/2.
:- multifile kind/1.
:- multifile quantity_character/2.
:- multifile quantity_formula/2.
:- multifile quantity_parent/2.

:- multifile absolute_point_origin/2.
:- multifile no_space_before_unit_symbol/1.
:- multifile prefix/3.
:- multifile relative_point_origin/2.
:- multifile unit_kind/2.
:- multifile unit_symbol/2.
:- multifile unit_symbol_formula/3.

units:dimension_symbol(dim_1, '').
units:quantity_parent(1, dim_1).
units:unit_kind(1, 1).

:- use_module(units/q).
:- use_module(units/systems/isq).
:- use_module(units/systems/si).
:- use_module(units/systems/angular).
:- use_module(units/systems/cgs).
:- use_module(units/systems/hep).
:- use_module(units/systems/iau).
:- use_module(units/systems/iec).
:- use_module(units/systems/imperial).
:- use_module(units/systems/international).
:- use_module(units/systems/usc).

user:portray(Q) :-
   is_dict(Q, q),
   get_dict(v, Q, V),
   get_dict(q, Q, Quantity),
   get_dict(u, Q, U),
   !,
   format("~p * ~p[~p]", [V, Quantity, U]).

is_quantity(Term) :-
   is_dict(Term, q),
   get_dict(q, Term, Q),
   (  var(Q)
   -> true
   ;  Q = kind_of(K)
   -> derived_root_kind(K)
   ;  alias_derived_quantity(Q)
   ),
   get_dict(u, Term, U),
   (  var(U)
   -> true
   ;  normalize_unit(U, _)
   ),
   get_dict(v, Term, _).
error:has_type(q:Quantity, Term) :-
   (  alias_derived_quantity(Quantity)
   -> true
   ;  domain_error(quantity, Quantity)
   ),
   is_quantity(Term),
   implicitly_convertible(Term.q, Quantity).

parse(A*B) ==>
   parse(A), parse(B).
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
parse(dim_1) ==>
   [].
parse(dim_1**_) ==>
   [].
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
   alias_quantity(Q).

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
      alias_or_quantity_parent(Unit, Formula)
   ),
   parse_normalize_factors(Formula**N, Factors).

:- table alias_quantity_parent/2.

alias_quantity_parent(Quantity, Parent) :-
   quantity_parent(Quantity, Parent),
   \+ dimension_symbol(Parent, _).
alias_quantity_parent(Alias, Parent) :-
   alias(Alias, Quantity),
   alias_quantity_parent(Quantity, Parent).

:- table alias_quantity_/1.

alias_quantity_(Quantity) :-
   alias_base_quantity(Quantity).
alias_quantity_(Quantity) :-
   alias_quantity_parent(Quantity, _).

alias_quantity(Quantity), \+ ground(Quantity) =>
   when(ground(Quantity), alias_quantity(Quantity)).
alias_quantity(Quantity) =>
   alias_quantity_(Quantity).

alias_derived_quantity(Quantity), \+ ground(Quantity) =>
   when(ground(Quantity), alias_derived_quantity(Quantity)).
alias_derived_quantity(Quantity) =>
   mapexpr(alias_quantity, Quantity).

alias_parent(Alias, Parent) :-
   alias(Alias, Quantity),
   (  alias_quantity_parent(Quantity, Parent)
   ;  base_quantity(Quantity), Parent = Quantity
   ).

:- table alias_or_quantity_parent/2.

alias_or_quantity_parent(Quantity, Parent) :-
   quantity_parent(Quantity, Parent),
   \+ dimension_symbol(Parent, _).
alias_or_quantity_parent(Alias, Quantity) :-
   alias(Alias, Quantity),
   alias_quantity(Quantity).

:- table alias_quantity_formula/2.

alias_quantity_formula(Quantity, Formula) :-
   quantity_formula(Quantity, Formula).
alias_quantity_formula(Alias, Formula) :-
   alias(Alias, Quantity),
   alias_quantity_formula(Quantity, Formula).

derived_quantity(_*_).
derived_quantity(_/_).
derived_quantity(_**_).

base_quantity(Quantity) :-
   quantity_parent(Quantity, Dimension),
   dimension_symbol(Dimension, _).

:- table alias_base_quantity/1.

alias_base_quantity(Quantity) :-
   base_quantity(Quantity).
alias_base_quantity(Alias) :-
   alias(Alias, Quantity),
   alias_base_quantity(Quantity).

:- table root/1.

root(BaseQuantity) :-
   base_quantity(BaseQuantity).
root(Quantity) :-
   quantity_parent(Quantity, DerivedQuantity),
   derived_quantity(DerivedQuantity).

:- table quantity_dimensions/2.

quantity_dimensions(Dimension, Dimension) :-
   dimension_symbol(Dimension, _).
quantity_dimensions(Quantity, Dimensions) :-
   alias(Quantity, Parent),
   quantity_dimensions(Parent, Dimensions).
quantity_dimensions(Quantity, Dimensions) :-
   quantity_parent(Quantity, Parent),
   quantity_dimensions(Parent, Dimensions).
quantity_dimensions(Quantity, Dimensions) :-
   derived_quantity(Quantity),
   mapexpr(quantity_dimensions, Quantity, Dimensions).

factors_dimensions(Factor, Dimensions) :-
   generate_expression([Factor], Quantity),
   quantity_dimensions(Quantity, Dimension),
   parse_normalize_factors(Dimension, Dimensions).

simplify_dimensions(Quantity, R) :-
   parse_normalize_factors(Quantity, Factors),
   maplist(factors_dimensions, Factors, Dimensions),
   pairs_keys_values(Pairs, Factors, Dimensions),
   phrase(simplify_dimension_pairs, Pairs, SimplifiedPairs),
   pairs_keys(SimplifiedPairs, SimplifiedFactors),
   generate_expression(SimplifiedFactors, R).
simplify_dimension_pairs -->
   select(_-A),
   { maplist(is_inverse, A, B) },
   select(_-B),
   !,
   simplify_dimension_pairs.
simplify_dimension_pairs -->
   [].

is_inverse(Q-N1, Q-N2) :-
   N2 is -N1.

:- table root_kind/1.

root_kind(Kind) :-
   kind(Kind).
root_kind(Root) :-
   root(Root).

:- table derived_root_kind_/1.

derived_root_kind_(Kind) :-
   mapexpr([X, X]>>root_kind(X), [X, _]>>domain_error(root_kind, X), Kind, Kind).

derived_root_kind(Kind), \+ ground(Kind) =>
   when(ground(Kind), derived_root_kind(Kind)).
derived_root_kind(Kind) =>
   derived_root_kind_(Kind).

:- table quantity_kind/2.

quantity_kind(kind_of(Kind), Kind).
quantity_kind(Kind, Kind) :-
   root_kind(Kind).
quantity_kind(Quantity, Kind) :-
   alias_or_quantity_parent(Quantity, Parent),
   quantity_kind(Parent, Kind).

derived_quantity_kind(Quantity, Kind) :-
   mapexpr(quantity_kind, [_, 1]>>true, Quantity, Kind).

common_quantity(Q1, Q2, Q), Q1=Q2 =>
   Q2 = Q.
common_quantity(kind_of(Q1), kind_of(Q2), Q) =>
   simplify_dimensions(Q1, K1),
   simplify_dimensions(Q2, K2),
   common_quantity(K1, K2, Q3),
   (  K1 == Q3
   -> Q = kind_of(Q2)
   ;  K2 == Q3
   -> Q = kind_of(Q1)
   ;  Q = Q3
   ).
common_quantity(kind_of(Q1), Q2, Q) =>
   simplify_dimensions(Q1, K1),
   common_quantity(K1, Q2, Q3),
   (  K1 == Q3
   -> Q = Q2
   ;  Q = Q3
   ).
common_quantity(Q1, kind_of(Q2), Q) =>
   common_quantity(kind_of(Q2), Q1, Q).
common_quantity(Q1, Q2, Q) =>
   common_expr(quantity, Q1, 1, Q2, 1, Q).

same_kind(Q1, Q2), Q1 = Q2 => true.
same_kind(Q1, Q2) =>
   derived_quantity_kind(Q1, K1),
   derived_quantity_kind(Q2, K2),
   common_quantity(K1, K2, K),
   (  (K1 == K ; K2 == K)
   -> true
   ).

%  From is implicitly convertible to To if:
%  
%  * From is a direct descendent of To: i.e. common_quantity(From, To, To)
%  * 
%
%  Exceptions:
%
%  * if To is a kind_of, then common_quantity(From, To, From)
%
%
implicitly_convertible(From, To, Explicit) :-
   normalize(To, NormalizedTo),
   mapexpr(alias_parent, NormalizedTo, AliasNormalizedTo),
   common_quantity(From, AliasNormalizedTo, CommonQuantity),
   (  AliasNormalizedTo = kind_of(_), CommonQuantity = From
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

implicitly_convertible(From, To), unifiable(From, To, _) =>
   From = To.
implicitly_convertible(From, To) =>
   implicitly_convertible(From, To, false).

explicitly_convertible(From, To), unifiable(From, To, _) =>
   From = To.
explicitly_convertible(From, To) =>
   explicitly_convertible_(From, To).

:- table explicitly_convertible_/2.

explicitly_convertible_(From, To) :-
   implicitly_convertible(From, To, true).
explicitly_convertible_(From, To) :-
   implicitly_convertible(To, From, true).

:- table alias_or_unit_symbol/2.

alias_or_unit_symbol(Unit, Symbol) :-
   (  unit_symbol(Unit, Symbol)
   ;  unit_symbol_formula(Unit, Symbol, _)
   ).
alias_or_unit_symbol(Alias, Symbol) :-
   alias(Alias, Unit),
   alias_or_unit_symbol(Unit, Symbol).

:- table alias_unit_symbol_formula/3.

alias_unit_symbol_formula(Unit, Symbol, Formula) :-
   unit_symbol_formula(Unit, Symbol, Formula).
alias_unit_symbol_formula(Alias, Symbol, Unit) :-
   alias(Alias, Unit),
   alias_or_unit_symbol(Unit, Symbol).

:- table has_prefix/2.

has_prefix(Module:PrefixUnit, Symbol) :-
   prefix(Module:Prefix, PrefixSymbol, _),
   PrefixUnit =.. [Prefix, Unit],
   (  alias_or_unit_symbol(Unit, UnitSymbol),
      atom_concat(PrefixSymbol, UnitSymbol, Symbol)
   -> true
   ;  domain_error("has_prefix", Module:PrefixUnit-Symbol)
   ).
has_prefix(Alias, Symbol) :-
   alias(Alias, Unit),
   has_prefix(Unit, Symbol).

:- table prefix_unit_symbol_formula/3.

prefix_unit_symbol_formula(Module:PrefixUnit, Symbol, PrefixFormula*Unit) :-
   \+ compound(Symbol),
   prefix(Module:Prefix, PrefixSymbol, PrefixFormula),
   PrefixUnit =.. [Prefix, Unit],
   alias_or_unit_symbol(Unit, UnitSymbol),
   \+ has_prefix(Unit, UnitSymbol),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol).

:- table alias_prefix_unit_symbol_formula/3.

alias_prefix_unit_symbol_formula(PrefixUnit, Symbol, Formula) :-
   prefix_unit_symbol_formula(PrefixUnit, Symbol, Formula).
alias_prefix_unit_symbol_formula(Alias, Symbol, Formula) :-
   alias(Alias, Unit),
   alias_prefix_unit_symbol_formula(Unit, Symbol, Formula).

:- table unit/3.

unit(U, S, F) :-
   (  alias_unit_symbol_formula(U, S, F)
   ;  alias_prefix_unit_symbol_formula(U, S, F)
   ).

:- table unit/2.

unit(U, S) :-
   (  unit_symbol(U, S)
   ;  unit(U, S, _)
   ).

all_unit_kind(Unit, R), unit_kind(Unit, Kind) =>
   R = kind_of(Kind).
all_unit_kind(Unit, R), unit(Unit, _, Formula) =>
   all_unit_kind(Formula, R).
all_unit_kind(Unit, R), derived_quantity(Unit) =>
   mapexpr(all_unit_kind, [_, 1]>>true, Unit, Kind),
   normalize(Kind, NKind),
   normalize_kind(NKind, R).
all_unit_kind(_, _) => fail.

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit), unifiable(Unit1, Unit2, _) =>
   Unit1 = Unit2,
   NewFactor1 = 1,
   NewFactor2 = 1,
   NewUnit = Unit2.
common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) =>
   common_expr(unit, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

comparable(AB, R) :-
   AB =.. [Op, A, B],
   eval_(A, A1),
   eval_(B, B1),
   (  same_kind(A1.q, B1.q), common_quantity(A1.q, B1.q, Q)
   -> (  common_unit(A1.u, AV, B1.u, BV, U)
      -> (  Op == is
         -> A1.v = A2,
            normalize(B1.v*BV/AV, B2)
         ;  normalize(A1.v*AV, A2),
            normalize(B1.v*BV, B2)
         ),
         V =.. [Op, A2, B2],
         R = q{v: V, u: U, q: Q}
      ;  domain_error(A1.u, B1.u)
      )
   ;  domain_error(A1.q, B1.q)
   ).

normalize_unit(Unit, R), var(Unit), ground(R) =>
   Unit = R.
normalize_unit(Unit, R), var(Unit), var(R) =>
   when((ground(Unit) ; ground(R)), normalize_unit(Unit, R)).
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
normalize_unit(U, R), derived_quantity(U) =>
   mapexpr(normalize_unit, U, R).
normalize_unit(_, _) => fail.

normalize_kind(kind_of(A)**N1/kind_of(B)**N2, R) =>
   normalize(A**N1/B**N2, AB),
   R = kind_of(AB).
normalize_kind(kind_of(A)/kind_of(B)**N, R) =>
   normalize(A/B**N, AB),
   R = kind_of(AB).
normalize_kind(kind_of(A)**N/kind_of(B), R) =>
   normalize(A**N/B, AB),
   R = kind_of(AB).
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
   comparable(quantity Result is ExprIn, R).
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
      (  ground(V1)
      -> V is V1
      ;  {V == V1}
      ),
      R = q{v: V, q: M.q, u: Q.u}
   ;  domain_error(M.q, Q.q)
   ).
eval_(as(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  implicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(force_as(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  explicitly_convertible(M.q, Quantity)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(cast(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  common_quantity(M.q, Quantity, _)
   -> R = M.put(q, Quantity)
   ;  domain_error(M.q, Quantity)
   ).
eval_(kind_of(Kind), R), derived_root_kind(Kind) =>
   R = q{v: _, q: kind_of(Kind), u: _}.
eval_(pi, R) =>
   R = q{v: pi, q: 1, u: 1}.
eval_(random_float, R) =>
   R = q{v: random_float, q: 1, u: 1}.
eval_(q:X, R), alias_derived_quantity(X) =>
   R = q{v: _, q: X, u: _}.
eval_(u:X, R) =>
   normalize_unit(X, U),
   when(ground(U), all_unit_kind(U, kind_of(UKind))),
   when((ground(UKind), ground(Q)), implicitly_convertible(kind_of(UKind), Q)),
   R = q{v: 1, q: Q, u: U}.
eval_(quantity(Quantity), R) =>
   eval_(_*q:_[u:_], Quantity),
   R = Quantity.
eval_(QuantityExpr[UnitExpr], R) =>
   eval_(QuantityExpr, R),
   eval_(UnitExpr, Unit),
   (  implicitly_convertible(Unit.q, R.q)
   -> true
   ;  domain_error(Unit.q, R.q)
   ),
   R.v = Unit.v,
   R.u = Unit.u.
eval_(X, R), var(X) =>
   R = q{v: X, q: 1, u: 1}.
eval_(Q, R), is_dict(Q, q) =>
   R = Q.
eval_(N, R), number(N) =>
   R = q{v: N, q: 1, u: 1}.
eval_(UnitOrSymbol, R), ground(UnitOrSymbol), normalize_unit(UnitOrSymbol, Unit) =>
   all_unit_kind(Unit, Kind),
   R = q{v: 1, q: Kind, u: Unit}.
eval_(Quantity, R), ground(Quantity), alias_quantity(Quantity) =>
   R = q{v: _, q: Quantity, u: _}.

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
implicitly_convertible_data(kind_of(isq:time*isq:frequency), kind_of(isq:angular_measure)).
implicitly_convertible_data(kind_of(isq:rotation/isq:frequency), kind_of(isq:time)).

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
   qeval(_Speed is (m/s in inch/hour) as isq:speed).

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
   qeval(Speed is 60 * isq:velocity[km/hour]),
   qeval(Duration is 8 * s),
   qeval(A is (Speed / Duration) as isq:acceleration),
   qeval(B is A in m/s**2),
   must_be(q:isq:acceleration, B).

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
