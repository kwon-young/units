:- module(units, [
   op(600, yfx, as),
   op(600, yfx, in),
   op(300, fy, quantity),
   op(300, fy, unit),
   op(300, fy, quantity_point),
   op(300, fy, origin),
   op(300, fy, point),
   op(300, fy, quantity_from_zero),
   op(300, yfx, quantity_from),
   op(300, yfx, point_for),
   op(100, yf,  []),
   op(99, xfy, :),

   qeval/1,
   qformat/1,

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
   unit_origin/2,
   unit_symbol/2,
   unit_symbol_formula/3
]).
:- reexport([units/q]).
:- reexport([units/qp]).

:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(library(error)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

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
:- multifile unit_origin/2.
:- multifile unit_symbol/2.
:- multifile unit_symbol_formula/3.

units:absolute_point_origin(0, _).
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

qformat(M) :-
   mapexpr(unit, M.u, Symbol),
   (  alias_no_space_before_unit_symbol(Symbol)
   -> Space = ""
   ;  Space = " "
   ),
   format("~h~s~w", [M.v, Space, Symbol]).

is_quantity(Term, R) :-
   catch(eval_(Term, R), _, fail),
   is_dict(R, q),
   get_dict(q, R, Q),
   (  var(Q)
   -> true
   ;  Q = kind_of(K)
   -> derived_root_kind(K)
   ;  alias_derived_quantity(Q)
   ),
   get_dict(u, R, U),
   (  var(U)
   -> true
   ;  normalize_unit(U, _)
   ),
   get_dict(v, R, _).
error:has_type(quantity(Quantity), Term) :-
   (  alias_derived_quantity(Quantity)
   -> true
   ;  domain_error(quantity, Quantity)
   ),
   is_quantity(Term, R),
   implicitly_convertible(R.q, Quantity).
error:has_type(Quantity, Term) :-
   ground(Quantity),
   alias_derived_quantity(Quantity),
   !,
   error:has_type(quantity(Quantity), Term).

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

inverse([]) --> [].
inverse([A-N | L]) -->
   { N1 is -N },
   [A-N1],
   inverse(L).

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
   parse(In, L),
   normalize_factors(L, L1),
   generate_expression(L1, Out).

normalize_dimension(In, Out) :-
   normalize(In, N),
   (  N == 1
   -> Out = dim_1
   ;  Out = N
   ).

is_num(_-N) :- N > 0.

power(A-1, A) :- !.
power(A-N, A**N).

generate_expression(In, Out) :-
   partition(is_num, In, Num, Denom),
   maplist(power, Num, Num1),
   phrase(inverse(Denom), Denom1),
   maplist(power, Denom1, Denom2),
   num_denom(Num1, Denom2, Out).

parse_normalize_factors(In, L3) :-
   parse(In, L),
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

:- table common_expr/6.

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
   unit(U, _).
is_of(quantity, Q-_) :-
   alias_quantity_(Q).

:- meta_predicate partition_soft(1,+,-,-).

partition_soft(Pred, List, Included, Excluded) :-
    partition_soft_(List, Pred, Included, Excluded).

partition_soft_([], _, [], []).
partition_soft_([H|T], Pred, Incl, Excl) :-
    (   call(Pred, H)
    *->  Incl=[H|I],
        partition_soft_(T, Pred, I, Excl)
    ;   Excl=[H|E],
        partition_soft_(T, Pred, Incl, E)
    ).

common_factors(L1, R1, Type, L, N, L2, R2) :-
   partition_soft(is_of(Type), L1, Unit1, Factor1),
   normalize_factors(Unit1, NUnit1),
   partition_soft(is_of(Type), L2, Unit2, Factor2),
   normalize_factors(Unit2, NUnit2),
   ord_intersection(NUnit1, NUnit2, CommonUnits, Unit2Only),
   ord_subtract(NUnit1, NUnit2, Unit1Only),
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
quantity_dimensions(kind_of(Quantity), Dimension) :-
   quantity_dimensions(Quantity, Dimension).
quantity_dimensions(Quantity, Dimensions) :-
   alias(Quantity, Parent),
   quantity_dimensions(Parent, Dimensions).
quantity_dimensions(Quantity, Dimensions) :-
   quantity_parent(Quantity, Parent),
   quantity_dimensions(Parent, Dimensions).
quantity_dimensions(Quantity, NormalizedDimensions) :-
   derived_quantity(Quantity),
   mapexpr(quantity_dimensions, Quantity, Dimensions),
   normalize_dimension(Dimensions, NormalizedDimensions).

same_dimension(Q, Q) :- !.
same_dimension(Q1, Q2) :-
   when(ground(Q1), quantity_dimensions(Q1, D)),
   when(ground(Q2), quantity_dimensions(Q2, D)).

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

common_quantity(Q1, Q2, Q) :-
   same_dimension(Q1, Q2),
   common_quantity_(Q1, Q2, Q).
common_quantity_(Q1, Q2, Q), Q1=Q2 =>
   Q2 = Q.
common_quantity_(kind_of(Q1), kind_of(Q2), Q) =>
   simplify_dimensions(Q1, K1),
   simplify_dimensions(Q2, K2),
   common_quantity_(K1, K2, Q3),
   (  K1 == Q3
   -> Q = kind_of(Q2)
   ;  K2 == Q3
   -> Q = kind_of(Q1)
   ;  Q = Q3
   ).
common_quantity_(kind_of(Q1), Q2, Q) =>
   simplify_dimensions(Q1, K1),
   common_quantity_(K1, Q2, Q3),
   (  K1 == Q3
   -> Q = Q2
   ;  Q = Q3
   ).
common_quantity_(Q1, kind_of(Q2), Q) =>
   common_quantity_(kind_of(Q2), Q1, Q).
common_quantity_(Q1, Q2, Q) =>
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

:- table all_unit_kind/2.

all_unit_kind(Unit, Kind) :-
   all_unit_kind_(Unit, Kind).

all_unit_kind_(Unit, R), unit_kind(Unit, Kind) =>
   R = kind_of(Kind).
all_unit_kind_(Unit, R), unit(Unit, _, Formula) =>
   all_unit_kind_(Formula, R).
all_unit_kind_(Unit, R), derived_quantity(Unit) =>
   mapexpr(all_unit_kind_, [_, 1]>>true, Unit, Kind),
   normalize(Kind, NKind),
   normalize_kind(NKind, R).
all_unit_kind_(_, _) => fail.

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit), unifiable(Unit1, Unit2, _) =>
   Unit1 = Unit2,
   NewFactor1 = 1,
   NewFactor2 = 1,
   NewUnit = Unit2.
common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) =>
   common_expr(unit, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

alias_no_space_before_unit_symbol(Unit) :-
   no_space_before_unit_symbol(Unit).
alias_no_space_before_unit_symbol(Alias) :-
   alias(Alias, Unit),
   alias_no_space_before_unit_symbol(Unit).

unit_origin_0(Unit, Origin) =>
   (  unit_origin(Unit, O)
   -> normalize_origin(O, Origin)
   ;  eval_(0*Unit, Q),
      Origin = qp{o: 0, q: Q}
   ).

:- table alias_origin_/1.

alias_origin_(Origin) :-
   (  absolute_point_origin(Origin, _)
   ;  relative_point_origin(Origin, _)
   ).
alias_origin_(Alias) :-
   alias(Alias, Origin),
   alias_origin_(Origin).

alias_origin(Origin) :-
   when(ground(Origin), alias_origin_(Origin)).

normalize_origin(Origin, qp{o: Origin, q: Q}) :-
   when(ground(Origin), normalize_origin_(Origin, Q)),
   eval_(0*_[_], Q).
normalize_origin_(Origin, Q), unit_origin(Unit, Origin) =>
   eval_(Unit, R),
   Q = R.put([v=0]).
normalize_origin_(Origin, Q), absolute_point_origin(Origin, Quantity) =>
   eval_(Quantity[_], R),
   Q = R.put([v=0]).
normalize_origin_(Origin, Q), relative_point_origin(Origin, Expr) =>
   eval_(Expr, QP),
   Q = QP.q.put([v=0]).
normalize_origin_(Alias, Q), alias(Alias, Origin) =>
   normalize_origin_(Origin, QP),
   Q = QP.q.

common_origin(O1, F1, O2, F2, O) :-
   once(iterative_deepening(1, common_origin_(O1, F1, O2, F2, O))).

common_origin(O, F, O, F, O, _-N) :-
   setarg(1, N, no),
   eval_(0*_[_], F).
common_origin(O1, F1, O2, F2, O, N) :-
   relative_point_origin(O1, Expr),
   eval_(Expr, R),
   common_origin_(R.o, FF1, O2, F2, O, N),
   F1 = R.q + FF1.
common_origin(Alias, F1, O2, F2, O, N) :-
   alias(Alias, O1),
   common_origin_(O1, F1, O2, F2, O, N).
common_origin_(O1, F1, O2, F2, O, Limit-N) :-
   (  Limit > 0
   -> Limit1 is Limit - 1
   ;  nb_setarg(1, N, depth_limit_exceeded),
      fail
   ),
   (  common_origin(O1, F1, O2, F2, O, Limit1-N)
   ;  common_origin(O2, F2, O1, F1, O, Limit1-N)
   ).

comparable(AB, R) :-
   AB =.. [Op, A, B],
   eval_(B, B1),
   is_dict(B1, BTag),
   (  Op == is, var(A)
   -> comparable_is(A, BTag:B1, R)
   ;  eval_(A, A1),
      is_dict(A1, ATag),
      comparable(Op, ATag:A1, BTag:B1, R)
   ).
comparable(is, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, _)
   -> comparable(A.q is (F2 + B.q) - F1, RQ),
      R = A.put([q=RQ])
   ;  domain_error(A.o, B.o)
   ).
comparable(=:=, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, O)
   -> comparable(A.q + F1 =:= F2 + B.q, RQ),
      R = qp{o: O, q: RQ}
   ;  domain_error(A.o, B.o)
   ).
comparable(=\=, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, O)
   -> comparable(A.q + F1 =\= F2 + B.q, RQ),
      R = qp{o: O, q: RQ}
   ;  domain_error(A.o, B.o)
   ).
comparable(-, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, _)
   -> comparable((F1 + A.q) - (F2 + B.q), R)
   ;  domain_error(A.o, B.o)
   ).
comparable(-, qp:A, q:B, R) =>
   comparable(A.q - B, Q),
   R = A.put([q=Q]).
comparable(+, qp:A, q:B, R) =>
   comparable(+, q:(A.q), q:B, RQ),
   R = A.put([q=RQ]).
comparable(+, q:A, qp:B, R) =>
   comparable(+, qp:B, q:A, R).
comparable(Op, q:A, q:B, R) =>
   (  same_kind(A.q, B.q), common_quantity(A.q, B.q, Q)
   -> (  common_unit(A.u, AV, B.u, BV, U)
      -> (  Op == is
         -> A.v = A2,
            normalize(B.v*BV/AV, B2)
         ;  normalize(A.v*AV, A2),
            normalize(B.v*BV, B2)
         ),
         V =.. [Op, A2, B2],
         R = q{v: V, u: U, q: Q}
      ;  domain_error(A.u, B.u)
      )
   ;  domain_error(A.q, B.q)
   ).

comparable_is(A, q:B, R) =>
   R = B.put([v=(V is B.v)]),
   A = V*B.q[B.u].
comparable_is(A, qp:B, R) =>
   comparable_is(AQ, q:(B.q), RQ),
   R = B.put([q=RQ]),
   O = B.o,
   (  (var(O) ; O = 0)
   -> Origin = origin(O)
   ;  Origin = O
   ),
   A = Origin+AQ.

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

normalize_kind_(kind_of(A)**N1/kind_of(B)**N2, R) =>
   normalize(A**N1/B**N2, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)/kind_of(B)**N, R) =>
   normalize(A/B**N, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)**N/kind_of(B), R) =>
   normalize(A**N/B, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)/kind_of(B), R) =>
   normalize(A/B, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)*kind_of(B), R) =>
   normalize(A*B, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)**N, R) =>
   normalize(A**N, AN),
   R = kind_of(AN).
normalize_kind_(kind_of(A)/1, R) =>
   R = kind_of(A).
normalize_kind_(1/kind_of(A), R) =>
   normalize(1/A, AN),
   R = kind_of(AN).
normalize_kind_(kind_of(A)*1, R) =>
   R = kind_of(A).
normalize_kind_(1*kind_of(A), R) =>
   R = kind_of(A).
normalize_kind_(kind_of(A)/B, R) =>
   normalize(A/B, R).
normalize_kind_(A/kind_of(B), R) =>
   normalize(A/B, R).
normalize_kind_(kind_of(A)*B, R) =>
   normalize(A*B, R).
normalize_kind_(A*kind_of(B), R) =>
   normalize(A*B, R).
normalize_kind_(_, _) => fail.

normalize_kind(E, R), mapsubterms(normalize_kind_, E, E1), dif(E, E1) =>
   normalize_kind(E1, R).
normalize_kind(E, R) =>
   normalize(E, R).

qeval((A, B)) =>
   qeval(A),
   qeval(B).
qeval(Expr) =>
   eval_(Expr, Q),
   is_dict(Q, Tag),
   qeval_call(Tag:Q).

qeval_call(q:Q) =>
   V = Q.v,
   (  (ground(V) ; V = {_} ; (V = (R is E), var(R), ground(E)))
   -> call(V)
   ;  call({V})
   ).
qeval_call(qp:P) =>
   qeval_call(q:(P.q)).

eval_({ExprIn}, R) =>
   eval_(ExprIn, ExprOut),
   R = ExprOut.put(v, {ExprOut.v}).
eval_(Result is ExprIn, R) =>
   comparable(Result is ExprIn, R).
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
   (  is_dict(M, qp)
   -> eval_(in(M.q, Unit), Q),
      R = M.put([q=Q])
   ;  eval_(Unit, Q),
      (  implicitly_convertible(M.q, Q.q)
      -> common_unit(M.u, F1, Q.u, F2, _),
         normalize(M.v*F1/F2, V),
         R = q{v: V, q: M.q, u: Q.u}
      ;  domain_error(M.q, Q.q)
      )
   ).
eval_(as(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  is_dict(M, qp)
   -> eval_(as(M.q, Quantity), Q),
      R = M.put([q=Q])
   ;  (  implicitly_convertible(M.q, Quantity)
      -> R = M.put(q, Quantity)
      ;  domain_error(M.q, Quantity)
      )
   ).
eval_(force_as(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  is_dict(M, qp)
   -> eval_(force_as(M.q, Quantity), Q),
      R = M.put([q=Q])
   ;  (  explicitly_convertible(M.q, Quantity)
      -> R = M.put(q, Quantity)
      ;  domain_error(M.q, Quantity)
      )
   ).
eval_(cast(Expr, Quantity), R), alias_derived_quantity(Quantity) =>
   eval_(Expr, M),
   (  is_dict(M, qp)
   -> eval_(cast(M.q, Quantity), Q),
      R = M.put([q=Q])
   ;  (  common_quantity(M.q, Quantity, _)
      -> R = M.put(q, Quantity)
      ;  domain_error(M.q, Quantity)
      )
   ).
eval_(pi, R) =>
   R = q{v: pi, q: 1, u: 1}.
eval_(random_float, R) =>
   R = q{v: random_float, q: 1, u: 1}.
eval_(unit(X), R) =>
   normalize_unit(X, U),
   when(ground(U), all_unit_kind(U, kind_of(UKind))),
   when((ground(UKind), ground(Q)), implicitly_convertible(kind_of(UKind), Q)),
   R = q{v: 1, q: Q, u: U}.
eval_(quantity(Quantity), R) =>
   Quantity = _*_[_],
   eval_(Quantity, R).
eval_(QuantityExpr[UnitExpr], R) =>
   eval_q(QuantityExpr, Q),
   (  var(UnitExpr)
   -> eval_(unit(UnitExpr), Unit)
   ;  eval_(UnitExpr, Unit)
   ),
   (  implicitly_convertible(Unit.q, Q)
   -> true
   ;  domain_error(Unit.q, Q)
   ),
   R = Unit.put([q=Q]).
eval_(point(Expr), R) =>
   eval_(Expr, Q),
   unit_origin_0(Q.u, Origin),
   R = Origin.put([q=Q]).
eval_(quantity_point(QP), R) =>
   QP = O + Q,
   (  var(O)
   -> Origin = origin(O)
   ;  Origin = O
   ),
   (  var(Q)
   -> Quantity = quantity(Q)
   ;  Quantity = Q
   ),
   eval_(Origin + Quantity, R).
eval_(origin(Origin), R), alias_origin(Origin) =>
   normalize_origin(Origin, R).
eval_(exp(Expr), R) =>
   eval_(Expr in 1, R1),
   R = R1.put([v=exp(R1.v)]).
eval_(quantity_from_zero(Expr), R) =>
   eval_(Expr - origin(0), R).
eval_(quantity_from(Expr, Origin), R) =>
   eval_(Expr, R1),
   eval_(R1 - Origin in R1.q.u, R).
eval_(point_for(Expr, Origin), R) =>
   (  (var(Origin) ; Origin = 0)
   -> O = origin(Origin)
   ;  O = Origin
   ),
   eval_(O + quantity_from(Expr, Origin), R).
eval_(X, R), var(X) =>
   R = q{v: X, q: 1, u: 1}.
eval_(Q, R), is_dict(Q, q) =>
   R = Q.
eval_(N, R), number(N) =>
   R = q{v: N, q: 1, u: 1}.
eval_(UnitOrSymbol, R), ground(UnitOrSymbol), normalize_unit(UnitOrSymbol, Unit) =>
   all_unit_kind(Unit, Kind),
   R = q{v: 1, q: Kind, u: Unit}.
eval_(Point, R), is_dict(Point, qp) =>
   R = Point.
eval_(Origin, R), alias_origin(Origin) =>
   normalize_origin(Origin, R).

eval_q(quantity(Q), R), alias_derived_quantity(Q) =>
   R = Q.
eval_q(X, R), alias_derived_quantity(X) =>
   R = X.
eval_q(kind_of(Kind), R), derived_root_kind(Kind) =>
   R = kind_of(Kind).

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

test('not_common_quantity', [fail]) :-
   call_with_time_limit(
      1,
      common_quantity(isq:voltage * isq:time/(isq:capacitance * isq:resistance), isq:time, _)
   ).

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
test('avg_speed2') :-
   avg_speed(isq:height[inch], quantity _Time, isq:speed[m/hour]).

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
   must_be(isq:acceleration, B).

test('clpBNR') :-
   qeval({A * inch =:= 1 * metre}),
   A == 5000r127,
   qeval({B =:= 5000 * gram / (2*gram)}),
   B == 2500,
   qeval({C is 1^2}),
   C == 1*1[1],
   qeval(D*unit(_) is 0.1),
   must_be(number, D).

test('quantity_kind') :-
   quantity_kind(isq:duration, isq:time).

test('quantity_point') :-
   qeval(_ is point(42*m)),
   qeval(_ is point(42*kelvin)),
   qeval(_ is point(42*degree_Celsius)),
   qeval(QP1 is point(100*m)),
   qeval(QP2 is point(120*m)),
   qeval(quantity_from_zero(QP1) =:= 100*m),
   qeval(quantity_from_zero(QP2) =:= 120*m),
   qeval(quantity_from(QP2, QP1) =:= 20*m),
   qeval(quantity_from(QP1, QP2) =:= -20*m),
   qeval(QP2 - QP1 =:= 20*m),
   qeval(QP1 - QP2 =:= -20*m).

test('quantity_point_error', [error(_)]) :-
   qeval(QP1 is point(100*isq:distance[m])),
   qeval(QP2 is point(120*isq:height[m])),
   qeval(_ is QP1 + QP2).

test('quantity_point2') :-
   qeval(QP1 is point(100*isq:distance[m])),
   qeval(QP2 is point(120*isq:height[m])),
   qeval(quantity_from(QP2, QP1) =:= 20*m),
   qeval(quantity_from(QP1, QP2) =:= -20*m),
   qeval(QP2 - QP1 =:= 20*m),
   qeval(QP1 - QP2 =:= -20*m).

test('absolute_point_origin') :-
   qeval(QP1 is si:absolute_zero + 100*kelvin),
   qeval(QP2 is 120*kelvin + si:absolute_zero),
   qeval(quantity_from(QP2, QP1) =:= 20*kelvin),
   qeval(quantity_from(QP1, si:absolute_zero) =:= 100*kelvin),
   qeval(quantity_from(QP2, si:absolute_zero) =:= 120*kelvin),
   qeval(quantity_from(QP1, QP2) =:= -20*kelvin),
   qeval(QP1 - si:absolute_zero =:= 100*kelvin),
   qeval(QP2 - si:absolute_zero =:= 120*kelvin),
   qeval(si:absolute_zero - QP1 =:= -100*kelvin),
   qeval(si:absolute_zero - QP2 =:= -120*kelvin).

test('absolute_point_origin_error', [error(_)]) :-
   qeval(_ is quantity_from_zero(si:absolute_zero + 100*kelvin)).

units:absolute_point_origin(oa,isq:distance).
units:relative_point_origin(ob, oa + 10*m).
units:relative_point_origin(oc, ob + 10*m).
units:relative_point_origin(od, oa + 30*m).

test('relative_point_origin') :-
   qeval(QP1 is oc + 100*m),
   qeval(QP2 is od + 120*m),
   QP1 = QP1O + _,
   QP2 = QP2O + _,
   qeval(quantity_from(QP1, QP1O) =:= 100*m),
   qeval(quantity_from(QP2, QP2O) =:= 120*m),
   qeval(quantity_from(QP2, QP1) =:= 30*m),
   qeval(quantity_from(QP1, QP2) =:= -30*m),
   qeval(QP2 - QP1 =:= 30*m),
   qeval(QP1 - QP2 =:= -30*m),
   qeval(quantity_from(QP1, oa) =:= 120*m),
   qeval(quantity_from(QP1, ob) =:= 110*m),
   qeval(quantity_from(QP1, oc) =:= 100*m),
   qeval(quantity_from(QP1, od) =:= 90*m),
   qeval(QP1 - oa =:= 120*m),
   qeval(QP1 - ob =:= 110*m),
   qeval(QP1 - oc =:= 100*m),
   qeval(QP1 - od =:= 90*m),
   qeval(quantity_from(QP2, oa) =:= 150*m),
   qeval(quantity_from(QP2, ob) =:= 140*m),
   qeval(quantity_from(QP2, oc) =:= 130*m),
   qeval(quantity_from(QP2, od) =:= 120*m),
   qeval(QP2 - oa =:= 150*m),
   qeval(QP2 - ob =:= 140*m),
   qeval(QP2 - oc =:= 130*m),
   qeval(QP2 - od =:= 120*m),
   qeval(ob - oa =:= 10*m),
   qeval(oc - oa =:= 20*m),
   qeval(od - oa =:= 30*m),
   qeval(od - oc =:= 10*m),
   qeval(ob - ob =:= 0*m),
   QP2B = origin(QP2BO) + quantity(_),
   qeval(QP2B is point_for(QP2, ob)),
   qeval(quantity_from(QP2B, QP2BO) =:= 140*m),
   QP2A = quantity_point(QP2AO + _),
   qeval(QP2A is point_for(QP2, oa)),
   qeval(quantity_from(QP2A, QP2AO) =:= 150*m),
   qeval(QP2 =:= QP2B),
   qeval(QP2 =:= QP2A),
   true.

test('variable_origin') :-
   qeval(_ is origin(_) + quantity(_)).

test('temperature') :-
   qeval(_ is si:zeroth_degree_Celsius + 20.5 * degree_Celsius),
   qeval(_ is point(20.5 * degree_Celsius)),
   true.

test('radian') :-
   qeval(_ is m/m in radian).

:- end_tests(units).
