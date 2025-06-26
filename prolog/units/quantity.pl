:- module(quantity, [
   any_quantity/1,
   quantity_dimensions/2,
   common_quantity/3,
   implicitly_convertible/2,
   explicitly_convertible/2,
   same_kind/2,
   alias_or_child_quantity_parent/2,
   normalize_kind/2
]).

:- use_module(utils).
:- use_module(search).
:- use_module('../units.pl').

base_quantity(Quantity) :-
   quantity_parent(Quantity, Dimension),
   dimension_symbol(Dimension, _).

child_quantity_parent(Child, Parent) :-
   quantity_parent(Child, Parent),
   \+ dimension_symbol(Parent, _).

alias_quantity(Quantity) :-
   aliased(quantity_parent(Quantity, _)).

% derived quantity, with alias, lazy
any_quantity(Quantity) :-
   lazy(any_quantity_(Quantity), Quantity).
any_quantity_(kind_of(Kind)) =>
   mapexpr1(root_kind, [_]>>fail, Kind).
any_quantity_(Quantity) =>
   mapexpr1(alias_quantity, [_]>>fail, Quantity).

alias_quantity_formula(Quantity, Formula) :-
   aliased(quantity_formula(Quantity, Formula)).

:- table root_kind/1.

root_kind(Kind) :-
   kind(Kind).
root_kind(BaseQuantity) :-
   base_quantity(BaseQuantity).
root_kind(Quantity) :-
   quantity_parent(Quantity, DerivedQuantity),
   derived(DerivedQuantity).

:- table quantity_kind/2.

quantity_kind(kind_of(Kind), Kind).
quantity_kind(Kind, Kind) :-
   root_kind(Kind), !.
quantity_kind(Quantity, Kind) :-
   alias_or_child_quantity_parent(Quantity, Parent),
   quantity_kind(Parent, Kind).

derived_quantity_kind(Quantity, NormalizedKind) :-
   mapexpr(quantity_kind, [_, _]>>fail, Quantity, Kind),
   normalize(Kind, NormalizedKind).

:- table quantity_dimensions/2.

quantity_dimensions(Dimension, Dimension) :-
   dimension_symbol(Dimension, _).
quantity_dimensions(kind_of(Quantity), Dimension) :-
   quantity_dimensions(Quantity, Dimension).
quantity_dimensions(Quantity, Dimensions) :-
   aliased(quantity_parent(Quantity, Parent)),
   quantity_dimensions(Parent, Dimensions).
quantity_dimensions(Quantity, NormalizedDimensions) :-
   derived(Quantity),
   mapexpr(quantity_dimensions, [_, _]>>fail, Quantity, Dimensions),
   normalize_dimension(Dimensions, NormalizedDimensions).

same_dimension(Q, Q) :- !.
same_dimension(Q1, Q2) :-
   lazy(quantity_dimensions(Q1, D), Q1),
   lazy(quantity_dimensions(Q2, D), Q2).

factor_dimensions(Factor, Dimensions) :-
   generate_expression([Factor], Quantity),
   quantity_dimensions(Quantity, Dimension),
   parse_normalize_factors(Dimension, Dimensions).

simplify_dimensions(Quantity, R) :-
   parse_normalize_factors(Quantity, Factors),
   maplist(factor_dimensions, Factors, Dimensions),
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

common_quantity(Q, Q, Q) :- !.
common_quantity(Q1, Q2, Q) :-
   same_dimension(Q1, Q2),
   common_quantity_(Q1, Q2, Q).
% intersection of two subtrees:
% if T1 is inside T2, Q = T1
% if T2 is inside T1, Q = T2
% else Q is the common ancestor of T1, T2
common_quantity_(kind_of(Q1), kind_of(Q2), Q) =>
   simplify_dimensions(Q1, K1),
   simplify_dimensions(Q2, K2),
   common_quantity_(K1, K2, Q3),
   (  K1 == Q3 % K2 is inside K1
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
   common_expr(alias_or_child_quantity_parent, Q1, 1, Q2, 1, Q).

alias_or_child_quantity_parent(Child, Parent) :-
   (  alias(Child, Parent),
      aliased(quantity_parent(Parent, _))
   ;  child_quantity_parent(Child, Parent)
   ).

same_kind(Q1, Q2), Q1 = Q2 => true.
same_kind(Q1, Q2) =>
   derived_quantity_kind(Q1, K1),
   derived_quantity_kind(Q2, K2),
   common_quantity(K1, K2, K),
   (  (K1 == K ; K2 == K)
   -> true
   ).

%% implicitly_convertible(?From, ?To) is semidet.
%
%  Checks if `From` can be implicitly converted to `To`.
%  Implicit conversion is possible if:
%  - `From` and `To` are the same.
%  - `From` is a direct descendant of `To` in the quantity hierarchy,
%    and the conversion does not cross a defined `kind/1` barrier.
%  - `To` is a `kind_of(X)` (a subtree) and `From` is in the subtree of `X`,
%    and the conversion does not cross a defined `kind/1` barrier.
%  - `To` is a root and `From` is implicitly convertible
%    to the parent formula of `To`, without crossing a kind barrier.
%  - `To` has a `quantity_formula/2` defined, and `From`
%    is implicitly convertible to that formula.
implicitly_convertible(Q, Q) :- !.
% From is implicitly convertible to To if From is a direct descendant of To
% meaning: common_quantity(From, To, To).
%
% additionally, if To is a kind_of, From can be part of the subtree of To.
% meaning: common_quantity(From, To, From).
%
% moreover, the conversion of From to To should not cross a kind.
% i.e. the kind of From should be a direct ancestor of To.
% meaning: quantity_kind(From, K), kind(K), common_quantity(K, To, K)
implicitly_convertible(From, To) :-
   % normalizing + dealiasing is necessary for unification check
   normalize(To, NormalizedTo),
   mapexpr(alias, NormalizedTo, AliasNormalizedTo),
   common_quantity(From, AliasNormalizedTo, CommonQuantity),
   (  AliasNormalizedTo = kind_of(_), CommonQuantity = From
   ;  % unification check here
      CommonQuantity = AliasNormalizedTo
   ),
   derived_quantity_kind(From, FromKind),
   (  kind(FromKind)
   -> common_quantity(FromKind, AliasNormalizedTo, FromKind)
   ;  true
   ),
   !.
% From can be implicitly converted to a kind To if From can be implicitly
% converted to the parent formula of the kind To.
% In this case, the formula becomes a common ancestor of From to To and
% From and To can not be directly related.
% But, same as above, conversion of From to To should not cross a kind
implicitly_convertible(From, ToKind) :-
   root_kind(ToKind),
   aliased(child_quantity_parent(ToKind, Formula)),
   implicitly_convertible(From, Formula),
   derived_quantity_kind(From, FromKind),
   common_quantity(FromKind, ToKind, FromKind),
   !.
% From can be implicitly converted to To if there is a formula for it
% and From is implicitly convertible to the formula.
implicitly_convertible(From, To) :-
   alias_quantity_formula(To, Formula),
   implicitly_convertible(From, Formula).

%% explicitly_convertible(?From, ?To) is semidet.
%
%  Two quantities are explicitly convertible if they are directly related,
%  meaning one can be implicitly converted to the other.
explicitly_convertible(From, To) :-
   implicitly_convertible(From, To), !.
explicitly_convertible(From, To) :-
   implicitly_convertible(To, From).

normalize_kind(A*B, R) =>
   normalize_kind(A, AK),
   normalize_kind(B, BK),
   normalize_kind_(AK*BK, R).
normalize_kind(A/B, R) =>
   normalize_kind(A, AK),
   normalize_kind(B, BK),
   normalize_kind_(AK/BK, R).
normalize_kind(A**N, R) =>
   normalize_kind(A, AK),
   normalize_kind_(AK**N, R).
normalize_kind(A, R) =>
   normalize_kind_(A, R).

normalize_kind_(kind_of(A)/kind_of(B), R) =>
   normalize_kind_(kind_of(A/B), R).
normalize_kind_(kind_of(A)*kind_of(B), R) =>
   normalize_kind_(kind_of(A*B), R).
normalize_kind_(kind_of(A)**N, R) =>
   normalize_kind_(kind_of(A**N), R).
normalize_kind_(kind_of(A)/1, R) =>
   normalize_kind_(kind_of(A), R).
normalize_kind_(1/kind_of(B), R) =>
   normalize_kind_(kind_of(1/B), R).
normalize_kind_(kind_of(A)*1, R) =>
   normalize_kind_(kind_of(A), R).
normalize_kind_(1*kind_of(B), R) =>
   normalize_kind_(kind_of(B), R).
normalize_kind_(kind_of(A)/B, R) =>
   normalize(A/B, R).
normalize_kind_(A/kind_of(B), R) =>
   normalize(A/B, R).
normalize_kind_(kind_of(A)*B, R) =>
   normalize(A*B, R).
normalize_kind_(A*kind_of(B), R) =>
   normalize(A*B, R).
normalize_kind_(kind_of(A), R) =>
   normalize(A, A1),
   R = kind_of(A1).
normalize_kind_(A, R) =>
   normalize(A, R).

:- begin_tests(quantity).

test('quantity_kind') :-
   quantity_kind(isq:duration, isq:time).

normalize_kind_data(kind_of(isq:mass)/(kind_of(isq:length)*kind_of(isq:time)**2),
                    kind_of(isq:mass/(isq:length*isq:time**2))).
normalize_kind_data(1*kind_of(isq:length**3), kind_of(isq:length**3)).
normalize_kind_data(kind_of(isq:time)/kind_of(isq:time), kind_of(1)).

test('normalize_kind', [forall(normalize_kind_data(K1, K2))]) :-
   normalize_kind(K1, R),
   R == K2.

:- end_tests(quantity).
