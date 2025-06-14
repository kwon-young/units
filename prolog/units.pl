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
   qformat/2,

   alias/2,
   dimension_symbol/2,
   kind/1,
   quantity_character/2,
   quantity_formula/2,
   quantity_parent/2,

   absolute_point_origin/2,
   no_space_before_unit_symbol/1,
   prefix/3,
   relative_point_origin/2,
   unit_kind/2,
   unit_origin/2,
   unit_symbol/2,
   unit_symbol_formula/3
]).

/** <module> units
 *
 *  Units is a quantity and units library modeled after
 *  [`mp-units`](https://mpusz.github.io/mp-units/latest/).
 *
 *  The key features are:
 *
 *  * `qeval/1` wrapper predicate for all arithmetic with units and quantities
 *  * Large amount of predefined units and quantities
 *  * safe arithmetic with units, quantities and quantity points
 *  * easy user customization through multi-file predicates
 **/

:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(library(error)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

%% alias(?AliasName, ?CanonicalName) is nondet.
%
%  Defines an alternative name (alias) for a canonical unit, quantity, or origin name.
%  This predicate is multifile, allowing users and other modules to define new aliases.
%
%  Examples:
%  ==
%  units:alias(my_meter, si:metre).
%  ==
%
%  @param AliasName The alternative name.
%  @param CanonicalName The original, canonical name of the unit, quantity, or origin.
:- multifile alias/2.

%% dimension_symbol(?DimensionName, ?Symbol) is nondet.
%
%  Defines a quantity dimension with its symbolic representation.
%  This predicate is multifile, allowing users and other modules to define new dimensions.
%
%  Examples:
%  ==
%  units:dimension_symbol(isq:dim_length, 'L').  % Length dimension
%  units:dimension_symbol(isq:dim_time, 'T').    % Time dimension
%  units:dimension_symbol(my_dim_currency, '$'). % Custom currency dimension
%  ==
%
%  @param DimensionName The name of the quantity dimension (e.g., `isq:dim_mass`).
%  @param Symbol The symbolic representation of the dimension (e.g., `'M'`).
:- multifile dimension_symbol/2.

%% kind(?KindName) is nondet.
%
%  Declares a `KindName` as a distinct "kind" of quantity.
%  This predicate is multifile, allowing users and other modules to define new kinds.
%
%  Base quantities (e.g., `isq:length`) are inherently kinds.
%  `kind/1` establishes other terms as distinct quantity kinds, often for specialization.
%  For example, `isq:angular_measure` is declared a `kind`.
%  Although its underlying dimension is `1` (dimensionless), this declaration makes it
%  distinct from generic dimensionless quantities.
%  Consequently, an angular measure (e.g., in radians) is not implicitly convertible
%  to a simple unitless number, enhancing type safety by requiring explicit conversion
%  if such a transformation is intended.
%
%  Examples:
%  ==
%  units:kind(isq:angular_measure).       % A specialized dimensionless quantity with unit si:radian.
%  units:kind(isq:solid_angular_measure). % A specialized dimensionless quantity with unit si:steradian.
%  units:kind(my_custom_fundamental_kind).% A user-defined kind.
%  ==
%
%  @param KindName The atom or compound term representing the kind of quantity.
:- multifile kind/1.
:- multifile quantity_character/2.

%% quantity_formula(?QuantityName, ?Formula) is nondet.
%
%  Associate a quantity `QuantityName` with a specific `Formula`.
%  This predicate is multifile, allowing users and other modules to define new derived quantities.
%
%  This is particularly important for child quantities that have a "constrained" definition;
%  for example, `isq:angular_measure` is specifically `isq:arc_length / isq:radius`.
%  Such definitions enable implicit conversions when the specific formula is matched.
%
%  Examples:
%  ==
%  units:quantity_formula(isq:thermodynamic_efficiency,(isq:work)/(isq:heat)).
%  ==
%
%  @param QuantityName The name of the derived quantity (e.g., `isq:thermodynamic_efficiency`).
%  @param Formula An expression representing how `QuantityName` is derived from other quantities
%                 (e.g., `isq:work/isq:heat`).
:- multifile quantity_formula/2.

%% quantity_parent(?ChildQuantity, ?ParentOrDimension) is nondet.
%
%  Defines a hierarchical relationship where `ChildQuantity` is a specialization of `ParentOrDimension`.
%  This predicate is multifile, allowing users and other modules to extend the quantity hierarchy.
%
%  Examples:
%  ==
%  units:quantity_parent(isq:length, isq:dim_length).         % isq:length is a base quantity with dimension isq:dim_length.
%  units:quantity_parent(isq:width, isq:length).              % isq:width is a kind of isq:length.
%  units:quantity_parent(isq:speed, isq:dim_length/isq:dim_time). % isq:speed's dimension is length/time.
%  ==
%
%  @param ChildQuantity The name of the child quantity (e.g., `isq:width`).
%  @param ParentOrDimension The name of the parent quantity (e.g., `isq:length`),
%                           a dimension name (e.g., `isq:dim_length`),
%                           or a derived quantity (e.g., `isq:length/isq:time`).
:- multifile quantity_parent/2.

%% absolute_point_origin(?OriginName, ?QuantityType) is nondet.
%
%  Declares an `OriginName` as an absolute reference point for a given `QuantityType`.
%  This predicate is multifile, allowing users and other modules to define new absolute origins.
%
%  Absolute origins serve as fundamental datums from which quantity points can be measured.
%  Unlike relative origins, they are not defined in terms of other origins.
%  The `QuantityType` specifies the kind of quantity for which this origin is a reference.
%  For example, `si:absolute_zero` is an absolute origin for `isq:thermodynamic_temperature`.
%  The special origin `0` is predefined as an absolute origin for any `QuantityType`
%  serving as the default origin for quantities that do not have a specific named origin.
%
%  Examples:
%  ==
%  units:absolute_point_origin(si:absolute_zero, isq:thermodynamic_temperature). % Absolute zero temperature.
%  units:absolute_point_origin(my_custom_epoch, isq:time).                      % A custom absolute time reference.
%  ==
%
%  @param OriginName The atom or compound term representing the name of the absolute origin.
%  @param QuantityType The quantity type for which this origin is a reference (e.g., `isq:thermodynamic_temperature`).
:- multifile absolute_point_origin/2.

%% no_space_before_unit_symbol(?UnitName) is nondet.
%
%  Declares that the symbol associated with a specific `UnitName`
%  should not be preceded by a space when a quantity is formatted (e.g., by `qformat/1`).
%  This predicate is multifile, allowing users and other modules to specify
%  formatting exceptions for unit symbols.
%
%  By default, `qformat/1` inserts a space between a numeric value and its
%  unit symbol. This predicate provides an override for units whose symbols
%  (like the degree symbol °) should directly follow the value.
%
%  Examples:
%  ==
%  units:no_space_before_unit_symbol(non_si:degree).
%  units:no_space_before_unit_symbol(non_si:arcminute).
%  units:no_space_before_unit_symbol(non_si:arcsecond).
%  ==
%
%  @param UnitName The unit name (an atom or compound term, e.g., `non_si:degree`)
%                  whose symbol should not have a preceding space.
:- multifile no_space_before_unit_symbol/1.

%% prefix(?PrefixName, ?Symbol, ?Factor) is nondet.
%
%  Defines a unit prefix, its symbol, and its numerical factor.
%  This predicate is multifile, allowing users and other modules to define new prefixes.
%
%  Prefixes are used to denote multiples or submultiples of units.
%  `PrefixName` is the full name of the prefix (e.g., `si:kilo`).
%  `Symbol` is the character(s) used to represent the prefix (e.g., `'k'`).
%  `Factor` is the numerical multiplier associated with the prefix (e.g., `1000`).
%
%  When a prefix is used with a unit (e.g., `si:kilo(si:metre)`), the library
%  combines the prefix's symbol with the unit's symbol (e.g., "km") and
%  applies the factor to the unit's value.
%  Note that the library won't recognize the concatenated name of the prefix and unit,
%  (e.g. `si:kilometre` for `si:kilo(si:metre)`.
%  If you want that, you can add an alias `units:alias(si:kilometre, si:kilo(si:metre))`.
%  The only predefined alias in the library is `si:kilogram` as it is a base SI unit.
%
%  Examples:
%  ==
%  units:prefix(si:kilo, 'k', 1000).
%  units:prefix(si:milli, 'm', 1/1000).
%  units:prefix(iec:kibi, 'Ki', 1024).
%  ==
%
%  @param PrefixName The full name of the prefix, often namespaced (e.g., `si:kilo`, `iec:mebi`).
%  @param Symbol The symbol for the prefix (e.g., `'k'`, `'M'`).
%  @param Factor The numerical factor the prefix represents (e.g., `1000`, `1024*1024`).
:- multifile prefix/3.

%% relative_point_origin(?OriginName, ?Offset) is nondet.
%
%  Declares `OriginName` as a symbolic name for a quantity point defined by the `Offset` expression.
%  The `Offset` expression itself evaluates to a specific point in a quantity's dimensional space.
%  This predicate is multifile, allowing users and other modules to define new named quantity points.
%
%  This predicate establishes that `OriginName` is equivalent to the point denoted by `Offset`.
%  The `Offset` expression should evaluate to a quantity point.
%
%  Examples:
%  ==
%  units:relative_point_origin(si:ice_point, point(273.15 * si:kelvin)).
%  ==
%
%  @param OriginName The atom or compound term representing the symbolic name of the quantity point.
%  @param Offset An expression that evaluates to a quantity point.
%                Typically `point(Quantity)` or `ExistingOrigin + Quantity`.
%                (e.g., `point(273.15*si:kelvin)` or `oa + 10*m`).
:- multifile relative_point_origin/2.

%% unit_kind(?UnitName, ?QuantityKind) is nondet.
%
%  Associates a `UnitName` with a specific `QuantityKind`.
%  This predicate is multifile, allowing users and other modules to define these associations.
%
%  The `QuantityKind` argument should be a term that represents a kind of quantity,
%  often defined using the `kind/1` predicate or being a base quantity.
%
%  Examples:
%  ==
%  units:unit_kind(si:metre, isq:length).                 % Metre is a unit for quantities of kind length.
%  ==
%
%  @param UnitName The name of the unit (e.g., `si:radian`, `si:metre`).
%  @param QuantityKind The specific kind of quantity this unit measures (e.g., `isq:angular_measure`, `isq:length`).
:- multifile unit_kind/2.

%% unit_origin(?UnitName, ?OriginName) is nondet.
%
%  Associates a `UnitName` with a specific `OriginName` to be used as its default
%  reference point when creating quantity points.
%  This predicate is multifile, allowing users and other modules to define these associations.
%
%  Certain units, particularly for quantities like temperature, imply a specific
%  origin that is not the absolute zero of the quantity scale. For example,
%  `si:degree_Celsius` measures temperature relative to the freezing point of water
%  (`si:ice_point`), whereas `si:kelvin` measures from absolute zero (`si:absolute_zero`).
%
%  When a quantity point is constructed using the `point/1` functor with a unit
%  that has an entry in `unit_origin/2`, the specified `OriginName` is used
%  as the origin of the resulting quantity point. If a unit has no `unit_origin/2`
%  entry, its default origin is `0`.
%
%  Examples:
%  ==
%  units:unit_origin(si:kelvin, si:absolute_zero). % Kelvin measures from absolute zero.
%  units:unit_origin(si:degree_Celsius, si:ice_point). % Celsius measures from the ice point.
%  % For `point(20 * si:degree_Celsius)`, the origin will be `si:ice_point`.
%  % For `point(20 * si:metre)`, the origin will be `0` (as metre has no specific unit_origin).
%  ==
%
%  @param UnitName The name of the unit (e.g., `si:degree_Celsius`).
%  @param OriginName The name of the origin associated with this unit (e.g., `si:ice_point`).
%                    This `OriginName` should be defined via `absolute_point_origin/2` or `relative_point_origin/2`.
:- multifile unit_origin/2.

%% unit_symbol(?UnitName, ?Symbol) is nondet.
%
%  Associates a base `UnitName` with its textual `Symbol`.
%  This predicate is multifile, allowing users and other modules to define symbols for new base units.
%
%  This predicate is intended for base units not defined by a formula
%  involving other units (which would use `unit_symbol_formula/3`). The `Symbol` is
%  used for formatting quantities (e.g., by `qformat/1`) and can also be used for
%  parsing input expressions.
%
%  The `UnitName` is the canonical, often namespaced, name of the unit.
%  The `Symbol` is an atom representing its common textual representation.
%
%  Examples:
%  ==
%  units:unit_symbol(si:metre, m).         % Metre symbol is 'm'.
%  units:unit_symbol(si:gram, g).          % Gram symbol is 'g'.
%  units:unit_symbol(currency:euro, '€').  % Custom Euro symbol.
%  ==
%
%  Note: For derived units (e.g., `si:newton`) or units defined as scaled versions of
%  others (e.g., `non_si:hour`), use `unit_symbol_formula/3` instead.
%
%  @param UnitName The canonical name of the base unit (e.g., `si:metre`, `isq:ampere`).
%  @param Symbol The textual symbol for the unit (e.g., `m`, `'A'`).
:- multifile unit_symbol/2.

%% unit_symbol_formula(?UnitName, ?Symbol, ?Formula) is nondet.
%
%  Defines a derived `UnitName`, its textual `Symbol`, and its defining `Formula`
%  in terms of other units.
%  This predicate is multifile, allowing users and other modules to define new derived units.
%
%  This is the primary way to introduce units that are not base units (which use `unit_symbol/2`).
%  The `Formula` expresses `UnitName` as a combination of other existing units
%  (base, prefixed, or other derived units) and arithmetic operators (`*`, `/`, `**`).
%  The `Symbol` is used for formatting quantities with this unit.
%
%  Examples:
%  ==
%  units:unit_symbol_formula(si:newton, 'N', si:kilogram * si:metre / si:second**2). % Newton (force)
%  units:unit_symbol_formula(si:hertz, 'Hz', 1 / si:second).                       % Hertz (frequency)
%  units:unit_symbol_formula(non_si:hour, h, 60 * non_si:minute).                  % Hour (time)
%  units:unit_symbol_formula(si:degree_Celsius, '℃', si:kelvin).                   % Degree Celsius (temperature, same scale as Kelvin but different origin)
%  ==
%
%  Note: For base units that are not derived from others, use `unit_symbol/2`.
%
%  @param UnitName The canonical name of the derived unit (e.g., `si:newton`, `non_si:hour`).
%  @param Symbol The textual symbol for the unit (e.g., `'N'`, `'h'`, `'℃'`).
%  @param Formula An expression defining the unit in terms of other units
%                 (e.g., `si:kilogram*si:metre/si:second**2`).
:- multifile unit_symbol_formula/3.

units:absolute_point_origin(0, _).
units:dimension_symbol(dim_1, '').
units:quantity_parent(1, dim_1).
units:unit_kind(1, 1).

:- use_module(units/utils).
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

%% qformat(+QuantityOrExpr) is det.
%
%  This predicate is a convenience wrapper around `qformat/2`.
%  It uses a default format (`"~p"`) for the numerical value of the quantity.
qformat(M) :-
   qformat("~p", M).

%% qformat(+ValueFormat, +QuantityOrExpr) is det.
%
%  Formats a quantity expression or an evaluated quantity term using a specific
%  `ValueFormat` for its numerical value, and prints the result to the current
%  output stream.
%
%  The output consists of the numerical value formatted according to `ValueFormat`,
%  followed by a space (unless suppressed by `no_space_before_unit_symbol/1`),
%  and then the symbol of the unit. If the unit is `1` (dimensionless),
%  no symbol or space is printed after the value.
%
%  Examples:
%  ==
%  ?- qformat("~2f", 1.23456 * si:metre).
%  1.23 m
%  true.
%
%  ?- qformat("~e", 12345 * si:pascal).
%  1.234500e+04 Pa
%  true.
%  ==
%
%  @param ValueFormat A Prolog format string for the numerical value part of the
%                     quantity (e.g., `"~2f"`, `"~e"`, `"~p"`).
%  @param QuantityOrExpr An expression that `qeval/1` can evaluate to a quantity,
%                        or an already evaluated quantity term.
qformat(VFormat, M) :-
   qeval(X is M),
   X = V*_[U],
   (  U == 1
   -> Symbol = "",
      Space = ""
   ;  mapexpr(unit, U, Symbol),
      (  aliased(units:no_space_before_unit_symbol(U))
      -> Space = ""
      ;  Space = " "
      )
   ),
   string_concat(VFormat, "~s~w", Format),
   format(Format, [V, Space, Symbol]).

%% error:has_type(+Type, @Term) is semidet.
%
%  Hook for library(error)'s must_be/2 predicate to validate quantity-related types.
%
%  This predicate defines type checking for:
%  * `quantity`: Succeeds if `Term` is a quantity (e.g., `Value*QuantityType[Unit]`).
%  * `quantity_point`: Succeeds if `Term` is a quantity point (e.g., `Origin+Quantity`).
%  * Specific quantity types (e.g., `isq:length`, `isq:speed`, `kind_of(isq:energy)`):
%    Succeeds if `Term` can be evaluated by qeval/1 and the resulting quantity
%    is implicitly convertible to the specified `Type`.
%
%  Examples:
%  ==
%  ?- qeval(X is 10*si:metre), must_be(quantity, X).
%  X = 10*kind_of(isq:length)[si:metre].
%
%  ?- qeval(X is 10*si:metre), must_be(isq:length, X).
%  X = 10*kind_of(isq:length)[si:metre].
%
%  ?- qeval(P is point(20*si:degree_Celsius)), must_be(quantity_point, P).
%  P = si:ice_point+20*kind_of(isq:thermodynamic_temperature)[si:degree_Celsius].
%
%  ?- qeval(X is 10*si:metre), must_be(isq:time, X). % Fails
%  ERROR: Type error: `isq:time' expected, found `10*kind_of(isq:length)[si:metre]'
%  ==
%
%  @param Type The expected type. Can be `quantity`, `quantity_point`, or a
%              specific quantity type atom/compound term recognized by `any_quantity/1`.
%  @param Term The term to check.
error:has_type(quantity, Term) :-
   !,
   catch(eval_(Term, R), _, fail),
   is_dict(R, q).
error:has_type(quantity_point, Term) :-
   !,
   catch(eval_(Term, R), _, fail),
   is_dict(R, qp).
error:has_type(Quantity, Term) :-
   ground(Quantity),
   any_quantity(Quantity),
   !,
   catch(eval_(Term, R), _, fail),
   (  is_dict(R, q)
   -> Q = R.q
   ;  is_dict(R, qp),
      Q = R.q.q
   ),
   implicitly_convertible(Q, Quantity).

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

is_of(_, X-_) :-
   \+ number(X),
   \+ X == pi.

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

select_(E, L, R) :-
   (  select(E, L, R)
   ;  L = R
   ).

common_factors(L1, R1, Type, L, N, L2, R2) :-
   exclude(ground, L1, Vars1),
   foldl(select_, Vars1, L2, _),
   exclude(ground, L2, Vars2),
   foldl(select_, Vars2, L1, _),
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
expand_factor(Type, Child-N, Factors) :-
   (  alias(Child, Parent)
   -> true
   ;  Type == unit
   -> unit(Child, _, Parent)
   ;  Type == quantity,
      child_quantity_parent(Child, Parent)
   ),
   parse_normalize_factors(Parent**N, Factors).

replace_arg1(A, B, M:T1, M:T2) :-
   T1 =.. [F, A | Args],
   T2 =.. [F, B | Args].

:- meta_predicate aliased(0).
:- table aliased/1.

aliased(Goal) :-
   call(Goal).
aliased(Goal) :-
   replace_arg1(A, B, Goal, Goal1),
   alias(A, B),
   aliased(Goal1).

:- meta_predicate lazy(0, ?).

lazy(Goal, Cond), \+ ground(Cond) =>
   when(ground(Cond), Goal).
lazy(Goal, _) =>
   call(Goal).

base_quantity(Quantity) :-
   quantity_parent(Quantity, Dimension),
   dimension_symbol(Dimension, _).

child_quantity_parent(Child, Parent) :-
   quantity_parent(Child, Parent),
   \+ dimension_symbol(Parent, _).

alias_quantity(Quantity) :-
   aliased(units:quantity_parent(Quantity, _)).

% derived quantity, with alias, lazy
any_quantity(Quantity) :-
   lazy(any_quantity_(Quantity), Quantity).
any_quantity_(kind_of(Kind)) =>
   mapexpr1(root_kind, [_]>>fail, Kind).
any_quantity_(Quantity) =>
   mapexpr1(alias_quantity, [_]>>fail, Quantity).

alias_quantity_formula(Quantity, Formula) :-
   aliased(units:quantity_formula(Quantity, Formula)).

derived(_*_).
derived(_/_).
derived(_**_).

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
   root_kind(Kind).
quantity_kind(Quantity, Kind) :-
   (  alias(Quantity, Parent)
   ;  child_quantity_parent(Quantity, Parent)
   ),
   quantity_kind(Parent, Kind).

derived_quantity_kind(Quantity, Kind) :-
   mapexpr(quantity_kind, [_, _]>>fail, Quantity, Kind).

:- table quantity_dimensions/2.

quantity_dimensions(Dimension, Dimension) :-
   dimension_symbol(Dimension, _).
quantity_dimensions(kind_of(Quantity), Dimension) :-
   quantity_dimensions(Quantity, Dimension).
quantity_dimensions(Quantity, Dimensions) :-
   aliased(units:quantity_parent(Quantity, Parent)),
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
   mapexpr(alias, NormalizedTo, AliasNormalizedTo),
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
   aliased(units:child_quantity_parent(ToKind, Formula)),
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

any_unit_symbol(Unit, Symbol) :-
   (  unit_symbol(Unit, Symbol)
   ;  unit_symbol_formula(Unit, Symbol, _)
   ).

has_prefix(Module:PrefixUnit, Symbol) :-
   prefix(Module:Prefix, PrefixSymbol, _),
   PrefixUnit =.. [Prefix, Unit],
   (  aliased(units:any_unit_symbol(Unit, UnitSymbol)),
      atom_concat(PrefixSymbol, UnitSymbol, Symbol)
   -> true
   ;  domain_error("has_prefix", Module:PrefixUnit-Symbol)
   ).

:- table prefix_unit_symbol_formula/3.

prefix_unit_symbol_formula(Module:PrefixUnit, Symbol, PrefixFormula*Unit) :-
   \+ compound(Symbol),
   prefix(Module:Prefix, PrefixSymbol, PrefixFormula),
   PrefixUnit =.. [Prefix, Unit],
   aliased(units:any_unit_symbol(Unit, UnitSymbol)),
   \+ has_prefix(Unit, UnitSymbol),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol).

:- table unit/3.

unit(U, S, F) :-
   alias(U, F),
   (  aliased(units:any_unit_symbol(F, S))
   ;  aliased(units:prefix_unit_symbol_formula(F, S, _))
   ).
unit(U, S, F) :-
   (  unit_symbol_formula(U, S, F)
   ;  prefix_unit_symbol_formula(U, S, F)
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
all_unit_kind_(Unit, R), derived(Unit) =>
   mapexpr(all_unit_kind_, [_, 1]>>true, Unit, Kind),
   normalize(Kind, NKind),
   normalize_kind(NKind, R).
all_unit_kind_(_, _) => fail.

normalize_kind_(kind_of(A)/kind_of(B), R) =>
   normalize(A/B, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)*kind_of(B), R) =>
   normalize(A*B, AB),
   R = kind_of(AB).
normalize_kind_(kind_of(A)**N, R) =>
   normalize(A**N, AN),
   R = kind_of(AN).
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
normalize_unit(U, R), derived(U) =>
   mapexpr(normalize_unit, [_, _]>>fail, U, R).
normalize_unit(_, _) => fail.

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit), unifiable(Unit1, Unit2, _) =>
   Unit1 = Unit2,
   NewFactor1 = 1,
   NewFactor2 = 1,
   NewUnit = Unit2.
common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) =>
   common_expr(unit, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

unit_origin_0(Unit, Origin) =>
   (  aliased(units:unit_origin(Unit, O))
   -> normalize_origin(O, Origin)
   ;  eval_(0*Unit, Q),
      Origin = qp{o: 0, q: Q}
   ).

:- table all_origin_/1.

all_origin_(Origin) :-
   (  absolute_point_origin(Origin, _)
   ;  relative_point_origin(Origin, _)
   ).

all_origin(Origin) :-
   lazy(aliased(units:all_origin_(Origin)), Origin).

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
   (  common_quantity(A.q, B.q, Q),
      same_kind(A.q, B.q)
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

%% qeval(+Expr) is det.
%
%  Evaluates an arithmetic expression `Expr` involving quantities, units, and quantity points.
%  This is the primary predicate for performing calculations and comparisons within the units library.
%  It handles all supported operators, conversions, and ensures dimensional consistency.
%
%  `Expr` can be:
%  * An assignment: `Result is SubExpr`.
%    - If `Result` is a variable, it is bound to the quantity or quantity point resulting from `SubExpr`.
%    - Example: `qeval(X is 3*si:metre + 50*si:centimetre)`
%  * A comparison: `A Op B` where `Op` is one of `=:=`, `=\=`, `<`, `=<`, `>`, `>=`.
%    - Both `A` and `B` are evaluated, and the comparison is performed.
%    - Requires `A` and `B` to have compatible quantity types and units.
%    - Example: `qeval(1*si:kilometre =:= 1000*si:metre)`
%  * A CLP(BNR) constraint: `{SubExpr}`.
%    - `SubExpr` is evaluated using CLP(BNR) arithmetic.
%    - Example: `qeval({X*si:metre =:= 10*si:foot})`
%  * A sequence of expressions: `(Expr1, Expr2, ...)`.
%
%  Supported sub-expressions within `Expr`:
%  * Basic arithmetic: `+A`, `-A`, `A+B`, `A-B`, `A*B`, `A/B`, `A**N` (or `A^N`).
%    - Operations respect quantity types and units.
%    - Addition/subtraction require compatible kinds.
%  * Conversions:
%    - `QExpr in UnitExpr`: Converts quantity `QExpr` to `UnitExpr`.
%      Example: `qeval(X is 1*si:foot in si:inch)`
%    - `QExpr as QuantityTypeExpr`: Casts quantity `QExpr` to `QuantityTypeExpr`.
%      Example: `qeval(X is 10*si:metre/si:second as isq:speed)`
%  * Quantity point operations:
%    - `point(QExpr)`: Creates a quantity point from quantity `QExpr`.
%      Origin is inferred from unit or defaults to `0`.
%      Example: `qeval(P is point(20*si:degree_Celsius))`
%    - `origin(OriginName)`: Refers to a defined origin.
%      Example: `qeval(P is si:ice_point + 5*si:kelvin)`
%    - `quantity_from_zero(PExpr)`: Vector from origin `0` to point `PExpr`.
%    - `quantity_from(PExpr, OriginExpr)`: Vector from `OriginExpr` to `PExpr`.
%    - `point_for(PExpr, NewOriginExpr)`: Represents point `PExpr` relative to `NewOriginExpr`.
%  * Disambiguation functors:
%    - `unit(U)`: Interprets `U` as a unit (e.g., `si:metre`).
%    - `quantity(Q)`: Interprets `Q` as a quantity (e.g., `V*Q[U]`).
%    - `quantity_point(QP)`: Interprets `QP` as a quantity point.
%  * Numeric values (e.g., `3`, `pi`, `random_float`) are treated as dimensionless quantities with unit `1`.
%  * Variables:
%    - On the left of `is`, bound to the result.
%    - Elsewhere, typically treated as CLP(BNR) variables of dimensionless quantity.
%
%  Quantities are represented as `Value * QuantityType[Unit]`.
%  Quantity points are represented as `Origin + Quantity`.
%
%  Examples:
%  ==
%  ?- qeval(Dist is 10 * si:kilo(si:metre) + 500 * si:metre).
%  Dist = 10500 * kind_of(isq:length)[si:metre].
%
%  ?- qeval(TempPoint is point(25 * si:degree_Celsius)).
%  TempPoint = si:zeroth_degree_Celsius+25*kind_of(isq:thermodynamic_temperature)[si:degree_Celsius].
%
%  ?- qeval(({Len_m * si:metre =:= Len_ft * foot}, Len_m =:= 1)).
%  Len_m = 1,
%  Len_ft = 1250r381.
%  ==
%
%  @param Expr The arithmetic expression to evaluate.
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
eval_(as(Expr, Quantity), R), any_quantity(Quantity) =>
   eval_(Expr, M),
   (  is_dict(M, qp)
   -> eval_(as(M.q, Quantity), Q),
      R = M.put([q=Q])
   ;  (  implicitly_convertible(M.q, Quantity)
      -> R = M.put(q, Quantity)
      ;  domain_error(M.q, Quantity)
      )
   ).
eval_(force_as(Expr, Quantity), R), any_quantity(Quantity) =>
   eval_(Expr, M),
   (  is_dict(M, qp)
   -> eval_(force_as(M.q, Quantity), Q),
      R = M.put([q=Q])
   ;  (  explicitly_convertible(M.q, Quantity)
      -> R = M.put(q, Quantity)
      ;  domain_error(M.q, Quantity)
      )
   ).
eval_(cast(Expr, Quantity), R), any_quantity(Quantity) =>
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
eval_(origin(Origin), R), all_origin(Origin) =>
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
eval_(Origin, R), all_origin(Origin) =>
   normalize_origin(Origin, R).

eval_q(quantity(Q), R), any_quantity(Q) =>
   R = Q.
eval_q(X, R), any_quantity(X) =>
   R = X.

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

test('explicitly_convertible2', [forall(explicitly_convertible_data(Q1, Q2))]) :-
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
   avg_speed(isq:height[inch], quantity Time, isq:speed[m/si:hour]),
   Time = _*Q[U],
   Q == isq:time,
   U == si:hour.

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

test('radian', [error(_)]) :-
   qeval(_ is m/m in angular:radian).

test('qformat', [
   forall(qformat_data(Expr, ExpectedString))
]) :-
   qeval(QuantityDict is Expr),
   with_output_to(string(FormattedString), qformat(QuantityDict)),
   assertion(FormattedString == ExpectedString).

qformat_data(10 * si:metre, "10 m").
qformat_data(25 * si:degree, "25°"). % Note: no_space_before_unit_symbol
qformat_data(5 * si:newton, "5 N").
qformat_data(1.5 * si:kilo(si:hertz), "1.5 kHz").
qformat_data(10, "10"). % Dimensionless with unit 1
qformat_data(10 * (si:metre/si:second), "10 m/s").

test(has_type_examples) :-
   qeval(X_len is 10*si:metre),
   must_be(quantity, X_len),
   must_be(isq:length, X_len),
   qeval(P_temp is point(20*si:degree_Celsius)),
   must_be(quantity_point, P_temp).

test(has_type_fail_example, [error(type_error(isq:time, _))]) :-
   qeval(X_len is 10*si:metre),
   must_be(isq:time, X_len).

:- end_tests(units).
