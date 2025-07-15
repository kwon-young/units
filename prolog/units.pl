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
   % quantity_character/2,
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
:- use_module(units/utils).
:- use_module(units/search).
:- use_module(units/quantity).
:- use_module(units/unit_defs).

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
units:unit_symbol(1, 1).

:- use_module(units/systems/isq).

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
      (  aliased(no_space_before_unit_symbol(U))
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
   catch(eval_(no, Term, R), _, fail),
   is_dict(R, q).
error:has_type(quantity_point, Term) :-
   !,
   catch(eval_(no, Term, R), _, fail),
   is_dict(R, qp).
error:has_type(Quantity, Term) :-
   ground(Quantity),
   any_quantity(Quantity),
   !,
   catch(eval_(no, Term, R), _, fail),
   (  is_dict(R, q)
   -> Q = R.q
   ;  is_dict(R, qp),
      Q = R.q.q
   ),
   implicitly_convertible(Q, Quantity).

unit_origin_0(M, Module:PrefixUnit, Origin),
      PrefixUnit =.. [Prefix, Unit],
      prefix(Module:Prefix, _, _) =>
   unit_origin_0(M, Unit, Origin).
unit_origin_0(M, Unit, Origin) =>
   (  aliased(unit_origin(Unit, O))
   -> normalize_origin(M, O, Origin)
   ;  eval_(M, 0*Unit, Q),
      Origin = qp{o: 0, q: Q}
   ).

:- table all_origin_/1.

all_origin_(Origin) :-
   (  absolute_point_origin(Origin, _)
   ;  relative_point_origin(Origin, _)
   ).

all_origin(Origin) :-
   lazy(aliased(all_origin_(Origin)), Origin).

normalize_origin(M, Origin, qp{o: Origin, q: Q}) :-
   when(ground(Origin), normalize_origin_(M, Origin, Q)),
   eval_(M, 0*_[_], Q).
normalize_origin_(M, Origin, Q), unit_origin(Unit, Origin) =>
   eval_(M, Unit, R),
   Q = R.put([v=0]).
normalize_origin_(M, Origin, Q), absolute_point_origin(Origin, Quantity) =>
   eval_(M, Quantity[_], R),
   Q = R.put([v=0]).
normalize_origin_(M, Origin, Q), relative_point_origin(Origin, Expr) =>
   eval_(M, Expr, QP),
   Q = QP.q.put([v=0]).
normalize_origin_(M, Alias, Q), alias(Alias, Origin) =>
   normalize_origin_(M, Origin, QP),
   Q = QP.q.

common_origin(O1, F1, O2, F2, O) :-
   once(iterative_deepening(1, common_origin_(O1, F1, O2, F2, O))).

common_origin(O, F, O, F, O, _-N) :-
   setarg(1, N, no),
   eval_(no, 0*_[_], F).
common_origin(O1, F1, O2, F2, O, N) :-
   relative_point_origin(O1, Expr),
   eval_(no, Expr, R),
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

comparable(M, AB, R) :-
   AB =.. [Op, A, B],
   eval_(M, B, B1),
   is_dict(B1, BTag),
   (  Op == is, var(A)
   -> comparable_is(A, BTag:B1, R)
   ;  eval_(M, A, A1),
      is_dict(A1, ATag),
      comparable(M, Op, ATag:A1, BTag:B1, R)
   ).
comparable(M, is, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, _)
   -> comparable(M, A.q is (F2 + B.q) - F1, RQ),
      R = A.put([q=RQ])
   ;  domain_error(A.o, B.o)
   ).
comparable(M, =:=, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, O)
   -> comparable(M, A.q + F1 =:= F2 + B.q, RQ),
      R = qp{o: O, q: RQ}
   ;  domain_error(A.o, B.o)
   ).
comparable(M, =\=, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, O)
   -> comparable(M, A.q + F1 =\= F2 + B.q, RQ),
      R = qp{o: O, q: RQ}
   ;  domain_error(A.o, B.o)
   ).
comparable(M, -, qp:A, qp:B, R) =>
   (  common_origin(A.o, F1, B.o, F2, _)
   -> comparable(M, (F1 + A.q) - (F2 + B.q), R)
   ;  domain_error(A.o, B.o)
   ).
comparable(M, -, qp:A, q:B, R) =>
   comparable(M, A.q - B, Q),
   R = A.put([q=Q]).
comparable(M, +, qp:A, q:B, R) =>
   comparable(M, +, q:(A.q), q:B, RQ),
   R = A.put([q=RQ]).
comparable(M, +, q:A, qp:B, R) =>
   comparable(M, +, qp:B, q:A, R).
comparable(_, Op, q:A, q:B, R) =>
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

:- module_transparent(qeval/1).
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
qeval(@(Goal)) =>
   call(Goal).
qeval(Expr) =>
   context_module(M),
   eval_(M, Expr, Q),
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

eval_(M, {ExprIn}, R) =>
   eval_(M, ExprIn, ExprOut),
   R = ExprOut.put(v, {ExprOut.v}).
eval_(M, Result is ExprIn, R) =>
   comparable(M, Result is ExprIn, R).
eval_(M, +A, R) =>
   eval_(M, A, A1),
   R = A1.put(v, +A1.v).
eval_(M, -A, R) =>
   eval_(M, A, A1),
   R = A1.put(v, -A1.v).
eval_(M, A+B, R) =>
   comparable(M, A+B, R).
eval_(M, A-B, R) =>
   comparable(M, A-B, R).
eval_(M, A=:=B, R) =>
   comparable(M, A=:=B, R).
eval_(M, A=\=B, R) =>
   comparable(M, A=\=B, R).
eval_(M, A<B, R) =>
   comparable(M, A<B, R).
eval_(M, A>B, R) =>
   comparable(M, A>B, R).
eval_(M, A=<B, R) =>
   comparable(M, A=<B, R).
eval_(M, A>=B, R) =>
   comparable(M, A>=B, R).
eval_(M, A*B, R) =>
   eval_(M, A, A1),
   eval_(M, B, B1),
   normalize_kind(A1.q*B1.q, Q),
   normalize(A1.u*B1.u, U),
   normalize(A1.v*B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(M, A/B, R) =>
   eval_(M, A, A1),
   eval_(M, B, B1),
   normalize_kind(A1.q/B1.q, Q),
   normalize(A1.u/B1.u, U),
   normalize(A1.v/B1.v, V),
   R = q{v: V, q: Q, u: U}.
eval_(M, A**N, R) =>
   eval_(M, A, A1),
   normalize_kind(A1.q**N, Q),
   normalize(A1.u**N, U),
   normalize(A1.v**N, V),
   R = q{v: V, q: Q, u: U}.
eval_(M, A^N, R) =>
   eval_(M, A**N, R).
eval_(M, in(Expr, Unit), R) =>
   eval_(M, Expr, R1),
   (  is_dict(R1, qp)
   -> eval_(M, in(R1.q, Unit), Q),
      R = R1.put([q=Q])
   ;  eval_(M, Unit, Q),
      (  implicitly_convertible(R1.q, Q.q)
      -> common_unit(R1.u, F1, Q.u, F2, _),
         normalize(R1.v*F1/F2, V),
         R = q{v: V, q: R1.q, u: Q.u}
      ;  domain_error(R1.q, Q.q)
      )
   ).
eval_(M, as(Expr, Quantity), R), any_quantity(Quantity) =>
   eval_(M, Expr, R1),
   (  is_dict(R1, qp)
   -> eval_(M, as(R1.q, Quantity), Q),
      R = R1.put([q=Q])
   ;  (  implicitly_convertible(R1.q, Quantity)
      -> R = R1.put(q, Quantity)
      ;  domain_error(R1.q, Quantity)
      )
   ).
eval_(M, force_as(Expr, Quantity), R), any_quantity(Quantity) =>
   eval_(M, Expr, R1),
   (  is_dict(R1, qp)
   -> eval_(M, force_as(R1.q, Quantity), Q),
      R = R1.put([q=Q])
   ;  (  explicitly_convertible(R1.q, Quantity)
      -> R = R1.put(q, Quantity)
      ;  domain_error(R1.q, Quantity)
      )
   ).
eval_(M, cast(Expr, Quantity), R), any_quantity(Quantity) =>
   eval_(M, Expr, R1),
   (  is_dict(R1, qp)
   -> eval_(M, cast(R1.q, Quantity), Q),
      R = R1.put([q=Q])
   ;  (  common_quantity(R1.q, Quantity, _)
      -> R = R1.put(q, Quantity)
      ;  domain_error(R1.q, Quantity)
      )
   ).
eval_(_, pi, R) =>
   R = q{v: pi, q: 1, u: 1}.
eval_(_, random_float, R) =>
   R = q{v: random_float, q: 1, u: 1}.
eval_(M, unit(X), R), normalize_unit(M, X, U) =>
   when(ground(U), all_unit_kind(U, UKind)),
   when((ground(UKind), ground(Q)), implicitly_convertible(UKind, Q)),
   R = q{v: 1, q: Q, u: U}.
eval_(M, quantity(Quantity), R) =>
   Quantity = _*_[_],
   eval_(M, Quantity, R).
eval_(M, QuantityExpr[UnitExpr], R) =>
   eval_q(QuantityExpr, Q),
   eval_(M, unit(UnitExpr), Unit),
   (  implicitly_convertible(Unit.q, Q)
   -> true
   ;  domain_error(Unit.q, Q)
   ),
   R = Unit.put([q=Q]).
eval_(M, point(Expr), R) =>
   eval_(M, Expr, Q),
   unit_origin_0(M, Q.u, Origin),
   R = Origin.put([q=Q]).
eval_(M, quantity_point(QP), R) =>
   QP = O + Q,
   (  var(O)
   -> Origin = origin(O)
   ;  Origin = O
   ),
   (  var(Q)
   -> Quantity = quantity(Q)
   ;  Quantity = Q
   ),
   eval_(M, Origin + Quantity, R).
eval_(M, origin(Origin), R), all_origin(Origin) =>
   normalize_origin(M, Origin, R).
eval_(M, exp(Expr), R) =>
   eval_(M, Expr in 1, R1),
   R = R1.put([v=exp(R1.v)]).
eval_(M, sin(Expr), R), fail_call(M:sin, Radian) =>
   eval_(M, Expr in Radian, R1),
   R = q{v: sin(R1.v), q: 1, u: 1}.
eval_(M, cos(Expr), R), fail_call(M:cos, Radian) =>
   eval_(M, Expr in Radian, R1),
   R = q{v: cos(R1.v), q: 1, u: 1}.
eval_(M, tan(Expr), R), fail_call(M:tan, Radian) =>
   eval_(M, Expr in Radian, R1),
   R = q{v: tan(R1.v), q: 1, u: 1}.
eval_(M, asin(Expr), R), fail_call(M:asin, Radian) =>
   eval_(M, Expr in 1, R1),
   eval_(M, Radian, R2),
   R = R2.put([v=asin(R1.v)]).
eval_(M, acos(Expr), R), fail_call(M:acos, Radian) =>
   eval_(M, Expr in 1, R1),
   eval_(M, Radian, R2),
   R = R2.put([v=acos(R1.v)]).
eval_(M, atan(Expr), R), fail_call(M:atan, Radian) =>
   eval_(M, Expr in 1, R1),
   eval_(M, Radian, R2),
   R = R2.put([v=atan(R1.v)]).
eval_(M, atan2(A, B), R), fail_call(M:atan2, Radian) =>
   eval_(M, A, RA1),
   eval_(M, B, RB1),
   (  common_quantity(RA1.q, RB1.q, _)
   -> (  common_unit(RA1.u, _, RB1.u, _, U)
      -> eval_(M, RA1 in U, RA2),
         eval_(M, RB1 in U, RB2)
      ;  domain_error(RA1.u, RB1.u)
      )
   ;  domain_error(RA1.q, RB1.q)
   ),
   eval_(M, Radian, R2),
   R = R2.put([v=atan2(RA2.v, RB2.v)]).
eval_(M, quantity_from_zero(Expr), R) =>
   eval_(M, Expr - origin(0), R).
eval_(M, quantity_from(Expr, Origin), R) =>
   eval_(M, Expr, R1),
   eval_(M, R1 - Origin in R1.q.u, R).
eval_(M, point_for(Expr, Origin), R) =>
   (  (var(Origin) ; Origin = 0)
   -> O = origin(Origin)
   ;  O = Origin
   ),
   eval_(M, O + quantity_from(Expr, Origin), R).
eval_(_, X, R), var(X) =>
   R = q{v: X, q: 1, u: 1}.
eval_(_, Q, R), is_dict(Q, q) =>
   R = Q.
eval_(_, N, R), number(N) =>
   R = q{v: N, q: 1, u: 1}.
eval_(M, UnitOrSymbol, R), ground(UnitOrSymbol), normalize_unit(M, UnitOrSymbol, Unit) =>
   all_unit_kind(Unit, Kind),
   R = q{v: 1, q: Kind, u: Unit}.
eval_(_, Point, R), is_dict(Point, qp) =>
   R = Point.
eval_(M, Origin, R), all_origin(Origin) =>
   normalize_origin(M, Origin, R).

eval_q(quantity(Q), R), any_quantity(Q) =>
   R = Q.
eval_q(X, R), any_quantity(X) =>
   R = X.
