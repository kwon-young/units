# Units: Quantity and Units library for swi-prolog

Units is a quantity and units library modeled after [`mp-units`](https://mpusz.github.io/mp-units/latest/).

The key features are:

* `qeval/1` wrapper predicate for all arithmetic with units and quantities
* Large amount of predefined units and quantities
* safe arithmetic with units, quantities and quantity points
* easy user customization through multi-file predicates

## Installation

You can install this pack with:

```bash
$ swipl pack install units
```

### Dependencies

This library depends on:

* swi-prolog >= 9.3.23
* clpBNR

## Quick Start

To use this library, just wrap all your arithmetic in the `qeval/1` predicate.
Use multiplication `*` to create quantities:

```prolog
?- use_module(library(units)).
?- qeval(X is 3*si:metre).
X = 3*kind_of(isq:length)[si:metre].
```

## Quantity

A quantity is a concrete amount of a unit.
It is modeled in the library with the following term: `Amount * QuantityType[Unit]`.

* `Amount` should be a number: 1, 20 or `pi`. Anything that can be used with regular prolog arithmetic.
* `Unit` should be a unit of the shape `System:UnitName`, e.g. `si:metre` or `si:second`.
  This normalized form is composed of two parts:
    * `System` should be an atom denoting the system of units used: `si` or `international`
    * `UnitName` should be an atom denoting the unit in the system. `1` denotes that the amount is unitless.
* `QuantityType` should be a quantity type of the shape `System:QuantityName`, e.g. `isq:speed` or `isq:length`.
  The quantity name `1` denotes that the amount is dimensionless.

For example, a speed of `3 metre/second` would be represented as `3 * isq:speed[si:metre/si:second]`.

> :warning: `3*isq:speed[si:metre/si:second]` is a quantity and `isq:speed` is a quantity **type**.

To create a quantity, you can multiply a number with a predefined unit:

```prolog
?- use_module(library(units)).

?- qeval(Q is 3 * si:metre / si:second).
Q = 3*kind_of(isq:length/isq:time)[si:metre/si:second].
```

As you can see, `Q` is a quantity of amount 3, unit `si:metre/si:second` and quantity type `kind_of(isq:length/isq:time)`.
This quantity type was derived from the units used in the expression:

* `si:metre` can be any *kind of* length
* `si:second` can be any *kind of* time

For convenience, the same units can be used without their system (the `si:` prefix) or with their symbol (`m` for metre and `s` for second):

```prolog
?- use_module(library(units/systems/si/symbols)).
?- qeval(Q is 3 * m / second).
Q = 3*kind_of(isq:length/isq:time)[si:metre/si:second].
```

Since a lot of units from different systems share the same symbol (or name), the use of symbols is enabled by importing sub modules `units/systems/{System}/symbols`.
This will introduce a predicate per symbol and unit name in the current context module.
These predicates will be used to disambiguate the use of symbols, as well as make sure that no duplicate symbols can be used in the same context.

For example, importing symbols for both `si` and `usc` systems will result in a number of predicate name collisions:

```prolog
?- use_module(library(units/systems/si/symbols)).
?- use_module(library(units/systems/usc/symbols)).
...
_symbol)
ERROR: import/1: No permission to import usc_symbol:(ft/1) into user (already imported from si_symbol)
ERROR: import/1: No permission to import usc_symbol:(pk/1) into user (already imported from si_symbol)
...
true.
```

You can resolve conflicts by using the [`use_module/2`](https://www.swi-prolog.org/pldoc/doc_for?object=use_module/2) directive by including, excluding or renaming predicates:

```prolog
?- use_module(library(units/systems/si/symbols), except([ft/1])).
?- use_module(library(units/systems/usc/symbols), [ft/1, pk/1 as mypk]).
true.
?- qeval(X is ft), qeval(Y is mypk).
X = 1*kind_of(isq:length)[usc:foot],
Y = 1*kind_of(isq:length**3)[usc:peck].

> :warning: Be aware that importing a symbol module will introduce a **lot** of short named predicates.
> This can potentionally cause naming collision with your own code.
> Therefore, when using symbols in code, try to import only the symbol used and no more.
> Even better is to avoid symbols in code altogether, and only used symbols for oneoff
> interactive queries on the top level.

Quantities of the same kind can be added, subtracted and compared:

```prolog
?- qeval(1 * km + 50 * m =:= 1050 * m).
true.
```

Quantities of various kind can be multiplied or divided:

```prolog
?- qeval(140 * km / (2 * hour) =:= 70 * km/hour).
true
```

Here is a quick preview of what is possible:

```prolog
:- use_module(library(units)).

% use multiplication to associate
?- qeval(10*km =:= 2*5*km).

% conversions to common units
?- qeval(1 * h =:= 3600 * s).
?- qeval(1 * km + 1 * m =:= 1001 * m).

% derived quantities
?- qeval(1 * km / (1 * s) =:= 1000 * m / s).
?- qeval(2 * km / h * (2 * h) =:= 4 * km).
?- qeval(2 * km / (2 * km / h) =:= 1 * h).

?- qeval(2 * m * (3 * m) =:= 6 * m**2).

?- qeval(10 * km / (5 * km) =:= 2).

?- qeval(1000 / (1 * s) =:= 1 * kHz).

% assignment and comparison 
?- qeval(A is 10*m), qeval(A < 20*km).
A = 10 * kind(isq:length)[si:metre].
```

### What are quantity types ?

If you have already used units in a scientific context before, you will probably have heard of [**dimensional analysis**](https://w.wiki/_ohbG).
The goal of dimensional analysis is to check the correctness of your expression.
The process is to reduce units to their base quantities (or dimensions):

* `3 * metre/second` has the dimensions `Length**1 * Time**-1`

```prolog
?- qeval(X*Q[U] is 3*m/s), units:quantity_dimensions(Q, D).
X = 3,
Q = kind_of(isq:length/isq:time),
U = si:metre/si:second,
D = isq:dim_length/isq:dim_time.
```

Once this is done, you can check the correctness of your expression.
For example, addition, subtraction or comparison should be done on expression with the exact same dimensions:

* you can add `1 * metre/second + 1 * inch/hour`
* you can't add `1 * metre/second + 1 * metre * second`

```prolog
?- qeval(X is 1*metre/second + 1*inch/hour).
X = 1.0000070555555556*kind_of(isq:length/isq:time)[si:metre/si:second].
?- qeval(X is 1*metre/second + 1*metre*second).
ERROR: Domain error: `kind_of(isq:length/isq:time)' expected, found `kind_of(isq:length*isq:time)'
```

While this is a very good system to check the correctness of your expression, it is a bit simplistic for various reasons.

Let's say you want to compute the aspect ratio of a rectangle:

```prolog
rect_ratio(Width, Height, Ratio) :-
  qeval(Ratio is Width / Height).
```

Let's try and use this predicate with a rectangle of width `1*metre` and height `2*metre`.

```prolog
?- Width = 1*metre, Height = 2*metre,
   rect_ratio(Width, Height, Ratio).
Width = 1*metre,
Height = 2*metre,
Ratio = 0.5*kind_of(1)[1].
```

But, what if we made a mistake and inverted width with height ?

```prolog
?- Width = 1*metre, Height = 2*metre,
rect_ratio(Height, Width, Ratio).
Width = 1*metre,
Height = 2*metre,
Ratio = 2*kind_of(1)[1].
```

We get a wrong aspect ratio with no indication that something is wrong.
This is because `Width` and `Height` are both a kind of `isq:length` here.
Therefore, we can't detect that something is wrong.

By using strong quantities, we can improve the safety of this code:

```prolog
rect_ratio(Width, Height, Ratio) :-
  qeval(Ratio is (Width as isq:width) / (Height as isq:height)).

?- Width = 1*isq:width[metre], Height = 2*isq:height[metre],
   rect_ratio(Width, Height, Ratio).
Width = 1*isq:width[metre],
Height = 2*isq:height[metre],
Ratio = 0.5*(isq:width/isq:height)[1].

?- Width = 1*isq:width[metre], Height = 2*isq:height[metre],
   rect_ratio(Height, Width, Ratio).
ERROR: Domain error: `isq:height' expected, found `isq:width'
```

Similar to the International System of Units (SI), there is the International System of Quantities (ISQ) that defines what are the 7 basic quantities, as well as hundreds of other related quantities.
These new quantities forms a complex interconnected graph that defines which quantities are compatible and which are not.

Here is a interesting use of the `speed` quantity that can be use to generically describe the ratio of any type of `length` by any type of `time`:

```prolog
avg_speed(Distance, Time, Speed) :-
   qeval(S is Distance / Time as isq:speed).

?- avg_speed(220 * si:kilo(metre), 2 * si:hour, Speed),
   qmust_be(isq:speed[si:kilo(metre)/si:hour], Speed).
Speed = 110 * isq:speed[si:kilo(metre)/si:hour].

?- avg_speed(220 * isq:height[inch], 2 * si:second, Speed).
Speed = 110 * isq:speed[international:inch/si:second].

?- avg_speed(220 * si:gram, 2 * si:second, Speed).
ERROR: Domain error: `kind(isq:mass)/kind(isq:time)' expected, found `isq:speed'
```

The `qmust_be/2` predicate can be used to check the quantity and unit of a result.

## Quantity Points

Some quantities like temperature requires a specific origin from where we measure a displacement.
For example, `si:kelvin` measures temperatures from the absolute zero and works like any other units (like metre or second).
Although `si:degree_Celsius` are the same as `si:kelvin` (`1*degree_Celsius =:= 1*kelvin`), they measure temperatures from different origins: `si:kelvin` from the absolute zero and `si:degree_Celsius` from the ice freezing point.

To model this, the original `mp-units` library introduced the notion of quantity points.
In this system, quantities are actually quantity **vectors**, meaning they represent a quantity change, or a displacement of some sort.
To represent a measurement, we also need to represent origins: the ice freezing point for degree Celsius or the sea level for altitude.

Therefore, A quantity point is the combination of an **origin** and a quantity **vector**.
This library model them with the term `Origin + Quantity`.
Origins are terms similar to units and can be either:
* absolute: `si:absolute_zero` is not defined in term of something else, it is absolute
* relative: `si:zeroth_degree_Celsius` is defined as a point relative to `si:absolute_zero`, it is relative
* 0 is the default origin of most units which don't have special origins

Quantity points restrict the type of operation you can do with them.
You can't:

* add two points
* subtract a point from a vector
* multiply nor divide point with anything else

## Exported predicates and supported arithmetic expression

* Exported predicates:
  * `qeval(Expr)`: Evaluates the arithmetic expression `Expr` involving quantities, units, and quantity points. This is the primary predicate for performing calculations with the library. It handles all supported operators and conversions. For example, `qeval(X is 3*metre + 50*centimetre)` will bind `X` to the resulting quantity.
  * `qformat(Quantity)`: Formats the given `Quantity` into a human-readable string using symbols for units.

Here are the list of supported arithmetic expressions for quantities:

* `R is Expr`: If `R` is a variable, it is interpreted as a **quantity** `V*Q[U]`
* Comparison operators, `A` and `B` should have compatible quantity type and units:
  * `A =:= B`: equality
  * `A =\= B`: inequality
  * `A < B`: less than
  * `A =< B`: less than or equal
  * `A > B`: greater than
  * `A >= B`: greater than or equal
* Unary operator
  * `+A` and `-A`
* Binary operator
  * `A + B`, `A - B`: `A` and `B` should have compatible quantity type and units
  * `A * B`
  * `A / B`
  * `A ** N` or `A ^ N`: `N` should be a number
* Conversion predicates
  * `Quantity in Unit`: convert the unit of `Quantity` into `Unit`, e.g. `metre in inch`
  * `Quantity as QuantityType`: convert the quantity type of `Quantity` into `QuantityType`, e.g. `metre/second as isq:speed`
* Disambiguation functor
  * `quantity Q`: `Q` will be interpreted as a **quantity**, e.g. `V*Q[U]`
  * `unit U`: `U` will be interpreted as a **unit**, i.e. `metre`
  * `V`: variables are interpreted as a **value** (except for the left hand side of `is`)
* Quantity point specific predicates:
  * `point(Expr)`: Interprets `Expr` as a quantity and creates a quantity point. The origin is inferred from the unit of `Expr` if defined (e.g. `degree_Celsius`), otherwise it defaults to `0`. Example: `point(10 * metre)` results in `0 + 10 * kind_of(isq:length)[si:metre]`. `point(20 * degree_Celsius)` results in `si:zeroth_degree_Celsius + 20 * kind_of(isq:thermodynamic_temperature)[si:degree_Celsius]`.
  * `quantity_point(QP)`: Ensures `QP` is treated as a quantity point, e.g. `Origin + Quantity`.
  * `origin(O)`: Interprets `O` as an origin, i.e. `si:ice_point`
  * `quantity_from_zero(Expr)`: Computes the quantity vector from the default origin `0` to the point `Expr`. This is equivalent to `Expr - origin(0)`.
  * `quantity_from(Expr, Origin)`: Computes the quantity vector from a given `Origin` to the point `Expr`. This is equivalent to `Expr - Origin`.
  * `point_for(Expr, NewOrigin)`: Represents the point `Expr` from the perspective of `NewOrigin`. For example, if `Expr` is `OriginA + QV_A`, this predicate calculates `QV_B` such that `NewOrigin + QV_B` is the same absolute point as `Expr`.

### Type Checking with `must_be/2`

This library integrates with SWI-Prolog's `library(error)` to allow type checking using `must_be/2`.
You can use `must_be/2` to assert that a term is a specific quantity type.

For example, to ensure a variable `Speed` is indeed a quantity of type `isq:speed`:
```prolog
?- use_module(library(error)).
?- qeval(Speed is 10 * m/s), must_be(isq:speed, Speed).
Speed = 10*kind_of(isq:length/isq:time)[si:metre/si:second].
```

If the term is not of the expected quantity type, `must_be/2` will throw an error:
```prolog
?- qeval(Dist is 10 * m), must_be(isq:speed, Dist).
ERROR: Type error: `isq:speed' expected, found `10*kind_of(isq:length)[si:metre]' (a compound)
```
You can also check for a generic quantity (any kind) using `quantity`:
```prolog
?- qeval(X is 3*m), must_be(quantity, X).
X = 3*kind_of(isq:length)[si:metre].
```

## clpBNR support

One peculiar feature is that this library also supports clpBNR arithmetic:

```prolog
?- qeval({A*metre =:= B*inch}), A = 1.
A = 1,
B = 5000r127.
```

You can wrap your expression in `{}/1` to explicitly use clpBNR.
The library will also default to clpBNR if the expression is not sufficiently ground to use traditional arithmetic:

```prolog
% be default, a variable on the left hand side of `is` is
% interpreted as a quantity
?- qeval(Speed is L*metre / T*second).
Speed = _A*kind_of(isq:length*isq:time)[si:metre*si:second],
::(_A, real(-1.0Inf, 1.0Inf)),
::(L, real(-1.0Inf, 1.0Inf)),
::(T, real(-1.0Inf, 1.0Inf)).

% when using equality `=:=`, you need to disambiguate between
% a clpBNR variable and a variable that should be bound to a quantity
?- qeval(quantity(Speed) =:= L*metre / T*second).
Speed = _A*kind_of(isq:length*isq:time)[si:metre*si:second],
::(_A, real(-1.0Inf, 1.0Inf)),
::(L, real(-1.0Inf, 1.0Inf)),
::(T, real(-1.0Inf, 1.0Inf)).
```

One can also use a variable for units by wrapping it with `unit(Variable)`:

```prolog
?- qeval(Amount*unit(Unit) =:= 3*metre).
Amount = 3,
Unit = si:metre.
```

This means you can write unit aware true relational predicates:

```prolog
avg_speed(Distance, Time, Speed) :-
  qeval(Speed =:= Distance / Time as isq:speed).

?- avg_speed(m, s, quantity(Speed)). % traditional forward mode
Speed = 1*isq:speed[si:metre/si:second].
?- avg_speed(quantity(Distance), s, m/s). % "backward" mode
Distance = 1*isq:length[si:metre].
?- avg_speed(X*inch, hour, m/s). % overspecified units
X = 18000000r127.
?- avg_speed(quantity(Distance), quantity(Time), quantity(Speed)). % underspecified units
Distance = _A*isq:length[_B],
Time = _C*isq:time[_D],
Speed = _E*isq:speed[_F],
... % lots of constraints
```

## List of quantities and units

Here is an exhaustive list of [quantities](Quantities.md) and [units](Units.md) that you can use out of the box with this library.

You will also find a hierarchical graph representation of quantities in [quantities.pdf](quantities.pdf)

## User customization

You can extend the library by defining your own custom units and quantities.
This is achieved by adding clauses to the library's multifile predicates. The file [currency.pl](examples/currency.pl) file provides a practical example of how to do this.

Here's a breakdown of the key predicates you'll use:

1.  **Define a new quantity dimension**:
    Use `units:quantity_dimension(QuantityName, Symbol)` to declare a new base quantity and its symbol. For example, to define `currency` with symbol `'$'`:
    ```prolog
    units:quantity_dimension(currency, '$').
    ```

2.  **Define new unit**:
    Use `units:unit_symbol(UnitName, Symbol)` to associate a unit name with its display symbol. For example, for euros and US dollars:
    ```prolog
    units:unit_symbol(euro, €).
    units:unit_symbol(us_dollar, usd).
    ```

3.  **Associate units with their quantity kind**:
    Use `units:unit_kind(UnitName, QuantityName)` to link a unit to its corresponding quantity type. This tells the system what kind of quantity the unit represents.
    ```prolog
    units:unit_kind(euro, currency).
    units:unit_kind(us_dollar, currency).
    ```

4.  **Provide symbols for use in expression**
    If you want to use symbols in expression for `qeval`, you need to provide 1 arity predicate of the form `Symbol(Unit)` that can be imported in the user module:
    ```prolog
    :- module(currency_symbols, ['€'/1, usd/1]).
    '€'(euro).
    usd(us_dollar).
    ```
    The symbol name is arbitrary, but be very careful that every predicate is unique.

By defining these predicates, your custom units and quantities will be integrated into the system, allowing them to be used with `qeval/1` and other library features.

## Changelog

### Version 0.19.0 (2025-06-26)

- **Features & Enhancements:**
  - Use the swi-prolog module system to disambiguate symbol and unit names in expressions.

### Version 0.16.0 (2025-06-21)

- **Features & Enhancements:**
  - now correctly handles variables in `common_expr` (does unification instead of generate and test)
  - improved runtime of `common_expr` by partitioning along dimensions.
  - added a lot of documentation

### Version 0.15.0 (2025-06-08)
- **Features & Enhancements:**
  - Added a script to generate a hierarchical graph of quantities (`quantities.pdf`) (`9b475d9`, `5637d2c`).
  - Improved integration with `must_be/2` for type checking of quantities (`63e2992`, `7e5920e`, `9f61765`).
  - Enhanced flexibility by allowing variables for units and quantities in `common_expr/6` (`eec9baf`).
- **Fixes:**
  - Corrected `qformat/1` behavior and added its documentation (`ff92a11`, `b4e0f0c`).
  - Addressed a case where non-ground quantities were not handled correctly in `eval_/2` (`e17bbda`).
  - Resolved duplicate predicate exports and test names (`30e95f0`).
  - Adjusted the `radian` unit test to correctly expect an error (`e999fdc`).
- **Documentation:**
  - Added comprehensive PlDoc for all exported predicates in `prolog/units.pl` (`c109549`).
  - Documented core evaluation and formatting predicates `qeval/1` and `qformat/1` (`b4e0f0c`).
  - Significantly improved `README.md` with detailed explanations of quantity types, quantity points, `qeval/1` usage, `clpBNR` support, and how to add custom units/quantities (`b49f10e`, `6e36431`, `abf82ee`, `b0bd0e7`).
  - Added lists of predefined quantities (`Quantities.md`) and units (`Units.md`) (`c97724d`).
  - Documented library dependencies (`cfb6a36`).
- **Refactoring & Internal Changes:**
  - Rewrote the internal parsing mechanism for expressions (`01e853b`).
  - Refactored `inverse/2` predicate location (`18b34f3`).
  - Internal cleanups and improved usage of `alias_quantity_/1` (`496a997`, `2e9c42d`).

