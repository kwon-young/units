# Units: Quantity and Units library for swi-prolog

Units is a quantity and units library modeled after [`mp-units`](https://mpusz.github.io/mp-units/latest/).

Here is a quick preview of what is possible:

```prolog
:- use_module(library(units)).

% simple numeric operations
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

% assignement and comparison 
?- qeval(A is 10*m), qeval(A < 20*km).
A = 10 * kind(isq:length)[si:metre].
```

The library can be used through a single predicate `qeval/1` which is a predicate that wraps any kind of arithmetic operations.

One of the specificity of the library is the combination of units and quantities.
Please read the excellent `mp-units` [documentation](https://mpusz.github.io/mp-units/latest/users_guide/framework_basics/systems_of_quantities/) on the advantages of using quantities in addition to units.

Units and quantites can be used in multiple ways:

* for units
    * `System:Unit`, example: `si:metre`. Currently, the `si` and `international` system are implemented.
    * `System:UnitSymbol`, example: `si:m` for `si:metre`.
    * `Unit`, example: `metre`
    * `UnitSymbol`, example: `m` for `si:metre`
    * `System:Prefix(Unit)`, example: `si:kilo(metre)`
    * `System:Prefix(OtherSystem:Unit)`, example: `si:kilo(international:pound_force)`
    * `Prefix(Unit)`, example: `kilo(metre)`
    * `PrefixSymbolUnitSymbol`, example: `km` for `kilo(metre)`
    * through aliases, but only a few are defined like `kilogram` for `si:kilo(gram)` or `hectare` for `si:hecto(are)`
* for quantities
    * `System:Quantity`, example: `isq:length`. `isq` is the only system of quantity implemented.
* for both at the same time
    * `Quantity[Unit]`, example: `isq:length[si:metre]`

Here is a interesting use of the `speed` quantity that can be use to generically describe the ratio of any type of `length` by any type of `time`:

```prolog
:- use_module(library(units)).

avg_speed(Distance, Time, Speed) :-
   qeval(S is Distance / Time),
   Speed = S.as(isq:speed).

?- avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, Speed),
   qmust_be(isq:speed[si:kilo(metre)/si:hour], Speed).
Speed = 110 * isq:speed[si:kilo(metre)/si:hour].

?- avg_speed(220 * isq:height[inch], 2 * si:second, Speed).
Speed = 110 * isq:speed[international:inch/si:second].

?- avg_speed(220 * si:gram, 2 * si:second, Speed).
ERROR: Domain error: `kind(isq:mass)/kind(isq:time)' expected, found `isq:speed'
```

The `qmust_be/2` predicate can be used to check the quantity and unit of a result.

## clpBNR support

One peculiar feature is that this library also supports clpBNR arithmetic:

```prolog
?- qeval({A*metre == B*inch}), A = 1.
A = 1,
B = 5000r127.
```
