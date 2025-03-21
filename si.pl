:- module(si, [unit/2, unit/3, unit_kind/2, alias/2, prefix/3]).

:- use_module(units_utils).

units_utils:system(unit, si).

unit_(second, 's').
unit_(metre, 'm').
unit_(gram, 'g').
unit_(ampere, 'A').
unit_(kelvin, 'K').
unit_(mole, 'mol').
unit_(candela, 'cd').
unit_(degree_celsius, '℃').

:- table unit/2.

unit(Unit, Symbol) :-
   (  unit_(Unit, Symbol)
   ;  unit(Unit, Symbol, _)
   ).

% derived units
unit_(radian, 'rad', (si:metre)/(si:metre)).
unit_(steradian, 'sr', (si:metre)^2/(si:metre)^2).
unit_(hertz, 'Hz', 1/(si:second)).
unit_(newton, 'N', (si:kilogram)*(si:metre)/(si:second)^2).
unit_(pascal, 'Pa', (si:newton)/(si:metre)^2).
unit_(joule, 'J', (si:newton)*(si:metre)).
unit_(watt, 'W', (si:joule) / (si:second)).
unit_(coulomb, 'C', (si:ampere) / (si:second)).
unit_(volt, 'V', (si:watt) / (si:ampere)).
unit_(farad, 'F', (si:coulomb) / (si:volt)).
unit_(ohm, 'Ω', (si:volt) / (si:ampere)).
unit_(siemens, 'S', 1/(si:ohm)).
unit_(weber, 'Wb', (si:volt) * (si:second)).
unit_(tesla, 'T', (si:weber) / (si:metre)^2).
unit_(henry, 'H', (si:weber) / (si:ampere)).
unit_(lumen, 'lm', (si:candela) * (si:steradian)).
unit_(lux, 'lx', (si:lumen) / (si:metre)^2).
unit_(becquerel, 'Bq', 1/(si:second)).
unit_(gray, 'Gy', (si:joule) / (si:kilogram)).
unit_(sievert, 'Sv', (si:joule) / (si:kilogram)).
unit_(katal, 'kat', (si:mole)/(si:second)).
unit_(minute, 'min', 60*(si:second)).
unit_(hour, 'h', 60*(si:minute)).
unit_(day, 'd', 24*(si:hour)).
unit_(astronomical_unit, 'au', 149 597 870 700 * (si:metre)).
unit_(degree, '°', pi / 180 * (si:radian)).
unit_(arcminute, '′', 1/60 * (si:degree)).
unit_(arcsecond, '″', 1/60 * (si:arcminute)).
unit_(are, 'a', si:deca(metre)^2).
unit_(litre, 'L', si:deci(metre)^3).
unit_(tonne, 't', 1000 * (si:kilogram)).
unit_(dalton, 'Da', 16 605 390 666 050/ 10 000 000 000 000 * 10^ -27 * (si:kilogram)).
unit_(electronvolt, 'eV', 1 602 176 634 / 1 000 000 000 * 10^ -19 * (si:joule)).

:- table unit/3.

unit(U, S, F) :-
   unit_(U, S, F).
unit(Alias, S, F) :-
   alias(Alias, System:U),
   System:unit(U, S, F).
unit(PrefixUnit, Symbol, PrefixFormula*si:Unit) :-
   \+ compound(Symbol),
   prefix(Prefix, PrefixSymbol, PrefixFormula),
   PrefixUnit =.. [Prefix, Unit],
   (  unit_(Unit, UnitSymbol, _)
   ;  unit_(Unit, UnitSymbol)
   ),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol).

unit_kind(second, isq:time).
unit_kind(metre, isq:length).
unit_kind(gram, isq:mass).
unit_kind(ampere, isq:electric_current).
unit_kind(kelvin, isq:thermodynamic_temperature).
unit_kind(mole, isq:amount_of_substance).
unit_kind(candela, isq:luminous_intensity).
unit_kind(radian, isq:angular_measure).
unit_kind(steradian, isq:solid_angular_measure).
unit_kind(hertz, isq:frequency).
unit_kind(becquerel, isq:activity).
unit_kind(gray, isq:absorbed_dose).
unit_kind(sievert, isq:dose_equivalent).

origin(kelvin, si:zeroth_kelvin).
origin(degree_celsius, si:zeroth_degree_Celsius).

alias(kilogram, si:kilo(gram)).
alias(zeroth_kelvin, si:absolute_zero).
alias(zeroth_degree_Celsius, si:ice_point).
alias(hectare, si:hectoare).

absolute_point_origin(absolute_zero, si:thermodynamic_temperature).
relative_point_origin(ice_point, 273 150 * si:milli(kelvin)).

prefix(quecto, 'q', 10^ -30).
prefix(ronto, 'r', 10^ -27).
prefix(yocto, 'y', 10^ -24).
prefix(zepto, 'z', 10^ -21).
prefix(atto, 'a', 10^ -18).
prefix(femto, 'f', 10^ -15).
prefix(pico, 'p', 10^ -12).
prefix(nano, 'n', 10^ -9).
prefix(micro, 'µ', 10^ -6).
prefix(milli, 'm', 10^ -3).
prefix(centi, 'c', 10^ -2).
prefix(deci, 'd', 10^ -1).
prefix(deca, 'da', 10^ 1).
prefix(hecto, 'h', 10^ 2).
prefix(kilo, 'k', 10^ 3).
prefix(mega, 'M', 10^ 6).
prefix(giga, 'G', 10^ 9).
prefix(tera, 'T', 10^ 12).
prefix(peta, 'P', 10^ 15).
prefix(exa, 'E', 10^ 18).
prefix(zetta, 'Z', 10^ 21).
prefix(yotta, 'Y', 10^ 24).
prefix(ronna, 'R', 10^ 27).
prefix(quetta, 'Q', 10^ 30).
