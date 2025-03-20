:- module(si, [unit/1]).

:- table unit/3.

unit_(second, 's', si:second).
unit_(metre, 'm', si:metre).
unit_(gram, 'g', si:gram).
unit_(ampere, 'A', si:ampere).
unit_(kelvin, 'K', si:kelvin).
unit_(mole, 'mol', si:mole).
unit_(candela, 'cd', si:candela).
unit_(degree_celsius, '℃', si:degree_celsius).

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
unit_(are, 'a', (si:deca(metre))^2).
unit_(litre, 'L', (si:deci(metre))^3).
unit_(tonne, 't', 1000 * (si:kilogram)).
unit_(dalton, 'Da', 16 605 390 666 050/ 10 000 000 000 000 * 10^ -27 * (si:kilogram)).
unit_(electronvolt, 'eV', 1 602 176 634 / 1 000 000 000 * 10^ -19 * (si:joule)).
unit(Unit, Symbol, Formula) :-
   unit_(Unit, Symbol, Formula).
unit(PrefixUnit, Symbol, Formula) :-
   \+ compound(Symbol),
   prefix(Prefix, PrefixSymbol, PrefixValue),
   PrefixUnit =.. [Prefix, Unit],
   unit_(Unit, UnitSymbol, _),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol),
   Formula = PrefixValue * (si:Unit).
unit(Alias, Symbol, Formula) :-
   alias(Alias, Unit),
   unit(Unit, Symbol, Formula).

unit_kind_(second, time).
unit_kind_(metre, length).
unit_kind_(gram, mass).
unit_kind_(ampere, electric_current).
unit_kind_(kelvin, thermodynamic_temperature).
unit_kind_(mole, amount_of_substance).
unit_kind_(candela, luminous_intensity).
unit_kind_(radian, angular_measure).
unit_kind_(steradian, solid_angular_measure).
unit_kind_(hertz, frequency).
unit_kind_(becquerel, activity).
unit_kind_(gray, absorbed_dose).
unit_kind_(sievert, dose_equivalent).

:- table unit_kind/2.

unit_kind(Unit, Kind) :-
   unit_kind_(Unit, Kind),
   !.
unit_kind(Unit, Kind) :-
   (  unit(Unit, _, Formula)
   ;  unit(_, Unit, Formula)
   ),
   formula_kind(Formula, Kind).
unit_kind(N, 1) :-
   number(N).

formula_kind(A*B, R) =>
   formula_kind(A, QA),
   formula_kind(B, QB),
   R = QA*QB.
formula_kind(A/B, R) =>
   formula_kind(A, QA),
   formula_kind(B, QB),
   R = QA/QB.
formula_kind(A^N, R) =>
   formula_kind(A, QA),
   R = QA^N.
formula_kind(System:Unit, Kind) =>
   System:unit_kind(Unit, Kind).
formula_kind(Unit, Kind) =>
   si:unit_kind(Unit, Kind).

origin(kelvin, zeroth_kelvin).
origin(degree_celsius, zeroth_degree_Celsius).

alias(kilogram, kilo(gram)).
alias(zeroth_kelvin, absolute_zero).
alias(zeroth_degree_Celsius, ice_point).
alias(hectare, hecto(are)).

absolute_point_origin(absolute_zero, thermodynamic_temperature).
relative_point_origin(ice_point, 273 150 * milli(kelvin)).

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

:- table unit/1.

unit(Unit) :-
   unit(Unit, _, _).
unit(Symbol) :-
   unit(_, Symbol, _).
