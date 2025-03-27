:- module(si, [prefix/3]).

:- use_module("../units.pl").

units:unit_symbol(si:second, 's').
units:unit_symbol(si:metre, 'm').
units:unit_symbol(si:gram, 'g').
units:unit_symbol(si:ampere, 'A').
units:unit_symbol(si:kelvin, 'K').
units:unit_symbol(si:mole, 'mol').
units:unit_symbol(si:candela, 'cd').
units:unit_symbol(si:degree_celsius, '℃').

% derived units
units:unit_symbol_formula(si:radian, 'rad', (si:metre)/(si:metre)).
units:unit_symbol_formula(si:steradian, 'sr', (si:metre)**2/(si:metre)**2).
units:unit_symbol_formula(si:hertz, 'Hz', 1/(si:second)).
units:unit_symbol_formula(si:newton, 'N', (si:kilogram)*(si:metre)/(si:second)**2).
units:unit_symbol_formula(si:pascal, 'Pa', (si:newton)/(si:metre)**2).
units:unit_symbol_formula(si:joule, 'J', (si:newton)*(si:metre)).
units:unit_symbol_formula(si:watt, 'W', (si:joule) / (si:second)).
units:unit_symbol_formula(si:coulomb, 'C', (si:ampere) / (si:second)).
units:unit_symbol_formula(si:volt, 'V', (si:watt) / (si:ampere)).
units:unit_symbol_formula(si:farad, 'F', (si:coulomb) / (si:volt)).
units:unit_symbol_formula(si:ohm, 'Ω', (si:volt) / (si:ampere)).
units:unit_symbol_formula(si:siemens, 'S', 1/(si:ohm)).
units:unit_symbol_formula(si:weber, 'Wb', (si:volt) * (si:second)).
units:unit_symbol_formula(si:tesla, 'T', (si:weber) / (si:metre)**2).
units:unit_symbol_formula(si:henry, 'H', (si:weber) / (si:ampere)).
units:unit_symbol_formula(si:lumen, 'lm', (si:candela) * (si:steradian)).
units:unit_symbol_formula(si:lux, 'lx', (si:lumen) / (si:metre)**2).
units:unit_symbol_formula(si:becquerel, 'Bq', 1/(si:second)).
units:unit_symbol_formula(si:gray, 'Gy', (si:joule) / (si:kilogram)).
units:unit_symbol_formula(si:sievert, 'Sv', (si:joule) / (si:kilogram)).
units:unit_symbol_formula(si:katal, 'kat', (si:mole)/(si:second)).
units:unit_symbol_formula(si:minute, 'min', 60*(si:second)).
units:unit_symbol_formula(si:hour, 'h', 60*(si:minute)).
units:unit_symbol_formula(si:day, 'd', 24*(si:hour)).
units:unit_symbol_formula(si:astronomical_unit, 'au', 149 597 870 700 * (si:metre)).
units:unit_symbol_formula(si:degree, '°', pi / 180 * (si:radian)).
units:unit_symbol_formula(si:arcminute, '′', 1/60 * (si:degree)).
units:unit_symbol_formula(si:arcsecond, '″', 1/60 * (si:arcminute)).
units:unit_symbol_formula(si:are, 'a', si:deca(si:metre)**2).
units:unit_symbol_formula(si:litre, 'L', si:deci(si:metre)**3).
units:unit_symbol_formula(si:tonne, 't', 1000 * (si:kilogram)).
units:unit_symbol_formula(si:dalton, 'Da', 16 605 390 666 050/ 10**13 * 10** -27 * (si:kilogram)).
units:unit_symbol_formula(si:electronvolt, 'eV', 1 602 176 634 / 10**9 * 10** -19 * (si:joule)).

%constants
units:unit_symbol_formula(si:hyperfine_structure_transition_frequency_of_cs, 'Δν_Cs', 9 192 631 770 * si:hertz).
units:unit_symbol_formula(si:speed_of_light_in_vacuum, 'c', 299 792 458 * si:metre / si:second).
units:unit_symbol_formula(si:planck_constant, 'ℎ', 662 607 015 / 10**8 * 10** -34 * si:joule * si:second).
units:unit_symbol_formula(si:elementary_charge, 'e', 1 602 176 634 / 10**9 * 10** -19 * si:coulomb).
units:unit_symbol_formula(si:boltzmann_constant, 'k', 1 380 649 / 10**6 * 10** -23 * si:joule / si:kelvin).
units:unit_symbol_formula(si:avogadro_constant, 'N_A', 602 214 076 / 10**8 * 10** 23 / si:mole).
units:unit_symbol_formula(si:luminous_efficacy, 'K_cd', 683 * si:lumen / si:watt).
units:unit_symbol_formula(si:standard_gravity, 'g_0', 980 665 / 10**5 * si:metre / second**2).
units:unit_symbol_formula(si:magnetic_constant, 'u_0', 4 * pi * 10** -7 * si:henry / si:metre).

% alias
units:unit_symbol_formula(si:kilogram, 'kg', si:kilo(si:gram)).
units:unit_symbol_formula(si:hectare, 'ha', si:hecto(si:are)).

units:prefix_unit_symbol(si:kilogram, 'kg').

units:unit_kind(si:second, isq:time).
units:unit_kind(si:metre, isq:length).
units:unit_kind(si:gram, isq:mass).
units:unit_kind(si:ampere, isq:electric_current).
units:unit_kind(si:kelvin, isq:thermodynamic_temperature).
units:unit_kind(si:mole, isq:amount_of_substance).
units:unit_kind(si:candela, isq:luminous_intensity).
units:unit_kind(si:radian, isq:angular_measure).
units:unit_kind(si:steradian, isq:solid_angular_measure).
units:unit_kind(si:hertz, isq:frequency).
units:unit_kind(si:becquerel, isq:activity).
units:unit_kind(si:gray, isq:absorbed_dose).
units:unit_kind(si:sievert, isq:dose_equivalent).

origin(kelvin, si:zeroth_kelvin).
origin(degree_celsius, si:zeroth_degree_Celsius).

alias(zeroth_kelvin, si:absolute_zero).
alias(zeroth_degree_Celsius, si:ice_point).

absolute_point_origin(absolute_zero, si:thermodynamic_temperature).
relative_point_origin(ice_point, 273 150 * si:milli(kelvin)).

prefix(si:quecto, 'q', 10** -30).
prefix(si:ronto, 'r', 10** -27).
prefix(si:yocto, 'y', 10** -24).
prefix(si:zepto, 'z', 10** -21).
prefix(si:atto, 'a', 10** -18).
prefix(si:femto, 'f', 10** -15).
prefix(si:pico, 'p', 10** -12).
prefix(si:nano, 'n', 10** -9).
prefix(si:micro, 'µ', 10** -6).
prefix(si:milli, 'm', 10** -3).
prefix(si:centi, 'c', 10** -2).
prefix(si:deci, 'd', 10** -1).
prefix(si:deca, 'da', 10** 1).
prefix(si:hecto, 'h', 10** 2).
prefix(si:kilo, 'k', 10** 3).
prefix(si:mega, 'M', 10** 6).
prefix(si:giga, 'G', 10** 9).
prefix(si:tera, 'T', 10** 12).
prefix(si:peta, 'P', 10** 15).
prefix(si:exa, 'E', 10** 18).
prefix(si:zetta, 'Z', 10** 21).
prefix(si:yotta, 'Y', 10** 24).
prefix(si:ronna, 'R', 10** 27).
prefix(si:quetta, 'Q', 10** 30).
