:- module(international, []).

:- use_module("../units.pl").

% mass
units:unit_symbol_formula(international:pound, lb, 45 359 237/100 000 000 * si:kilogram).
units:unit_symbol_formula(international:ounce, oz, 1/16 * international:pound).
units:unit_symbol_formula(international:dram, dr, 1/16 * international:ounce).
units:unit_symbol_formula(international:grain, gr, 1/7 000 * international:pound).

% length
% https://en.wikipedia.org/wiki/United_States_customary_units#Length
units:unit_symbol_formula(international:yard, yd, 9 144/10 000 * si:metre).
units:unit_symbol_formula(international:foot, ft, 1/3 * international:yard).
units:unit_symbol_formula(international:inch, in, 1/12 * international:foot).
units:unit_symbol_formula(international:pica, 'P', 1/6 * international:inch).
units:unit_symbol_formula(international:point, p, 1/12 * international:pica).
units:unit_symbol_formula(international:mil, mil, 1/1 000 * international:inch).
units:unit_symbol_formula(international:twip, twip, 1/20 * international:point).
units:unit_symbol_formula(international:mile, mi, 1760 * international:yard).
units:unit_symbol_formula(international:league, le, 3 * international:mile).

units:unit_symbol_formula(international:nautical_mile, nmi, 1852 * si:metre).

% speed
units:unit_symbol_formula(international:knot, kn, international:nautical_mile / si:hour).

% force
% https://en.wikipedia.org/wiki/Poundal
units:unit_symbol_formula(international:poundal, pdl, international:pound * international:foot / si:second**2).

% https://en.wikipedia.org/wiki/Pound_(force)
units:unit_symbol_formula(international:pound_force, lbf, international:pound * si:standard_gravity).

% pressure
units:unit_symbol_formula(international:psi, psi, international:pound_force / international:inch**2).

% power
% https://en.wikipedia.org/wiki/Horsepower#Definitions
units:unit_symbol_formula(mechanical_horsepower, 'hpI', 33 000 * international:foot * international:pound_force / si:minute).

% https://en.wikipedia.org/wiki/Kip_(unit),
units:unit_symbol_formula(international:kip, kip, si:kilo(international:international:pound_force)).
