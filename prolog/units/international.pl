:- module(international, [unit/2, unit/3, unit_kind/2, alias/2]).

:- use_module(units_utils).

units_utils:system(unit, international).

unit_kind(_, _) :- fail.
unit_(_, _) :- fail.

unit(Unit, Symbol) :-
   unit(Unit, Symbol, _).

% mass
unit_(pound, lb, 45 359 237/100 000 000 * si:kilogram).
unit_(ounce, oz, 1/16 * international:pound).
unit_(dram, dr, 1/16 * international:ounce).
unit_(grain, gr, 1/7 000 * international:pound).

% length
% https://en.wikipedia.org/wiki/United_States_customary_units#Length
unit_(yard, yd, 9 144/10 000 * si:metre).
unit_(foot, ft, 1/3 * international:yard).
unit_(inch, in, 1/12 * international:foot).
unit_(pica, 'P', 1/6 * international:inch).
unit_(point, p, 1/12 * international:pica).
unit_(mil, mil, 1/1 000 * international:inch).
unit_(twip, twip, 1/20 * international:point).
unit_(mile, mi, 1760 * international:yard).
unit_(league, le, 3 * international:mile).

unit_(nautical_mile, nmi, 1852 * si:metre).

% speed
unit_(knot, kn, international:nautical_mile / si:hour).

% force
% https://en.wikipedia.org/wiki/Poundal
unit_(poundal, pdl, international:pound * international:foot / si:second^2).

% https://en.wikipedia.org/wiki/Pound_(force)
unit_(pound_force, lbf, international:pound * si:standard_gravity).

% pressure
unit_(psi, psi, international:pound_force / international:inch^2).

% power
% https://en.wikipedia.org/wiki/Horsepower#Definitions
unit_(mechanical_horsepower, 'hp(I)', 33 000 * international:foot * international:pound_force / si:minute).

:- table unit/3.

unit(Unit, Symbol, Formula) :-
   unit_(Unit, Symbol, Formula).
unit(Alias, Symbol, Formula) :-
   alias(Alias, System:Unit),
   System:unit(Unit, Symbol, Formula).

% https://en.wikipedia.org/wiki/Kip_(unit),
alias(kip, si:kilo(international:pound_force)).
