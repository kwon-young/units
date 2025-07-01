:- module(iau,[]).
:- reexport(iau/symbols).

:- use_module(si, []).
:- use_module(iau/symbols).

units:alias(iau:astronomical_unit,si:astronomical_unit).
units:unit_symbol_formula(iau:day,'D',si:day).
units:unit_symbol_formula(iau:'Julian_year',a,36525/100*(iau:day)).
units:unit_symbol_formula(iau:solar_mass,'M_☉',198847/100000*10**30*(si:kilogram)).
units:unit_symbol_formula(iau:'Jupiter_mass','M_JUP',1898/1000*10**27*(si:kilogram)).
units:unit_symbol_formula(iau:'Earth_mass','M_EARTH',59742/10000*10**24*(si:kilogram)).
units:unit_symbol_formula(iau:lunar_distance,'LD',384399*(si:kilo(si:metre))).
units:unit_symbol_formula(iau:light_year,ly,9460730472580800*(si:metre)).
units:unit_symbol_formula(iau:parsec,pc,(iau:astronomical_unit)/(1/3600*(si:degree))).
units:unit_symbol_formula(iau:angstrom,'Å',10** -10*(si:metre)).
units:unit_symbol_formula(iau:gaussian_gravitational_constant,k,1720209895/100000000000*(iau:astronomical_unit)**(3/2)/(iau:solar_mass)**(1/2)/(iau:day)).
units:unit_symbol_formula(iau:speed_of_light,'c₀',si:speed_of_light_in_vacuum).
units:unit_symbol_formula(iau:constant_of_gravitation,'G',667430/100000*10** -11*(si:metre)**3/(si:kilogram)/(si:second)**2).
units:unit_symbol_formula(iau:hubble_constant,'H₀',701/10*(si:kilo(si:metre))/(si:second)/(si:mega(iau:parsec))).
