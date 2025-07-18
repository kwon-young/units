:- module(international,[]).
:- reexport(international/symbols).

:- use_module(si, []).
:- use_module(international/symbols).

units:alias(international:kip,si:kilo(international:pound_force)).
units:unit_symbol_formula(international:pound,lb,45359237/100000000*(si:kilogram)).
units:unit_symbol_formula(international:ounce,oz,1/16*(international:pound)).
units:unit_symbol_formula(international:dram,dr,1/16*(international:ounce)).
units:unit_symbol_formula(international:grain,gr,1/7000*(international:pound)).
units:unit_symbol_formula(international:yard,yd,9144/10000*(si:metre)).
units:unit_symbol_formula(international:foot,ft,1/3*(international:yard)).
units:unit_symbol_formula(international:inch,in,1/12*(international:foot)).
units:unit_symbol_formula(international:pica,'P',1/6*(international:inch)).
units:unit_symbol_formula(international:point,p,1/12*(international:pica)).
units:unit_symbol_formula(international:mil,mil,1/1000*(international:inch)).
units:unit_symbol_formula(international:twip,twip,1/20*(international:point)).
units:unit_symbol_formula(international:mile,mi,1760*(international:yard)).
units:unit_symbol_formula(international:league,le,3*(international:mile)).
units:unit_symbol_formula(international:nautical_mile,nmi,1852*(si:metre)).
units:unit_symbol_formula(international:knot,kn,(international:nautical_mile)/(si:hour)).
units:unit_symbol_formula(international:poundal,pdl,(international:pound)*(international:foot)/(si:second)**2).
units:unit_symbol_formula(international:pound_force,lbf,(international:pound)*(si:standard_gravity)).
units:unit_symbol_formula(international:psi,psi,(international:pound_force)/(international:inch)**2).
units:unit_symbol_formula(international:mechanical_horsepower,'hp(I)',33000*(international:foot)*(international:pound_force)/(si:minute)).
