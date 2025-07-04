:- module(imperial,[]).
:- reexport(imperial/symbols).

:- use_module(international, []).
:- use_module(imperial/symbols).

units:alias(imperial:mechanical_horsepower,international:mechanical_horsepower).
units:alias(imperial:psi,international:psi).
units:alias(imperial:kip,international:kip).
units:alias(imperial:pound_force,international:pound_force).
units:alias(imperial:poundal,international:poundal).
units:alias(imperial:knot,international:knot).
units:alias(imperial:nautical_mile,international:nautical_mile).
units:alias(imperial:league,international:league).
units:alias(imperial:mile,international:mile).
units:alias(imperial:twip,international:twip).
units:alias(imperial:mil,international:mil).
units:alias(imperial:point,international:point).
units:alias(imperial:pica,international:pica).
units:alias(imperial:inch,international:inch).
units:alias(imperial:foot,international:foot).
units:alias(imperial:yard,international:yard).
units:alias(imperial:grain,international:grain).
units:alias(imperial:dram,international:dram).
units:alias(imperial:ounce,international:ounce).
units:alias(imperial:pound,international:pound).
units:alias(imperial:drachm,imperial:dram).
units:alias(imperial:long_ton,imperial:ton).
units:unit_symbol_formula(imperial:hand,hh,1/3*(imperial:foot)).
units:unit_symbol_formula(imperial:barleycorn,'Bc',1/3*(imperial:inch)).
units:unit_symbol_formula(imperial:thou,th,1/12000*(imperial:foot)).
units:unit_symbol_formula(imperial:chain,ch,22*(imperial:yard)).
units:unit_symbol_formula(imperial:furlong,fur,10*(imperial:chain)).
units:unit_symbol_formula(imperial:cable,cb,1/10*(imperial:nautical_mile)).
units:unit_symbol_formula(imperial:fathom,ftm,1/1000*(imperial:nautical_mile)).
units:unit_symbol_formula(imperial:link,li,1/100*(imperial:chain)).
units:unit_symbol_formula(imperial:rod,rd,25*(imperial:link)).
units:unit_symbol_formula(imperial:perch,perch,(imperial:rod)**2).
units:unit_symbol_formula(imperial:rood,rood,40*(imperial:perch)).
units:unit_symbol_formula(imperial:acre,acre,4*(imperial:rood)).
units:unit_symbol_formula(imperial:gallon,gal,454609/100000*(si:litre)).
units:unit_symbol_formula(imperial:quart,qt,1/4*(imperial:gallon)).
units:unit_symbol_formula(imperial:pint,pt,1/2*(imperial:quart)).
units:unit_symbol_formula(imperial:gill,gi,1/4*(imperial:pint)).
units:unit_symbol_formula(imperial:fluid_ounce,'fl oz',1/5*(imperial:gill)).
units:unit_symbol_formula(imperial:stone,st,14*(imperial:pound)).
units:unit_symbol_formula(imperial:quarter,qr,2*(imperial:stone)).
units:unit_symbol_formula(imperial:long_hundredweight,cwt,8*(imperial:stone)).
units:unit_symbol_formula(imperial:ton,t,2240*(imperial:pound)).
