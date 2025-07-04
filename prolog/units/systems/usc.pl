:- module(usc,[]).
:- reexport(usc/symbols).

:- use_module(si, []).
:- use_module(international, []).
:- use_module(usc/symbols).

units:alias(usc:mechanical_horsepower,international:mechanical_horsepower).
units:alias(usc:psi,international:psi).
units:alias(usc:kip,international:kip).
units:alias(usc:pound_force,international:pound_force).
units:alias(usc:poundal,international:poundal).
units:alias(usc:knot,international:knot).
units:alias(usc:nautical_mile,international:nautical_mile).
units:alias(usc:league,international:league).
units:alias(usc:mile,international:mile).
units:alias(usc:twip,international:twip).
units:alias(usc:mil,international:mil).
units:alias(usc:point,international:point).
units:alias(usc:pica,international:pica).
units:alias(usc:inch,international:inch).
units:alias(usc:foot,international:foot).
units:alias(usc:yard,international:yard).
units:alias(usc:grain,international:grain).
units:alias(usc:dram,international:dram).
units:alias(usc:ounce,international:ounce).
units:alias(usc:pound,international:pound).
units:alias(usc:short_ton,usc:ton).
units:alias(usc:zeroth_rankine,si:zeroth_kelvin).
units:relative_point_origin(usc:zeroth_degree_Fahrenheit,point(459670*(si:milli(usc:rankine)))).
units:unit_origin(usc:rankine,usc:zeroth_rankine).
units:unit_origin(usc:degree_Fahrenheit,usc:zeroth_degree_Fahrenheit).
units:unit_symbol_formula(usc:fathom,'ftm(us)',2*(usc:yard)).
units:unit_symbol_formula(usc:cable,'cb(us)',120*(usc:fathom)).
units:unit_symbol_formula(usc:link,li,33/50*(usc:foot)).
units:unit_symbol_formula(usc:rod,rd,25*(usc:link)).
units:unit_symbol_formula(usc:chain,ch,4*(usc:rod)).
units:unit_symbol_formula(usc:furlong,fur,10*(usc:chain)).
units:unit_symbol_formula(usc:acre,acre,10*(usc:chain)**2).
units:unit_symbol_formula(usc:section,section,640*(usc:acre)).
units:unit_symbol_formula(usc:gallon,gal,231*(usc:inch)**3).
units:unit_symbol_formula(usc:pottle,pot,1/2*(usc:gallon)).
units:unit_symbol_formula(usc:quart,qt,1/2*(usc:pottle)).
units:unit_symbol_formula(usc:pint,pt,1/2*(usc:quart)).
units:unit_symbol_formula(usc:cup,c,1/2*(usc:pint)).
units:unit_symbol_formula(usc:gill,gi,1/2*(usc:cup)).
units:unit_symbol_formula(usc:fluid_ounce,'fl oz',1/4*(usc:gill)).
units:unit_symbol_formula(usc:tablespoon,tbsp,1/2*(usc:fluid_ounce)).
units:unit_symbol_formula(usc:shot,jig,3*(usc:tablespoon)).
units:unit_symbol_formula(usc:teaspoon,tsp,1/3*(usc:tablespoon)).
units:unit_symbol_formula(usc:minim,min,1/80*(usc:teaspoon)).
units:unit_symbol_formula(usc:fluid_dram,'fl dr',60*(usc:minim)).
units:unit_symbol_formula(usc:barrel,bbl,315/10*(usc:gallon)).
units:unit_symbol_formula(usc:oil_barrel,bbl,4/3*(usc:barrel)).
units:unit_symbol_formula(usc:hogshead,hogshead,63*(usc:gallon)).
units:unit_symbol_formula(usc:dry_barrel,bbl,7056*(usc:inch)**3).
units:unit_symbol_formula(usc:bushel,bu,3523907016688/100000000000*(si:litre)).
units:unit_symbol_formula(usc:peck,pk,1/4*(usc:bushel)).
units:unit_symbol_formula(usc:dry_gallon,gal,1/2*(usc:peck)).
units:unit_symbol_formula(usc:dry_quart,qt,1/4*(usc:dry_gallon)).
units:unit_symbol_formula(usc:dry_pint,pt,1/2*(usc:dry_quart)).
units:unit_symbol_formula(usc:quarter,qr,25*(usc:pound)).
units:unit_symbol_formula(usc:short_hundredweight,cwt,100*(usc:pound)).
units:unit_symbol_formula(usc:ton,t,2000*(usc:pound)).
units:unit_symbol_formula(usc:pennyweight,dwt,24*(usc:grain)).
units:unit_symbol_formula(usc:troy_once,'oz t',20*(usc:pennyweight)).
units:unit_symbol_formula(usc:troy_pound,'lb t',12*(usc:troy_once)).
units:unit_symbol_formula(usc:inch_of_mercury,inHg,3386389/1000*(si:pascal)).
units:unit_symbol_formula(usc:rankine,'°R',5/9*(si:kelvin)).
units:unit_symbol_formula(usc:degree_Fahrenheit,℉,usc:rankine).
