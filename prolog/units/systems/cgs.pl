:- module(cgs,[]).
:- reexport(cgs/symbols).

:- use_module(si, []).
:- use_module(cgs/symbols).

units:alias(cgs:centimetre,si:centi(si:metre)).
units:alias(cgs:gram,si:gram).
units:alias(cgs:second,si:second).
units:unit_symbol_formula(cgs:gal,'Gal',(cgs:centimetre)/(cgs:second)**2).
units:unit_symbol_formula(cgs:dyne,dyn,(cgs:gram)*(cgs:centimetre)/(cgs:second)**2).
units:unit_symbol_formula(cgs:erg,erg,(cgs:dyne)*(cgs:centimetre)).
units:unit_symbol_formula(cgs:barye,'Ba',(cgs:gram)/((cgs:centimetre)*(cgs:second)**2)).
units:unit_symbol_formula(cgs:poise,'P',(cgs:gram)/((cgs:centimetre)*(cgs:second))).
units:unit_symbol_formula(cgs:stokes,'St',(cgs:centimetre)**2/(cgs:second)).
units:unit_symbol_formula(cgs:kayser,'Ky',1/(cgs:centimetre)).
