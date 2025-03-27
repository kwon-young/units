:- use_module(library(units)).

units:quantity_parent(fuel_consumption, isq:volume/isq:length).

units:unit_symbol_formula(l_per_100km, 'l100km', si:litre/(100*si:kilo(si:metre))).

main :-
   qeval((
      Q1 is 5.8 * l_per_100km,
      Q2 is 5.8 * fuel_consumption[l_per_100km],
      _A1 is Q1 as isq:area % works
   )),
   qeval(_A2 is Q2 as isq:area). % does not work
