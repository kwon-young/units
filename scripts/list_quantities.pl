:- use_module(library(units)).

main :-
   findall(Q, units:mapexpr(units:alias_quantity_, Q), Qs),
   maplist(units:quantity_dimensions, Qs, Ds),
   mapsubterms(units:dimension_symbol, Ds, Ss),
   pairs_keys_values(Pairs, Qs, Ss),
   sort(Pairs, AlphaSort),
   sort(2, @=<, AlphaSort, Sort),
   print_term(Sort, []).
