:- use_module(library(units)).

main :-
   findall(U-S, units:unit(U, S), UnitSymbol),
   pairs_keys(UnitSymbol, Units),
   maplist(units:all_unit_kind, Units, Kinds),
   mapsubterms([kind_of(K), K]>>true, Kinds, Kinds1),
   pairs_keys_values(Pairs, UnitSymbol, Kinds1),
   maplist([X]>>format("~p~n", [X]), Pairs).
