:- use_module(library(units)).
:- use_module(library(dot_dcg)).

main :-
   findall(Q, units:mapexpr(units:alias_quantity_, Q), Qs),
   maplist(units:quantity_dimensions, Qs, Ds),
   mapsubterms(units:dimension_symbol, Ds, Ss),
   pairs_keys_values(Pairs, Qs, Ss),
   sort(Pairs, AlphaSort),
   sort(2, @=<, AlphaSort, Sort),
   print_term(Sort, []).

main_graph :-
   time(findall(edge_stmt([Q, P]), quantity_parent(isq:Q, isq:P), Edges)),
   time(dot(digraph("quantities", Edges), X, [])),
   open("quantities.dot", write, S),
   format(S, X, []),
   close(S).
