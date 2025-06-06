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
   time(findall(Q-P, quantity_parent(Q, P), Edges)),
   partition([_-Q]>>units:derived_quantity(Q), Edges, Derived, Simple),
   pairs_keys_values(Simple, K, V),
   append(K, V, Nodes),
   maplist(node_label, Nodes, Labels),
   maplist(edge, Simple, EdgesStmt),
   maplist(node, Nodes, Labels, NodesStmt),
   append(EdgesStmt, NodesStmt, Stmts),
   time(dot(digraph("quantities", Stmts), X, [])),
   open("quantities.dot", write, S),
   format(S, X, []),
   close(S).

as_atom(1, R) => R = one.
as_atom(S:A, A_) => atomic_list_concat([S, '_', A], A_).
as_atom(A, A_) =>
   format(atom(A_), "~p", [A]).
edge(A-B, edge_stmt([A_, B_])) :-
   as_atom(A, A_),
   as_atom(B, B_).
node(Node, Label, node_stmt(Node_, [attr(label, Label)])) :-
   as_atom(Node, Node_).

node_label(Node, Quoted) :-
   findall(Alias, alias(Alias, Node), Aliases),
   maplist([A, A_]>>format(atom(A_), "~p", [A]), [Node | Aliases], Atoms),
   atomic_list_concat(Atoms, " / ", Label),
   (  (  quantity_parent(Node, Parent), units:derived_quantity(Parent)
      ;  quantity_formula(Node, Parent)
      )
   -> format(atom(Formula), "\n(~p)", Parent)
   ;  Formula = ''
   ),
   atomic_list_concat(['"', Label, Formula, '"'], Quoted).
