:- use_module(library(units)).
:- use_module(library(record)).
:- use_module(library(error)).

units:quantity_parent(horizontal_length, isq:length).
units:quantity_parent(horizontal_area, iqs:area).
units:quantity_formula(horizontal_area, horizontal_length*isq:width).

is_quantity(Term) :-
   is_dict(Term, q),
   get_dict(q, Term, Q),
   (  var(Q)
   -> true
   ;  Q = kind_of(K)
   -> units:derived_root_kind(K)
   ;  units:alias_or_quantity(Q)
   ),
   get_dict(u, Term, U),
   (  var(U)
   -> true
   ;  units:normalize_unit(U, _)
   ),
   get_dict(v, Term, _).
error:has_type(q:Quantity, Term) :-
   (  ground(Quantity), units:alias_or_quantity(Quantity)
   -> true
   ;  domain_error(quantity, Quantity)
   ),
   is_quantity(Term),
   units:implicitly_convertible(Term.q, Quantity).

:- record box(length: q:horizontal_length, width: q:isq:width, height: q:isq:height).

main :-
   qeval((
      L is 2*m,
      W is 3*m,
      H is 1*m
   )),
   make_box([length(L), width(W), height(H)], Box),
   print_term(Box, []), nl,
   make_box([length(W), width(L), height(H)], Box2), % works but is wrong
   print_term(Box2, []), nl,
   qeval((
      L1 is 2*horizontal_length[m],
      W1 is 3*isq:width[m],
      H1 is 1*isq:height[m]
   )),
   make_box([length(L1), width(W1), height(H1)], Box1),
   print_term(Box1, []), nl,
   make_box([length(W1), width(L1), height(H1)], _). % won't work
