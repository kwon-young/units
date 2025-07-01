:- use_module('../prolog/units.pl').
:- use_module('../prolog/units/systems/si.pl').
:- use_module(library(record)).
:- use_module(library(error)).

units:quantity_parent(horizontal_length, isq:length).
units:quantity_parent(horizontal_area, iqs:area).
units:quantity_formula(horizontal_area, horizontal_length*isq:width).

:- record box(length: horizontal_length, width: isq:width, height: isq:height).

floor(Length, Width, Area) :-
   qeval(Area is Length * Width as horizontal_area).

main :-
   qeval((
      L is 2*m,
      W is 3*m,
      H is 1*m
   )),
   make_box([length(L), width(W), height(H)], Box),
   format("~p", [Box]), nl,
   make_box([length(W), width(L), height(H)], Box2), % works but is wrong
   format("~p", [Box2]), nl,
   qeval((
      L1 is 2*horizontal_length[m],
      W1 is 3*isq:width[m],
      H1 is 1*isq:height[m]
   )),
   make_box([length(L1), width(W1), height(H1)], Box1),
   format("~p", [Box1]), nl,
   box_length(Box1, Length),
   box_width(Box1, Width),
   floor(Length, Width, Floor),
   format("~p", [Floor]), nl,
   box_height(Box1, Height),
   catch(floor(Length, Height, Floor), E1, true), % won't work
   format("~p~n", [E1]),
   catch(make_box([length(W1), width(L1), height(H1)], _), E2, true), % won't work
   format("~p~n", [E2]),
   true.
