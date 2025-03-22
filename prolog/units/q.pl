:- module(q, []).

:- use_module(units_utils).

M.in(Unit) := R :-
   units:eval_(M in Unit, R).

M.as(Quantity) := R :-
   units:eval_(M as Quantity, R).

M.cast(Quantity) := R :-
   units:eval_(cast(M, Quantity), R).
