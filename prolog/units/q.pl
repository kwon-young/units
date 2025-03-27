:- module(q, []).

:- use_module("../units.pl").

M.in(Unit) := R :-
   units:eval_(M in Unit, R).

M.as(Quantity) := R :-
   units:eval_(M as Quantity, R).

M.cast(Quantity) := R :-
   units:eval_(cast(M, Quantity), R).

M.this(This) := This :-
   This =  q{q: _, u: _, v: _},
   M >:< This.
