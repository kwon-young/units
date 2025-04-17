:- module(q, []).

:- use_module("../units.pl").

M.in(Unit) := R :-
   qeval(R is M in Unit).

M.as(Quantity) := R :-
   qeval(R is M as Quantity).

M.cast(Quantity) := R :-
   qeval(R is cast(M, Quantity)).

M.this(This) := This :-
   This =  q{q: _, u: _, v: _},
   M >:< This.
