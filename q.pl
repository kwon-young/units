:- module(q, []).

:- use_module(units_utils).
:- use_module(units, [eval_/2]).

M.in(Unit) := R :-
   eval_(M in Unit, R).

M.as(Quantity) := R :-
   eval_(M as Quantity, R).

M.cast(Quantity) := R :-
   eval_(cast(M, Quantity), R).
