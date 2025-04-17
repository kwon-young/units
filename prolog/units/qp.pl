:- module(qp, []).

:- use_module("../units.pl").

M.zero() := R :-
   R = qp{o: 0, q: q{v: 0, u: M.q.u, q: M.q.q}}.

M.origin() := R :-
   R = M.q.put([v=M.o]).

M.quantity_from_zero() := R :-
   Zero = M.zero(),
   qeval(R is M - Zero).

M.quantity_from(Other) := R :-
   qeval(R is M - Other in M.q.u).

M.point_for(Origin) := R :-
   R = qp{o: Origin, q: _},
   qeval(R is M).

M.in(Unit) := R :-
   R = M.put([q=M.q.in(Unit)]).
