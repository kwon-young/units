:- use_module(library(clpBNR)).

% flatten([], []).
% flatten([H | T], L) :-
%    flatten_(H, L, R),
%    flatten(T, R).

my_flatten(L, Flat) :-
   flatten_(L, Flat, []).
flatten_([], R, R).
flatten_([H | T], L, R) :-
   flatten_(H, L, R1),
   flatten_(T, R1, R).
flatten_(a, [a | R], R).
flatten_(b, [b | R], R).
flatten_(c, [c | R], R).
flatten_(d, [d | R], R).
flatten_(1, [1 | R], R).
flatten_(2, [2 | R], R).
flatten_(foo, [foo | R], R).

is_list([]).
is_list([

trisect(A, B, _, X) :- B - A < 1e-6, !,
   X is (A+B)/2.
trisect(A, B, F, X) :-
   P is (2*A+B)/3,
   call(F, P, U),
   Q is (A+2*B)/3,
   call(F, Q, V),
   (U < V ->
      trisect(P, B, F, X);
      trisect(A, Q, F, X)).

fun(P, F) :-
   F is P*(1-P**2)*(1-P**3).
