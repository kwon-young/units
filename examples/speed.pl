:- use_module(library(units)).

avg_speed(Distance, Time, Speed) :-
   qeval(S is Distance / Time),
   Speed = S.as(isq:speed).

main :-
   avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, Speed),
   qmust_be(isq:speed[si:kilo(metre)/si:hour], Speed).

