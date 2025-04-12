:- use_module(library(units)).

avg_speed(Distance, Time, Speed) :-
   qeval({Speed is Distance / Time as isq:speed}).

main :-
   avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, Speed),
   format("~p~n", [Speed]).
   
main2 :-
   qeval(Speed is 60 * isq:velocity[km/hour]),
   qeval(Duration is 8 * s),
   qeval(Acceleration is (Speed / Duration in m/s**2) as isq:acceleration),
   format("~p~n", [Acceleration]).
