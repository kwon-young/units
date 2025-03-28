:- use_module(library(units)).

% mass_velocity_energy(M, V, E) :-
%    qeval({E is (1r2*(M as isq:mass)*(V as isq:velocity)^2) as isq:kinetic_energy}).
% law_conservation_energy(M1, V1, M2, V2, E) :-
%    mass_velocity_energy(M1, V1, E1),
%    mass_velocity_energy(M2, V2, E2),
%    qeval({E is E1 + E2}).
%
% mass_velocity_momentum(M, V, Momentum) :-
%    qeval({Momentum is ((M as isq:mass)*(V as isq:velocity)) as isq:momentum}).
% law_conservation_momentum(M1, V1, M2, V2, Momentum) :-
%    mass_velocity_momentum(M1, V1, Momentum1),
%    mass_velocity_momentum(M2, V2, Momentum2),
%    qeval({Momentum is Momentum1 + Momentum2}).
avg_speed(Length, Time, Speed) :-
   qeval(Speed is Length / Time as isq:speed).

activity(Time, Activity, Result) :-
   qeval(Result is 1/ Time + Activity as isq:activity).
