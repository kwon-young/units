:- use_module('../prolog/units.pl').
:- use_module('../prolog/units/systems/si/symbols.pl').

main :-
   qeval(Lever is 20*isq:position_vector[cm]),
   qeval(Force is 500*isq:force['N']),
   qeval(Angle is 90*isq:angular_measure[°]),
   qeval(Torque is force_as(Lever*Force*sin(Angle), isq:torque)/rad),
   format("Applying a perpendicular force of ~@ to a ~@ long lever results in ~@ of torque",
          [qformat(Force), qformat(Lever), qformat(Torque in ('N' * m / rad))]),
   nl.
