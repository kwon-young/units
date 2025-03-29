:- use_module(library(units)).

main :-
   qeval((
      Speed is 110*km/h,
      RateOfClimb is -0.63657*m/s,
      GlideRatio is Speed / (-1*RateOfClimb),
      % conversion to radian will force GlideRatio to be convert to unit of 1
      % requiring the km, m, h and s units to be correctly converted to each other
      GlideAngle is (1/GlideRatio as isq:angular_measure) in rad
   )),
   format("GlideRatio ~p", [GlideRatio.in(1)]), nl,
   format("GlideAngle:", []), nl,
   format("  - ~p", [GlideAngle]), nl,
   format("  - ~p", [GlideAngle.in(degree)]), nl,
   true.
