:- use_module('../prolog/units.pl').
:- use_module('../prolog/units/systems/si.pl').

main :-
   qeval((
      Speed is 110*km/h,
      RateOfClimb is -0.63657*m/s,
      GlideRatio is Speed / (-1*RateOfClimb),
      % conversion to radian will force GlideRatio to be convert to unit of 1
      % requiring the km, m, h and s units to be correctly converted to each other
      GlideAngle is (1/GlideRatio as isq:angular_measure) in rad
   )),
   % GlideRatio 48.000307201966095
   % GlideAngle:
   %   - 0.0208332 rad
   %   - 1.1936544337519466°
   format("GlideRatio ~@", [qformat(GlideRatio in 1)]), nl,
   format("GlideAngle:"), nl,
   format("  - ~@", [qformat(GlideAngle)]), nl,
   format("  - ~@", [qformat(GlideAngle in degree)]), nl,
   true.
