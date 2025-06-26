:- use_module('../prolog/units.pl').
:- use_module('../prolog/units/systems/si/symbols.pl').

main :-
   format("units capacitor time curve example...~n"),
   qeval((
      CC is 0.47 * isq:capacitance[µF],
      V0 is 5.0 * isq:voltage['V'],
      RR is 4.7 * isq:resistance[si:kilo(si:ohm)]
   )),
   forall(
      between(0, 50, I),
      (
         qeval(TT is I * milli(si:second)),
         format("at ~@ voltage is ", [qformat(TT)]),
         qeval(Vt is V0 * exp(-TT / (RR * CC)) as isq:voltage),
         (  qeval(Vt >= 'V')
         -> qeval(V is Vt in 'V')
         ;  qeval(Vt >= mV)
         -> qeval(V is Vt in mV)
         ;  qeval(Vt >= µV)
         -> qeval(V is Vt in µV)
         ;  qeval(Vt >= nV)
         -> qeval(V is Vt in nV)
         ;  qeval(V is Vt in pV)
         ),
         qformat(V), nl
      )
   ).
