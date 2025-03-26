:- use_module(library(units)).

main :-
   % simple numeric operations
   qeval(10*km =:= 2*5*km),

   % conversions to common units
   qeval(1 * h =:= 3600 * s),
   qeval(1 * km + 1 * m =:= 1001 * m),

   % derived quantities
   qeval(1 * km / (1 * s) =:= 1000 * m / s),
   qeval(2 * km / h * (2 * h) =:= 4 * km),
   qeval(2 * km / (2 * km / h) =:= 1 * h),

   qeval(2 * m * (3 * m) =:= 6 * m**2),

   qeval(10 * km / (5 * km) =:= 2),

   qeval(1000 / (1 * s) =:= 1 * kHz),

   % assignement
   qeval(A is 10*m),

   % comparison
   qeval(A < 20*km).
