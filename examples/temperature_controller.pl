:- use_module('../prolog/units.pl').
:- use_module('../prolog/units/systems/si.pl').

units:relative_point_origin(room_reference_temp,point(21*(si:degree_Celsius))).

main :-
   qeval(StepDelta is 0.5 * degree_Celsius),
   NumberOfSteps = 6,
   qeval(RoomRef is room_reference_temp + 0 * isq:'Celsius_temperature'[degree_Celsius]),
   qeval(RoomLow is RoomRef - NumberOfSteps * StepDelta),
   qeval(RoomHigh is RoomRef + NumberOfSteps * StepDelta),
   qeval(RoomRefK is RoomRef in si:kelvin),
   qeval(RoomRefKA is RoomRefK quantity_from si:absolute_zero),
   format("Room reference temperature: ~@ (~@, ~@)~n", [
      qformat(RoomRef quantity_from si:ice_point),
      qformat((RoomRef in usc:degree_Fahrenheit) quantity_from usc:zeroth_degree_Fahrenheit),
      qformat(RoomRefKA)]),
   nl,
   F = "| ~t~s~t~20+ | ~t~s~t~20+ | ~t~s~t~20+ | ~t~s~t~20+ |~n",
   format(F, ["Temperature delta", "Room reference", "Ice point", "Absolute zero"]),
   format("|~`=t~21+|~`=t~20+|~`=t~20+|~`=t~20+|~n"),
   print_temp("Lowest", RoomLow),
   print_temp("Default", RoomRef),
   print_temp("Highest", RoomHigh),
   true.

print_temp(Name, V) :-
   qeval((
      V1 is V - room_reference_temp,
      V2 is V - si:ice_point in degree_Celsius,
      V3 is V - si:absolute_zero in degree_Celsius
   )),
   F = "| ~s~t~20+ | ~t~@~t~20+ | ~t~@~t~20+ | ~t~@~t~20+ |~n",
   format(F, [Name, qformat(V1), qformat(V2), qformat(V3)]),
   true.
