:- use_module('../prolog/units.pl').

overwrite_(international:mechanical_horsepower, hp).
overwrite_(usc:oil_barrel, oil_bbl).
overwrite_(usc:dry_barrel, dry_bbl).
overwrite_(usc:dry_gallon, dry_gal).
overwrite_(usc:dry_pint, dry_pt).
overwrite_(usc:dry_quart, dry_qt).
overwrite_(iec:volt_ampere_reactive_power, 'VAR').
overwrite(Unit, Symbol, NewSymbol) :-
   (  overwrite_(Unit, NewSymbol)
   -> true
   ;  NewSymbol = Symbol
   ).

remove(imperial:dram).
remove(imperial:drachm).
remove(imperial:long_ton).
remove(usc:short_ton).
remove(si:peta(si:are)).
remove(si:centi(si:day)).
remove(si:femto(si:tonne)).
remove(si:pico(si:tonne)).
remove(si:quecto(si:tonne)).
remove(si:centi(si:hour)).
remove(si:hecto(si:hour)).
remove(si:yocto(si:day)).
remove(si:ronto(si:day)).
remove(si:pico(si:boltzmann_constant)).
remove(si:pico(si:speed_of_light_in_vacuum)).
get_unit_symbol(M:PU, S3) :-
   utils:aliased(unit_defs:any_unit_symbol(M:U, S1)),
   \+ remove(M:U),
   overwrite(M:U, S1, S2),
   (  units:prefix(M:P, PS, _),
      PU =.. [P, M:U],
      \+ remove(M:PU),
      atom_concat(PS, S2, S3)
   ;  PU = U,
      S3 = S2
   ;  U \== S2,
      PU = U,
      S3 = U
   ).
get_unit_symbol(si:ampere, amp).
get_unit_symbol(si:degree, deg).
get_unit_symbol(si:degree_Celsius, deg_C).
get_unit_symbol(usc:degree_Fahrenheit, deg_F).

main :-
   findall(M-(S-U), get_unit_symbol(M:U, S), P1),
   keysort(P1, P2),
   group_pairs_by_key(P2, P3),
   maplist(main_system, P3).

main_system(System-P1) :-
   pairs_keys(P1, Symbols),
   (  is_set(Symbols)
   -> true
   ;  keysort(P1, S1),
      group_pairs_by_key(S1, G1),
      include([_-[_,_|_]]>>true, G1, G2),
      domain_error(set, System-G2)
   ),
   maplist(fact(System), P1, Facts),
   format(atom(File), "prolog/units/systems/~s/symbols.pl", [System]),
   directory_file_path(Dir, _, File),
   make_directory_path(Dir),
   atomic_list_concat([System, symbol], '_', Module),
   maplist([K-_, K/1]>>true, P1, Defs),
   setup_call_cleanup(
      open(File, write, Stream),
      (  write_term(Stream, :- module(Module, Defs), [fullstop(true), nl(true), quoted(true)]),
         member(Fact, Facts),
         write_term(Stream, Fact, [fullstop(true), nl(true), quoted(true), ignore_ops(true)]),
         fail
      ),
      close(Stream)
   )
   ;  true.

fact(M, S-U, Fact) :-
   Fact =.. [S, M:U].
