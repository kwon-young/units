:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pcre)).

:- op(99, xfy, ::).
:- op(200, fy, symbol_text).

replace_string -->
   re_replace("namespace unit_symbols {(.*\n)*}  // namespace unit_symbols"/g, ""),
   re_replace("namespace survey1893 {(.*\n)*}  // namespace survey1893"/g, ""),
   re_replace("\\[\\[deprecated.*\n.*"/g, ""),
   re_replace("struct us_survey_.*"/g, ""),
   re_replace("//"/g, "%"),
   re_replace("#"/g, "%"),
   re_replace("MP_UNITS_EXPORT_BEGIN"/g, ""),
   re_replace("MP_UNITS_EXPORT_END"/g, ""),
   re_replace("MP_UNITS_EXPORT"/g, ""),
   re_replace("decltype\\(U\\)"/g, ""),
   re_replace("}\\s*% namespace\\s.+\n(\n|%.*)*\\s*(?<w>\\w)"/g, "};\n$w"),
   re_replace("namespace (?<ns1>\\w+)::(?<ns2>\\w+) {"/g, "$ns1::$ns2::{"),
   re_replace("namespace (?<ns1>\\w+) {"/g, "$ns1::{"),
   re_replace("{};"/g, ";"),
   re_replace("inline"/g, ""),
   re_replace("constexpr"/g, ""),
   re_replace("template<PrefixableUnit auto U>"/g, ""),
   re_replace("auto"/g, ""),
   re_replace("struct(?!ure)"/g, ""),
   re_replace("final"/g, ""),
   re_replace("template<>"/g, ""),
   re_replace("template<PrefixableUnit U>"/g, ""),
   re_replace(", U{}"/g, ""),
   re_replace("<"/g, "("),
   re_replace(">"/g, ")"),
   re_replace("\\) ({} \\w+|{}|\\w+);"/g, ");"),
   re_replace(";(?<rest>(?<r>[\n, \s]*%.*)*[\n, \s]*})"/g, "$rest"),
   re_replace("\\)\\("/g, ")**("),
   re_replace("\\(::"/g, "("),
   re_replace("'"/g, " "),
   re_replace("QUANTITY_SPEC"/g, "'QUANTITY_SPEC'"),
   re_replace("MP_UNITS_REMOVE_CONST"/g, "'MP_UNITS_REMOVE_CONST'"),
   re_replace("using namespace (?<module>\\w+)"/g, "using(namespace($module))"),
   re_replace("using (?<module>(\\w+::)\\w+)"/g, "%"),
   re_replace("MP_UNITS_INLINE"/g, ""),
   re_replace("bool"/g, ""),
   re_replace("u8"/g, ""),
   re_replace("symbol_text{"/g, "symbol_text {"),
   re_replace("Poynting_vector"/g, "'Poynting_vector'"),
   re_replace("Hamming_distance"/g, "'Hamming_distance'"),
   re_replace("Poisson_number"/g, "'Poisson_number'"),
   re_replace("Young_modulus"/g, "'Young_modulus'"),
   re_replace("Celsius_temperature"/g, "'Celsius_temperature'"),
   re_replace("(?<!specific_)Helmholtz_energy"/g, "'Helmholtz_energy'"),
   re_replace("(?<!specific_)Helmholtz_function"/g, "'Helmholtz_function'"),
   re_replace("(?<!specific_)Gibbs_energy"/g, "'Gibbs_energy'"),
   re_replace("(?<!specific_)Gibbs_function"/g, "'Gibbs_function'"),
   re_replace("Massieu_function"/g, "'Massieu_function'"),
   re_replace("Planck_function"/g, "'Planck_function'"),
   re_replace("Joule_Thomson_coefficient"/g, "'Joule_Thomson_coefficient'"),
   re_replace("Julian_year"/g, "'Julian_year'"),
   re_replace("Jupiter_mass"/g, "'Jupiter_mass'"),
   re_replace("Earth_mass"/g, "'Earth_mass'"),
   re_replace("survey1893"/g, "usc"),
   re_replace("si::si2019"/g, "si"),
   [].

parse_term(mp_units::T) ==>
   parse_term(T).
parse_term(unit_symbols::_) ==>
   [].
parse_term(Namespace::T), atom(Namespace) ==>
   push_namespace(Namespace, parse_term(T)).
parse_term({}(L)) ==>
   parse_term(L).
parse_term((T1; T2)) ==>
   parse_term(T1),
   parse_term(T2).
parse_term('QUANTITY_SPEC'(Quantity, Parent)) ==>
   parse_term('QUANTITY_SPEC'(Quantity, Parent), _).
parse_term('QUANTITY_SPEC'(Quantity, Parent, Opt1)) ==>
   parse_term('QUANTITY_SPEC'(Quantity, Parent), NsQuantity),
   quantity_spec_opt(NsQuantity, Opt1).
parse_term('QUANTITY_SPEC'(Quantity, Parent, Opt1, Opt2)) ==>
   parse_term('QUANTITY_SPEC'(Quantity, Parent), NsQuantity),
   quantity_spec_opt(NsQuantity, Opt1),
   quantity_spec_opt(NsQuantity, Opt2).
parse_term(space_before_unit_symbol(Unit)=false) ==>
   namespace(Ns),
   { add_namespace(Ns, Unit, NsUnit) },
   add_fact(no_space_before_unit_symbol(NsUnit)).
parse_term(Alias=Quantity) ==>
   namespace(Ns),
   {  add_namespace(Ns, Alias, NsAlias),
      normalize_expr(Ns, Quantity, NsQuantity)
   },
   add_fact(alias(NsAlias, NsQuantity)),
   origin(Origins),
   (  {ord_memberchk(NsQuantity, Origins)}
   -> add_origin(NsAlias)
   ;  []
   ).
parse_term(Unit:named_unit(Symbol, Opt1)) ==>
   parse_term(Unit:named_unit(Symbol, Opt1), _).
parse_term(Unit:named_unit(Symbol, Opt1, Opt2)) ==>
   parse_term(Unit:named_unit(Symbol, Opt1), NsUnit),
   unit_opt(NsUnit, Opt2, _).
parse_term(Point:absolute_point_origin(Kind)) ==>
   namespace(Ns),
   {  add_namespace(Ns, Point, NsPoint),
      add_namespace(Ns, Kind, NsKind)
   },
   add_fact(absolute_point_origin(NsPoint, NsKind)),
   add_origin(NsPoint).
parse_term(Point:relative_point_origin(mp_units::point(Unit)**N)) ==>
   namespace(Ns),
   {  add_namespace(Ns, Point, NsPoint),
      normalize_expr(Ns, Unit, NsUnit)
   },
   add_fact(relative_point_origin(NsPoint, point(N*NsUnit))),
   add_origin(NsPoint).
parse_term({}) ==>
   [].
parse_term(Dimension:base_dimension(Symbol)) ==>
   namespace(Ns),
   {  add_namespace(Ns, Dimension, NsDimension),
      symbol(Symbol, S)
   },
   add_fact(dimension_symbol(NsDimension, S)).
parse_term(PrefixU:prefixed_unit(Symbol, Expr)) ==>
   namespace(Ns),
   {  atom_concat(Prefix, '_', PrefixU),
      add_namespace(Ns, Prefix, NsPrefix),
      symbol(Symbol, S),
      normalize_expr(Ns, Expr, NsExpr)
   },
   add_fact(prefix(NsPrefix, S, NsExpr)).
parse_term(UnusedPrefix), UnusedPrefix =.. [_Prefix, 'MP_UNITS_REMOVE_CONST'()] ==>
   [].
parse_term(using(namespace(Module))) ==>
   fact(Facts),
   namespace(Ns),
   sequence(alias_fact(Ns, Module), Facts).

alias_fact(Ns, Module, Fact), arg(1, Fact, From), From = Module:Name ==>
   { add_namespace(Ns, Name, To) },
   add_fact(alias(To, From)).
alias_fact(_, _, _) ==> [].

fact(Facts, T, T) :-
   lookup(fact, Facts, T).

unit_opt(Unit, kind_of(Kind), R) ==>
   namespace(Ns),
   {  add_namespace(Ns, Kind, NsKind),
      R = basic
   },
   add_fact(unit_kind(Unit, NsKind)).
unit_opt(Unit, Opt, Type) ==>
   namespace(Ns),
   origin(Origins),
   unit_opt(Ns, Unit, Origins, Opt, Type).

origin(Origins, Tree, Tree) :-
   lookup(origin, Origins, Tree).

lookup(Key, Value, Tree) :-
   (  rb_lookup(Key, V, Tree)
   -> Value = V
   ;  existence_error(key, Key, Tree)
   ).
   
unit_opt(Ns, Unit, Origins, Origin, R),
      add_namespace(Ns, Origin, NsOrigin),
      ord_memberchk(NsOrigin, Origins) ==>
   {R = basic},
   add_fact(unit_origin(Unit, NsOrigin)).
unit_opt(Ns, _Unit, _Origins, Expr, NsExpr) ==>
   {  normalize_expr(Ns, Expr, NsExpr) }.


add_unit(Unit, Symbol, basic) ==>
   add_fact(unit_symbol(Unit, Symbol)).
add_unit(Unit, Symbol, Formula) ==>
   add_fact(unit_symbol_formula(Unit, Symbol, Formula)).
   
symbol(symbol_text {Symbol, _}, R) =>
   symbol(Symbol, R).
symbol(Symbol, R), string(Symbol) =>
   atom_string(Atom, Symbol),
   R = Atom.

add_origin(Origin, In, Out) :-
   rb_update(In, origin, L1, L2, Out),
   ord_add_element(L1, Origin, L2).

parse_term('QUANTITY_SPEC'(Quantity, Parent), NsQuantity) ==>
   namespace(Ns),
   {  add_namespace(Ns, Quantity, NsQuantity),
      normalize_expr(Ns, Parent, NsParent)
   },
   add_fact(quantity_parent(NsQuantity, NsParent)).
parse_term(Unit:named_unit(Symbol, Opt1), NsUnit) ==>
   namespace(Ns),
   {  add_namespace(Ns, Unit, NsUnit),
      symbol(Symbol, S)
   },
   unit_opt(NsUnit, Opt1, O1),
   add_unit(NsUnit, S, O1).

quantity_spec_opt(Quantity, quantity_character::Character) ==>
   add_fact(quantity_character(Quantity, quantity_character:Character)).
quantity_spec_opt(Quantity, is_kind) ==>
   add_fact(kind(Quantity)).
quantity_spec_opt(Quantity, Formula) ==>
   namespace(Ns),
   { normalize_expr(Ns, Formula, NsFormula) },
   add_fact(quantity_formula(Quantity, NsFormula)).

add_fact(Fact, In, Out) :-
   rb_update(In, fact, Facts, [Fact | Facts], Out).

normalize_expr(_, dimensionless, R) =>
   R = 1.
normalize_expr(_, one, R) =>
   R = 1.
normalize_expr(Ns, A*B, R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, B, B1),
   R = A1*B1.
normalize_expr(Ns, A/B, R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, B, B1),
   R = A1/B1.
normalize_expr(Ns, pow(N)**A, R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, N, N1),
   R = A1**N1.
normalize_expr(Ns, pow(B,C)**A, R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, B, B1),
   normalize_expr(Ns, C, C1),
   R = A1**(B1/C1).
normalize_expr(Ns, inverse(A), R) =>
   normalize_expr(Ns, A, A1),
   R = 1/A1.
normalize_expr(Ns, mag(N), R) =>
   normalize_expr(Ns, N, R).
normalize_expr(Ns, cubic(A), R) =>
   normalize_expr(Ns, A, A1),
   R = A1**3.
normalize_expr(Ns, square(A), R) =>
   normalize_expr(Ns, A, A1),
   R = A1**2.
normalize_expr(_Ns, mag_ratio(A, B), R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, B, B1),
   R = A1/B1.
normalize_expr(_Ns, mag_power(A, B), R) =>
   normalize_expr(Ns, A, A1),
   normalize_expr(Ns, B, B1),
   R = A1**B1.
normalize_expr(_Ns, N, R), number(N) =>
   R = N.
normalize_expr(_Ns, π, R) =>
   R = pi.
normalize_expr(Ns, Mod::PrefixUnit, R), PrefixUnit =.. [Prefix, Unit] =>
   normalize_expr(Ns, Unit, NsUnit),
   PrefixNsUnit =.. [Prefix, NsUnit],
   R = Mod:PrefixNsUnit.
normalize_expr(Ns, PrefixUnit, R), PrefixUnit =.. [Prefix, Unit] =>
   normalize_expr(Ns, Unit, NsUnit),
   PrefixNsUnit =.. [Prefix, NsUnit],
   add_namespace(Ns, PrefixNsUnit, R).
normalize_expr(Ns, A, R) =>
   add_namespace(Ns, A, R).

add_namespace([N], survey1893::In, Out) =>
   Out = N:In.
add_namespace([_], N::In, Out) =>
   Out = N:In.
add_namespace([N], In, Out) =>
   Out = N:In.
add_namespace([], N::In, Out) =>
   Out = N:In.
add_namespace([], In, Out) =>
   Out = In.


namespace(Ns, Tree, Tree) :-
   lookup(ns, Ns, Tree).
push_namespace(N, Goal, T1, T) :-
   rb_update(T1, ns, L1, L2, T2),
   (  L1 = []
   -> L2 = [N],
      call(Goal, T2, T3),
      rb_update(T3, ns, L2, L1, T)
   ;  L2 = L1,
      call(Goal, T2, T)
   ).

write_facts(File, Facts) :-
   directory_file_path(FileDir, Name, File),
   file_name_extension(NameModule, _, Name),
   (  NameModule = units
   -> file_base_name(FileDir, PrefixModule),
      atomic_list_concat([PrefixModule, '_', NameModule], Module)
   ;  Module = NameModule
   ),
   indir(InDir),
   outdir(OutDir),
   relative_file_name(File, InDir, RelFile),
   relative_file_name(OutFile1, OutDir, RelFile),
   file_name_extension(Base, _, OutFile1),
   file_name_extension(Base, ".pl", OutFile),
   directory_file_path(Dir, _, OutFile),
   make_directory_path(Dir),
   setup_call_cleanup(
      open(OutFile, write, Stream),
      (  write_term(Stream, :- module(Module, []), [fullstop(true), nl(true)]),
         member(Fact, Facts),
         write_term(Stream, units:Fact, [fullstop(true), nl(true), quoted(true)]),
         fail
      ),
      close(Stream)
   )
   ;  true.

new_state(Tree3) :-
   rb_new(Tree),
   rb_insert_new(Tree, ns, [], Tree1),
   rb_insert_new(Tree1, fact, [], Tree2),
   rb_insert_new(Tree2, origin, [], Tree3).

normalize_facts(RFacts, FinalFacts) :-
   reverse(RFacts, Facts),
   length(Facts, N),
   numlist(1, N, Ns),
   pairs_keys_values(NFacts, Ns, Facts),
   sort(2, @<, NFacts, NUniqueFacts),
   sort(1, @<, NUniqueFacts, NUniqueSortedFacts),
   pairs_values(NUniqueSortedFacts, UniqueFacts),
   maplist(functor, UniqueFacts, Names, Arities),
   maplist([Name, A, Name/A]>>true, Names, Arities, NamesArities),
   pairs_keys_values(FunctorFacts, NamesArities, UniqueFacts),
   sort(1, @=<, FunctorFacts, SortedFunctorFacts),
   pairs_values(SortedFunctorFacts, FinalFacts).

parse_file(File, TreeIn, TreeOut) :-
   read_file_to_string(File, S1, []),
   call_dcg(replace_string, S1, S2),
   % format("~s", [S2]),
   read_term_from_atom(S2, T, []),
   must_be(ground, T),
   lookup(fact, InitialFacts, TreeIn),
   call_dcg(parse_term(T), TreeIn, TreeOut),
   lookup(fact, RFacts, TreeOut),
   append(NewFacts, InitialFacts, RFacts),
   (  NewFacts = [_|_]
   -> normalize_facts(NewFacts, Facts),
      write_facts(File, Facts)
   ;  true
   ).

indir("/home/kwon-young/prog/mp-units/src/systems/include/mp-units/systems/.").
outdir("/home/kwon-young/prog/units/prolog/units/systems/.").

file("isq/atomic_and_nuclear_physics.h").
file("isq/base_quantities.h").
file("isq/electromagnetism.h").
file("isq/information_science_and_technology.h").
file("isq/light_and_radiation.h").
file("isq/mechanics.h").
file("isq/si_quantities.h").
file("isq/space_and_time.h").
file("isq/thermodynamics.h").
file("si/constants.h").
file("si/prefixes.h").
file("si/units.h").
file("iec/binary_prefixes.h").
file("iec/units.h").
file("angular/units.h").
file("cgs.h").
file("hep.h").
file("iau.h").
file("international.h").
file("imperial.h").
file("usc.h").

main :-
   findall(File, (
      file(RelFile),
      indir(InDir),
      relative_file_name(File, InDir, RelFile)
   ), Files),
   print_term(Files, []),
   new_state(T),
   foldl(parse_file, Files, T, _).
