:- module(unit_defs, [
   all_unit_kind/2,
   common_unit/5,
   normalize_unit/3,
   unit/2,
   unit_parent/2
]).

:- use_module(utils).
:- use_module(search).
:- use_module(quantity).
:- use_module('../units').

any_unit_symbol(Unit, Symbol) :-
   (  unit_symbol(Unit, Symbol)
   ;  unit_symbol_formula(Unit, Symbol, _)
   ).

has_prefix(Module:PrefixUnit, Symbol) :-
   prefix(Module:Prefix, PrefixSymbol, _),
   PrefixUnit =.. [Prefix, Unit],
   (  aliased(any_unit_symbol(Unit, UnitSymbol)),
      atom_concat(PrefixSymbol, UnitSymbol, Symbol)
   -> true
   ;  domain_error("has_prefix", Module:PrefixUnit-Symbol)
   ).

:- table prefix_unit_symbol_formula/3.

prefix_unit_symbol_formula(Module:PrefixUnit, Symbol, PrefixFormula*Unit) :-
   \+ compound(Symbol),
   prefix(Module:Prefix, PrefixSymbol, PrefixFormula),
   PrefixUnit =.. [Prefix, Unit],
   aliased(any_unit_symbol(Unit, UnitSymbol)),
   \+ has_prefix(Unit, UnitSymbol),
   atom_concat(PrefixSymbol, UnitSymbol, Symbol).

:- table unit/3.

unit(U, S, F) :-
   alias(U, F),
   (  aliased(any_unit_symbol(F, S))
   ;  aliased(prefix_unit_symbol_formula(F, S, _))
   ).
unit(U, S, F) :-
   (  unit_symbol_formula(U, S, F)
   ;  prefix_unit_symbol_formula(U, S, F)
   ).

:- table unit/2.

unit(U, S) :-
   (  unit_symbol(U, S)
   ;  unit(U, S, _)
   ).

:- table all_unit_kind/2.

all_unit_kind(Unit, Kind) :-
   all_unit_kind_(Unit, Kind).

all_unit_kind_(Unit, R), unit_kind(Unit, Kind) =>
   R = kind_of(Kind).
all_unit_kind_(Unit, R), unit(Unit, _, Formula) =>
   all_unit_kind_(Formula, R).
all_unit_kind_(Unit, R), derived(Unit) =>
   mapexpr(all_unit_kind_, [_, 1]>>true, Unit, Kind),
   normalize(Kind, NKind),
   normalize_kind(NKind, R).
all_unit_kind_(_, _) => fail.

fail_call(M:Goal, Arg) :-
   debug(fail_call, "~p", [M]),
   atom(Goal),
   Head =.. [Goal, Arg],
   predicate_property(M:Head, visible),
   call(M:Head).
   
normalize_unit(_, Unit, R), var(Unit), ground(R) =>
   Unit = R.
normalize_unit(M, Unit, R), var(Unit), var(R) =>
   when((ground(Unit) ; ground(R)), normalize_unit(M, Unit, R)).
normalize_unit(_, Unit, R), unit(Unit, _) =>
   R = Unit.
normalize_unit(M, Symbol, R), fail_call(M:Symbol, Unit) =>
   R = Unit.
normalize_unit(M, Unit, R), fail_call(M:Unit, ModuleUnit) =>
   R = ModuleUnit.
normalize_unit(M, Module:Symbol, R), fail_call(M:Symbol, Module:Unit) =>
   R = Module:Unit.
normalize_unit(M, ModulePrefixUnit, R),
      (  subsumes_term(_:_, ModulePrefixUnit)
      -> Module:PrefixUnit = ModulePrefixUnit
      ;  PrefixUnit = ModulePrefixUnit
      ),
      PrefixUnit =.. [P, Unit],
      (  prefix(Module:P, _, _), Prefix = P
      ;  prefix(Module:Prefix, P, _)
      ) =>
   normalize_unit(M, Unit, R1),
   R2 =.. [Prefix, R1],
   R = Module:R2.
normalize_unit(M, U, R), derived(U) =>
   mapexpr(normalize_unit(M), [_, _]>>fail, U, R).
normalize_unit(_, _, _) => fail.

common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit), unifiable(Unit1, Unit2, _) =>
   Unit1 = Unit2,
   NewFactor1 = 1,
   NewFactor2 = 1,
   NewUnit = Unit2.
common_unit(Unit1, NewFactor1, Unit2, NewFactor2, NewUnit) =>
   common_expr(unit_parent, Unit1, NewFactor1, Unit2, NewFactor2, NewUnit).

unit_parent(Child, Parent) :-
   unit(Child, _, Parent).
