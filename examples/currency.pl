% :- use_module(library(units)).
:- use_module("units.pl").

units:quantity_dimension(currency, '$').

units:unit_symbol(euro, €).
units:unit_symbol(us_dollar, usd).

units:unit_kind(euro, currency).
units:unit_kind(us_dollar, currency).

exchange_rate(From, To, Rate) :-
   % user code to get real currency conversion value
   qeval(Rate is 0.9319664492078285 *To/From).

exchange_to(From, To) :-
   exchange_rate(From.u, To.u, Rate),
   qeval(To is Rate * From).

main :-
   qeval(FromUsd is 100*usd),
   ToEuro = q{u: euro, q: kind_of(currency), v: _},
   exchange_to(FromUsd, ToEuro),
   format("~p -> ~p~n", [FromUsd, ToEuro]).
   % qeval(_ is FromUsd + ToEuro).
   % ERROR: Domain error: `us_dollar' expected, found `euro'
