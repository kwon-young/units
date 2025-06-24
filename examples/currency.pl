% :- use_module(library(units)).
:- use_module('../prolog/units.pl').

units:dimension_symbol(currency_dim, '$').
units:quantity_parent(currency, currency_dim).

units:unit_symbol(euro, €).
units:unit_symbol(us_dollar, usd).

units:unit_kind(euro, currency).
units:unit_kind(us_dollar, currency).

exchange_rate(From, To, Rate) :-
   % user code to get real currency conversion value
   qeval(Rate is random_float *To/From).

exchange_to(From, ToUnit, To) :-
   qeval(F is From),
   F = _*_[U],
   exchange_rate(U, ToUnit, Rate),
   qeval(To is Rate * From).

main :-
   qeval(FromUsd is 100*usd),
   exchange_to(FromUsd, euro, ToEuro),
   format("~p -> ~p~n", [FromUsd, ToEuro]),
   qeval(_ is FromUsd + ToEuro).
   % ERROR: Domain error: `us_dollar' expected, found `euro'
