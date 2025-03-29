:- use_module(library(units)).

units:quantity_dimension(currency, '$').

units:unit_symbol(euro, €).
units:unit_symbol(us_dollar, usd).

units:unit_kind(euro, currency).
units:unit_kind(us_dollar, currency).

exchange_rate(From, To, Rate) :-
   % user code to get real currency conversion value
   qeval(Rate is random_float *To/From).

exchange_to(From, ToUnit, To) :-
   exchange_rate(From.u, ToUnit, Rate),
   qeval(To is Rate * From).

main :-
   qeval(FromUsd is 100*usd),
   exchange_to(FromUsd, euro, ToEuro),
   format("~p -> ~p~n", [FromUsd, ToEuro]),
   qeval(_ is FromUsd + ToEuro).
   % ERROR: Domain error: `us_dollar' expected, found `euro'
