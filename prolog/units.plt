:- begin_tests(units, [timeout(2)]).

:- use_module(units).
:- use_module(units/systems/si).
:- use_module(units/systems/angular, []).
:- use_module(units/systems/cgs, []).
:- use_module(units/systems/hep, []).
:- use_module(units/systems/iau, []).
:- use_module(units/systems/iec, []).
:- use_module(units/systems/imperial, []).
:- use_module(units/systems/international, []).
:- use_module(units/systems/usc, [inch/1, foot/1, lbf/1]).

qeval_data(si:metre =:= si:metre).
qeval_data(si:kilo(metre) =:= si:kilo(metre)).
qeval_data(si:kilogram =:= si:kilo(gram)).
qeval_data(si:kg =:= si:kilo(gram)).
qeval_data(10*(si:kilo(metre)) =:= 5*2*(si:kilo(metre))).
qeval_data(10*(si:kilo(metre)) / 2 =:= 5*(si:kilo(metre))).
qeval_data(1 * (si:hour) =:= 3600 * (si:second)).
qeval_data(1 * (si:kilo(metre)) + 1 * (si:metre) =:= 1001 * (si:metre)).
qeval_data(1 * (si:kilo(metre)) / (1 * (si:second)) =:= 1000 * (si:metre) / (si:second)).
qeval_data(2 * (si:kilo(metre)) / (si:hour) * (2 * (si:hour)) =:= 4 * (si:kilo(metre))).
qeval_data(2 * (si:kilo(metre)) / (2 * (si:kilo(metre)) / (si:hour)) =:= 1 * (si:hour)).
qeval_data(2 * (si:metre) * (3 * (si:metre)) =:= 6 * (si:metre)**2).
qeval_data(10 * (si:kilo(metre)) / (5 * (si:kilo(metre))) =:= 2).
qeval_data(1000 / (1 * (si:second)) =:= 1 * (si:kilo(hertz))).
qeval_data(1001 / (1 * (si:second)) =\= 1 * (si:kilo(hertz))).
qeval_data(si:metre < si:kilo(metre)).
qeval_data(si:metre =< si:kilo(metre)).
qeval_data(si:metre > si:centi(metre)).
qeval_data(si:metre >= si:centi(metre)).

test('qeval', [forall(qeval_data(Expr))]) :-
   qeval(Expr).

fail_qeval_data(1001 / (1 * (si:second)) =:= 1 * (si:kilo(hertz))).

test('fail_qeval', [forall(fail_qeval_data(Expr)), fail]) :-
   qeval(Expr).

error_qeval_data(si:hertz =:= si:becquerel).
error_qeval_data(_ is si:hertz + si:becquerel).

test('error_qeval', [forall(error_qeval_data(Expr)), error(domain_error(_, _))]) :-
   qeval(Expr).

implicitly_convertible_data(isq:width, isq:length).
implicitly_convertible_data(isq:radius, isq:width).
implicitly_convertible_data(isq:radius, isq:length).
implicitly_convertible_data(isq:mass*isq:length**2/isq:time**2, isq:energy).
implicitly_convertible_data(isq:mass*isq:height**2/isq:time**2, isq:energy).
implicitly_convertible_data(isq:height**2*isq:mass/isq:time**2, isq:energy).
implicitly_convertible_data(isq:mass*isq:speed**2, isq:kinetic_energy).
implicitly_convertible_data(kind_of(isq:length), isq:height).
implicitly_convertible_data(isq:acceleration, isq:speed/isq:time).
implicitly_convertible_data(kind_of(isq:length/isq:time**2), isq:acceleration).
implicitly_convertible_data(kind_of(isq:length/isq:time**2), isq:velocity/isq:duration).
implicitly_convertible_data(kind_of(isq:time*isq:frequency), isq:rotation).
implicitly_convertible_data(kind_of(isq:time*isq:frequency), kind_of(isq:rotation)).
implicitly_convertible_data(kind_of(isq:time*isq:frequency), kind_of(isq:angular_measure)).
implicitly_convertible_data(kind_of(isq:rotation/isq:frequency), kind_of(isq:time)).

test('implicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   units:implicitly_convertible(Q1, Q2).

% not implicitly convertible that are explicitly convertible
explicitly_convertible_data(isq:length, isq:width).
explicitly_convertible_data(isq:width, isq:radius).
explicitly_convertible_data(isq:length, isq:radius).
explicitly_convertible_data(isq:energy, isq:mechanical_energy).
explicitly_convertible_data(isq:length, isq:height).
explicitly_convertible_data(isq:mass*isq:length**2/isq:time**2, isq:mechanical_energy).
explicitly_convertible_data(isq:angular_measure, 1).
explicitly_convertible_data(isq:speed/isq:time, isq:acceleration).

not_implicitly_convertible_data(isq:time*isq:frequency, isq:rotation).

test('not_implicitly_convertible(explicit_data)', [forall(explicitly_convertible_data(Q1, Q2)), fail]) :-
   units:implicitly_convertible(Q1, Q2).
test('not_implicitly_convertible', [forall(not_implicitly_convertible_data(Q1, Q2)), fail]) :-
   units:implicitly_convertible(Q1, Q2).

common_quantity_data(isq:width, isq:height, isq:length).
common_quantity_data(isq:thickness, isq:radius, isq:width).
common_quantity_data(isq:distance, isq:path_length, isq:path_length).
common_quantity_data(1, 1, 1).
common_quantity_data(1, isq:rotation, 1).
common_quantity_data(kind_of(isq:length), kind_of(isq:length), kind_of(isq:length)).
common_quantity_data(isq:width, kind_of(isq:length), isq:width).

test('common_quantity', [forall(common_quantity_data(Q1, Q2, Q))]) :-
   units:common_quantity(Q1, Q2, Q).

test('not_common_quantity', [fail]) :-
   call_with_time_limit(
      1,
      units:common_quantity(isq:voltage * isq:time/(isq:capacitance * isq:resistance), isq:time, _)
   ).

test('explicitly_convertible', [forall(implicitly_convertible_data(Q1, Q2))]) :-
   units:explicitly_convertible(Q1, Q2).

test('explicitly_convertible2', [forall(explicitly_convertible_data(Q1, Q2))]) :-
   units:explicitly_convertible(Q1, Q2).

not_explicitly_convertible_data(isq:height, isq:width).
not_explicitly_convertible_data(isq:time, isq:length).
not_explicitly_convertible_data(isq:frequency, isq:activity).
not_explicitly_convertible_data(kind_of(isq:frequency), kind_of(isq:activity)).
not_explicitly_convertible_data(isq:mass*isq:height**2/isq:time**2, isq:mechanical_energy).

test('not_explicitly_convertible', [forall(not_explicitly_convertible_data(Q1, Q2)), fail]) :-
   units:explicitly_convertible(Q1, Q2).

avg_speed(Distance, Time, Speed) :-
   qeval(Speed is Distance / Time as isq:speed).

test('avg_speed') :-
   avg_speed(220 * isq:distance[si:kilo(metre)], 2 * si:hour, _Speed).
test('avg_speed2') :-
   avg_speed(isq:height[inch], quantity Time, isq:speed[m/si:hour]),
   Time = _*Q[U],
   Q == isq:time,
   U == si:hour.

test('in as') :-
   qeval(_Speed is (m/s in inch/hour) as isq:speed).

as_data(_ is isq:width[m] as isq:length).
as_data(_ is isq:width[m] / isq:time[s] as isq:speed).

test('as', [forall(as_data(Expr))]) :-
   qeval(Expr).

error_as_data(_ is isq:length[m] as isq:width).

test('error_as', [forall(error_as_data(Expr)), error(domain_error(_, _))]) :-
   qeval(Expr).

test('error_in', [error(domain_error(_, _))]) :-
   qeval(_ is si:hertz in si:becquerel).

test('acceleration') :-
   qeval(Speed is 60 * isq:velocity[km/hour]),
   qeval(Duration is 8 * s),
   qeval(A is (Speed / Duration) as isq:acceleration),
   qeval(B is A in m/s**2),
   qformat(B),
   must_be(isq:acceleration, B).

test('clpBNR') :-
   qeval({A * inch =:= 1 * metre}),
   A == 5000r127,
   qeval({B =:= 5000 * gram / (2*gram)}),
   B == 2500,
   qeval({C is 1^2}),
   C == 1*1[1],
   qeval(D*unit(_) is 0.1),
   must_be(number, D).

test('quantity_point') :-
   qeval(_ is point(42*m)),
   qeval(_ is point(42*kelvin)),
   qeval(_ is point(42*degree_Celsius)),
   qeval(QP1 is point(100*m)),
   qeval(QP2 is point(120*m)),
   qeval(quantity_from_zero(QP1) =:= 100*m),
   qeval(quantity_from_zero(QP2) =:= 120*m),
   qeval(quantity_from(QP2, QP1) =:= 20*m),
   qeval(quantity_from(QP1, QP2) =:= -20*m),
   qeval(QP2 - QP1 =:= 20*m),
   qeval(QP1 - QP2 =:= -20*m).

test('quantity_point_error', [error(_)]) :-
   qeval(QP1 is point(100*isq:distance[m])),
   qeval(QP2 is point(120*isq:height[m])),
   qeval(_ is QP1 + QP2).

test('quantity_point2') :-
   qeval(QP1 is point(100*isq:distance[m])),
   qeval(QP2 is point(120*isq:height[m])),
   qeval(quantity_from(QP2, QP1) =:= 20*m),
   qeval(quantity_from(QP1, QP2) =:= -20*m),
   qeval(QP2 - QP1 =:= 20*m),
   qeval(QP1 - QP2 =:= -20*m).

test('absolute_point_origin') :-
   qeval(QP1 is si:absolute_zero + 100*kelvin),
   qeval(QP2 is 120*kelvin + si:absolute_zero),
   qeval(quantity_from(QP2, QP1) =:= 20*kelvin),
   qeval(quantity_from(QP1, si:absolute_zero) =:= 100*kelvin),
   qeval(quantity_from(QP2, si:absolute_zero) =:= 120*kelvin),
   qeval(quantity_from(QP1, QP2) =:= -20*kelvin),
   qeval(QP1 - si:absolute_zero =:= 100*kelvin),
   qeval(QP2 - si:absolute_zero =:= 120*kelvin),
   qeval(si:absolute_zero - QP1 =:= -100*kelvin),
   qeval(si:absolute_zero - QP2 =:= -120*kelvin).

test('absolute_point_origin_error', [error(_)]) :-
   qeval(_ is quantity_from_zero(si:absolute_zero + 100*kelvin)).

units:absolute_point_origin(oa,isq:distance).
units:relative_point_origin(ob, oa + 10*si:metre).
units:relative_point_origin(oc, ob + 10*si:metre).
units:relative_point_origin(od, oa + 30*si:metre).

test('relative_point_origin') :-
   qeval(QP1 is oc + 100*m),
   qeval(QP2 is od + 120*m),
   QP1 = QP1O + _,
   QP2 = QP2O + _,
   qeval(quantity_from(QP1, QP1O) =:= 100*m),
   qeval(quantity_from(QP2, QP2O) =:= 120*m),
   qeval(quantity_from(QP2, QP1) =:= 30*m),
   qeval(quantity_from(QP1, QP2) =:= -30*m),
   qeval(QP2 - QP1 =:= 30*m),
   qeval(QP1 - QP2 =:= -30*m),
   qeval(quantity_from(QP1, oa) =:= 120*m),
   qeval(quantity_from(QP1, ob) =:= 110*m),
   qeval(quantity_from(QP1, oc) =:= 100*m),
   qeval(quantity_from(QP1, od) =:= 90*m),
   qeval(QP1 - oa =:= 120*m),
   qeval(QP1 - ob =:= 110*m),
   qeval(QP1 - oc =:= 100*m),
   qeval(QP1 - od =:= 90*m),
   qeval(quantity_from(QP2, oa) =:= 150*m),
   qeval(quantity_from(QP2, ob) =:= 140*m),
   qeval(quantity_from(QP2, oc) =:= 130*m),
   qeval(quantity_from(QP2, od) =:= 120*m),
   qeval(QP2 - oa =:= 150*m),
   qeval(QP2 - ob =:= 140*m),
   qeval(QP2 - oc =:= 130*m),
   qeval(QP2 - od =:= 120*m),
   qeval(ob - oa =:= 10*m),
   qeval(oc - oa =:= 20*m),
   qeval(od - oa =:= 30*m),
   qeval(od - oc =:= 10*m),
   qeval(ob - ob =:= 0*m),
   QP2B = origin(QP2BO) + quantity(_),
   qeval(QP2B is point_for(QP2, ob)),
   qeval(quantity_from(QP2B, QP2BO) =:= 140*m),
   QP2A = quantity_point(QP2AO + _),
   qeval(QP2A is point_for(QP2, oa)),
   qeval(quantity_from(QP2A, QP2AO) =:= 150*m),
   qeval(QP2 =:= QP2B),
   qeval(QP2 =:= QP2A),
   true.

test('variable_origin') :-
   qeval(_ is origin(_) + quantity(_)).

test('temperature') :-
   qeval(_ is si:zeroth_degree_Celsius + 20.5 * degree_Celsius),
   qeval(_ is point(20.5 * degree_Celsius)),
   true.

test('radian', [error(_)]) :-
   qeval(_ is m/m in angular:radian).

test('qformat', [
   forall(qformat_data(Expr, ExpectedString))
]) :-
   with_output_to(string(FormattedString), qformat(Expr)),
   assertion(FormattedString == ExpectedString).

qformat_data(10 * si:metre, "10 m").
qformat_data(25 * si:degree, "25°"). % Note: no_space_before_unit_symbol
qformat_data(5 * si:newton, "5 N").
qformat_data(1.5 * si:kilo(si:hertz), "1.5 kHz").
qformat_data(10, "10"). % Dimensionless with unit 1
qformat_data(10 * (si:metre/si:second), "10 m/s").

test(has_type_examples) :-
   qeval(X_len is 10*si:metre),
   must_be(quantity, X_len),
   must_be(isq:length, X_len),
   qeval(P_temp is point(20*si:degree_Celsius)),
   must_be(quantity_point, P_temp).

test(has_type_fail_example, [error(type_error(isq:time, _))]) :-
   qeval(X_len is 10*si:metre),
   must_be(isq:time, X_len).

test(slow_conversion, []) :-
   qeval(_X*foot*lbf/s =:= 1*watt).

test(unitless_with_constraints) :-
   qeval(quantity _ is (3*foot)/(2*m)).

test(systemless_prefix) :-
   qeval(_ is milli(si:second)).

test(prefix_origin) :-
   qeval(_ is (si:zeroth_degree_Celsius + 20*degree_Celsius) quantity_from si:absolute_zero).

test(dimensionless) :-
   qeval(X is 1),
   qeval(_ is X*si:metre).

:- end_tests(units).
