:- module(isq, []).

% dimensions of base quantities
base_dimension(dim_length, "L").
base_dimension(dim_mass, "M").
base_dimension(dim_time, "T").
base_dimension(dim_electric_current, "I").
base_dimension(dim_thermodynamic_temperature, "Θ").
base_dimension(dim_amount_of_substance, "N").
base_dimension(dim_luminous_intensity, "J").
base_dimension(dimensionless, "").

:- table quantity_parent/2.

% base quantities
quantity_parent(length, dim_length).
quantity_parent(mass, dim_mass).
quantity_parent(time, dim_time).
quantity_parent(electric_current, dim_electric_current).
quantity_parent(thermodynamic_temperature, dim_thermodynamic_temperature).
quantity_parent(amount_of_substance, dim_amount_of_substance).
quantity_parent(luminous_intensity, dim_luminous_intensity).
quantity_parent(1, dimensionless).

% space and time
quantity_parent(width, length).
quantity_parent(radius, width).
quantity_parent(path_length, length).
quantity_parent(area, length^2).
quantity_parent(period_duration, duration).
quantity_parent(frequency, 1/period_duration).

quantity_parent(height, length).
quantity_parent(thickness, width).
quantity_parent(diameter, width).
quantity_parent(distance, path_length).
quantity_parent(radial_distance, distance).
quantity_parent(position_vector, displacement).
quantity_parent(radius_of_curvature, radius).
quantity_parent(curvature, 1/radius_of_curvature).
quantity_parent(volume, length^3).
quantity_parent(phase_angle, angular_measure).
quantity_parent(speed, length / time).
quantity_parent(acceleration, velocity / duration).
quantity_parent(acceleration_of_free_fall, acceleration).
quantity_parent(angular_acceleration, angular_velocity / duration).
quantity_parent(time_constant, duration).
quantity_parent(rotation, 1).
quantity_parent(rotational_frequency, rotation / duration).
quantity_parent(angular_frequency, phase_angle / duration).
quantity_parent(wavelength, length).
quantity_parent(repetency, 1/wavelength).
quantity_parent(angular_repetency, 1/wavelength).
quantity_parent(phase_speed, angular_frequency / angular_repetency).
quantity_parent(group_speed, angular_frequency / angular_repetency).
quantity_parent(damping_coefficient, 1/time_constant).
quantity_parent(attenuation, 1/distance).
quantity_parent(phase_coefficient, phase_angle / path_length).
quantity_parent(propagation_coefficient, 1/length).

% mechanics
quantity_parent(energy, mass*length^2/time^2).
quantity_parent(mass_density, mass / volume).
quantity_parent(specific_volume, 1/mass_density).
quantity_parent(relative_mass_density, mass_density / mass_density).
quantity_parent(surface_mass_density, mass / area).
quantity_parent(linear_mass_density, mass / length).
quantity_parent(momentum, mass* velocity).
quantity_parent(force, mass* acceleration).
quantity_parent(static_friction_force, force).
quantity_parent(kinetic_friction_force, force).
quantity_parent(rolling_resistance, force).
quantity_parent(drag_force, force).
quantity_parent(impulse, force* time).
quantity_parent(angular_momentum, position_vector* momentum).
quantity_parent(moment_of_force, position_vector* force).
quantity_parent(angular_impulse, moment_of_force* time).
quantity_parent(gauge_pressure, pressure).
quantity_parent(relative_linear_strain, length / length).
quantity_parent(relative_volume_strain, volume / volume).
quantity_parent(modulus_of_elasticity, normal_stress / relative_linear_strain).
quantity_parent(modulus_of_rigidity, shear_stress / shear_strain).
quantity_parent(modulus_of_compression, pressure / relative_volume_strain).
quantity_parent(compressibility, 1/volume * (volume / pressure)).
quantity_parent(second_axial_moment_of_area, radial_distance^2 * area).
quantity_parent(second_polar_moment_of_area, radial_distance^2 * area).
quantity_parent(section_modulus, second_axial_moment_of_area / radial_distance).
quantity_parent(kinematic_viscosity, dynamic_viscosity / mass_density).
quantity_parent(power, mass* length^2 / time^3).
quantity_parent(mechanical_energy, energy).
quantity_parent(potential_energy, mechanical_energy).
quantity_parent(mechanical_efficiency, mechanical_power / mechanical_power).
quantity_parent(mass_flow, mass_density* velocity).
quantity_parent(mass_change_rate, mass / time).
quantity_parent(action, energy* time).

% atomic and nuclear physics
quantity_parent(activity, 1/duration).
quantity_parent(absorbed_dose, energy/mass).
quantity_parent(ionizing_radiation_quality_factor, 1).
quantity_parent(dose_equivalent, absorbed_dose*ionizing_radiation_quality_factor).

quantity_parent(displacement, length).
quantity_parent(angular_velocity, angular_displacement / duration).
quantity_parent(wave_vector, repetency).
quantity_parent(moment_of_inertia, angular_momentum / angular_velocity).
quantity_parent(torque, moment_of_force).
quantity_parent(pressure, force / area).
quantity_parent(stress, pressure).
quantity_parent(normal_stress, pressure).
quantity_parent(shear_stress, pressure).
quantity_parent(strain, 1).
quantity_parent(rolling_resistance_factor, force / force).
quantity_parent(dynamic_viscosity, shear_stress* length / velocity).
quantity_parent(surface_tension, force / length).
quantity_parent(mechanical_work, force* displacement).
quantity_parent(mass_flow_rate, mass_flow* area).
quantity_parent(volume_flow_rate, velocity* area).
quantity_parent(angular_measure, 1).
quantity_parent(solid_angular_measure, 1).
quantity_parent(rotational_displacement, angular_measure).
quantity_parent(velocity, speed).
quantity_parent(weight, force).
quantity_parent(poisson_number, 1).
quantity_parent(kinetic_energy, mechanical_energy).
quantity_parent(shear_strain, 1).
quantity_parent(static_friction_coefficient, 1).
quantity_parent(kinetic_friction_factor, 1).
quantity_parent(drag_coefficient, 1).
quantity_parent(mechanical_power, power).
quantity_parent(logarithmic_decrement, 1).
quantity_parent(Alias, Parent) :-
   alias(Alias, Quantity),
   quantity_parent(Quantity, Parent).

quantity_formula(angular_measure, arc_length/radius).
quantity_formula(solid_angular_measure, area/radius^2).
quantity_formula(rotational_displacement, path_length / radius).
quantity_formula(velocity, displacement / duration).
quantity_formula(weight, mass* acceleration_of_free_fall).
quantity_formula(poisson_number, width / length).
quantity_formula(kinetic_energy, mass* speed^2).
quantity_formula(shear_strain, displacement / thickness).
quantity_formula(static_friction_coefficient, static_friction_force / force).
quantity_formula(kinetic_friction_factor, kinetic_friction_force / force).
quantity_formula(drag_coefficient, drag_force / (mass_density * speed^2 * area)).
quantity_formula(mechanical_power, force* velocity).
quantity_formula(logarithmic_decrement, damping_coefficient* period_duration).


alias(duration, time).
alias(breadth, width).
alias(arc_length, path_length).
alias(period, period_duration).
alias(depth, height).
alias(altitude, height).
alias(angular_displacement, rotational_displacement).
alias(wavenumber, repetency).
alias(angular_wavenumber, angular_repetency).
alias(extinction, attenuation).
alias(density, mass_density).
alias(relative_density, relative_mass_density).
alias(surface_density, surface_mass_density).
alias(linear_density, linear_mass_density).
alias(static_friction, static_friction_force).
alias(dynamic_friction_force, kinetic_friction_force).
alias(rolling_drag, rolling_resistance).
alias(rolling_friction_force, rolling_resistance).
alias(young_modulus, modulus_of_elasticity).
alias(shear_modulus, modulus_of_rigidity).
alias(bulk_modulus, modulus_of_compression).
alias(static_friction_factor, static_friction_coefficient).
alias(coefficient_of_static_friction, static_friction_coefficient).
alias(dynamic_friction_factor, kinetic_friction_factor).
alias(drag_factor, drag_coefficient).
alias(work, mechanical_work).

:- table kind/1.

kind(angular_measure).
kind(solid_angular_measure).
kind(Kind) :-
   quantity_root(_, Kind).

quantity_kind(Quantity, Root) :-
   quantity_root(Quantity, Root).

quantity_kinds(Quantity, Kinds) :-
   findall(Kind, quantity_kind(Quantity, Kind), Kinds).

:- table quantity_root/2.

quantity_root(Alias, Ancestor) :-
   alias(Alias, Quantity),
   quantity_root(Quantity, Ancestor).
quantity_root(Quantity, Ancestor) :-
   quantity_parent(Quantity, Parent),
   quantity_root(Parent, Ancestor).
quantity_root(Quantity, Quantity) :-
   quantity_parent(Quantity, Parent),
   \+ quantity_parent(Parent, _).

:- table quantity_child/2.

quantity_child(Quantity, Child) :-
   quantity_root(Quantity, Root),
   quantity_child_(Root, Child),
   kind(Child).
quantity_child_(Q, Q).
quantity_child_(Parent, Child) :-
   quantity_parent(Quantity, Parent),
   quantity_child_(Quantity, Child).
quantity_child_(AB, AChild/BChild) :-
   subsumes_term(A/B, AB),
   AB = A/B,
   quantity_child_(A, AChild),
   quantity_child_(B, BChild).
quantity_child_(AB, AChild*BChild) :-
   subsumes_term(A*B, AB),
   AB = A*B,
   quantity_child_(A, AChild),
   quantity_child_(B, BChild).
quantity_child_(AB, AChild^B) :-
   subsumes_term(A^B, AB),
   AB = A^B,
   quantity_child_(A, AChild).

quantity_character(displacement, vector).
quantity_character(angular_velocity, vector).
quantity_character(wave_vector, vector).
quantity_character(moment_of_inertia, tensor).
quantity_character(torque, real_scalar).
quantity_character(pressure, real_scalar).
quantity_character(stress, tensor).
quantity_character(normal_stress, real_scalar).
quantity_character(shear_stress, real_scalar).
quantity_character(strain, tensor).
quantity_character(shear_strain, real_scalar).
quantity_character(static_friction_coefficient, real_scalar).
quantity_character(kinetic_friction_factor, real_scalar).
quantity_character(rolling_resistance_factor, real_scalar).
quantity_character(drag_coefficient, real_scalar).
quantity_character(dynamic_viscosity, real_scalar).
quantity_character(surface_tension, real_scalar).
quantity_character(mechanical_power, real_scalar).
quantity_character(mechanical_work, real_scalar).
quantity_character(mass_flow_rate, real_scalar).
quantity_character(volume_flow_rate, real_scalar).
