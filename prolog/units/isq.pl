:- module(isq, []).

:- use_module("../units.pl").

% dimensions of base quantities
units:quantity_dimension(isq:length, 'L').
units:quantity_dimension(isq:mass, 'M').
units:quantity_dimension(isq:time, 'T').
units:quantity_dimension(isq:electric_current, 'I').
units:quantity_dimension(isq:thermodynamic_temperature, 'Θ').
units:quantity_dimension(isq:amount_of_substance, 'N').
units:quantity_dimension(isq:luminous_intensity, 'J').
units:quantity_dimension(1, '').

% space and time
units:quantity_parent(isq:width, isq:length).
units:quantity_parent(isq:radius, isq:width).
units:quantity_parent(isq:path_length, isq:length).
units:quantity_parent(isq:area, isq:length**2).
units:quantity_parent(isq:period_duration, isq:duration).
units:quantity_parent(isq:frequency, 1/isq:period_duration).

units:quantity_parent(isq:height, isq:length).
units:quantity_parent(isq:thickness, isq:width).
units:quantity_parent(isq:diameter, isq:width).
units:quantity_parent(isq:distance, isq:path_length).
units:quantity_parent(isq:radial_distance, isq:distance).
units:quantity_parent(isq:position_vector, isq:displacement).
units:quantity_parent(isq:radius_of_curvature, isq:radius).
units:quantity_parent(isq:curvature, 1/isq:radius_of_curvature).
units:quantity_parent(isq:volume, isq:length**3).
units:quantity_parent(isq:phase_angle, isq:angular_measure).
units:quantity_parent(isq:speed, isq:length / isq:time).
units:quantity_parent(isq:acceleration, isq:velocity / isq:duration).
units:quantity_parent(isq:acceleration_of_free_fall, isq:acceleration).
units:quantity_parent(isq:angular_acceleration, isq:angular_velocity / isq:duration).
units:quantity_parent(isq:time_constant, isq:duration).
units:quantity_parent(isq:rotation, 1).
units:quantity_parent(isq:rotational_frequency, isq:rotation / isq:duration).
units:quantity_parent(isq:angular_frequency, isq:phase_angle / isq:duration).
units:quantity_parent(isq:wavelength, isq:length).
units:quantity_parent(isq:repetency, 1/isq:wavelength).
units:quantity_parent(isq:angular_repetency, 1/isq:wavelength).
units:quantity_parent(isq:phase_speed, isq:angular_frequency / isq:angular_repetency).
units:quantity_parent(isq:group_speed, isq:angular_frequency / isq:angular_repetency).
units:quantity_parent(isq:damping_coefficient, 1/isq:time_constant).
units:quantity_parent(isq:attenuation, 1/isq:distance).
units:quantity_parent(isq:phase_coefficient, isq:phase_angle / isq:path_length).
units:quantity_parent(isq:propagation_coefficient, 1/isq:length).

% mechanics
units:quantity_parent(isq:energy, isq:mass*isq:length**2/isq:time**2).
units:quantity_parent(isq:mass_density, isq:mass / isq:volume).
units:quantity_parent(isq:specific_volume, 1/isq:mass_density).
units:quantity_parent(isq:relative_mass_density, isq:mass_density / isq:mass_density).
units:quantity_parent(isq:surface_mass_density, isq:mass / isq:area).
units:quantity_parent(isq:linear_mass_density, isq:mass / isq:length).
units:quantity_parent(isq:momentum, isq:mass* isq:velocity).
units:quantity_parent(isq:force, isq:mass* isq:acceleration).
units:quantity_parent(isq:static_friction_force, isq:force).
units:quantity_parent(isq:kinetic_friction_force, isq:force).
units:quantity_parent(isq:rolling_resistance, isq:force).
units:quantity_parent(isq:drag_force, isq:force).
units:quantity_parent(isq:impulse, isq:force* isq:time).
units:quantity_parent(isq:angular_momentum, isq:position_vector* isq:momentum).
units:quantity_parent(isq:moment_of_force, isq:position_vector* isq:force).
units:quantity_parent(isq:angular_impulse, isq:moment_of_force* isq:time).
units:quantity_parent(isq:gauge_pressure, isq:pressure).
units:quantity_parent(isq:relative_linear_strain, isq:length / isq:length).
units:quantity_parent(isq:relative_volume_strain, isq:volume / isq:volume).
units:quantity_parent(isq:modulus_of_elasticity, isq:normal_stress / isq:relative_linear_strain).
units:quantity_parent(isq:modulus_of_rigidity, isq:shear_stress / isq:shear_strain).
units:quantity_parent(isq:modulus_of_compression, isq:pressure / isq:relative_volume_strain).
units:quantity_parent(isq:compressibility, 1/isq:volume * (isq:volume / isq:pressure)).
units:quantity_parent(isq:second_axial_moment_of_area, isq:radial_distance**2 * isq:area).
units:quantity_parent(isq:second_polar_moment_of_area, isq:radial_distance**2 * isq:area).
units:quantity_parent(isq:section_modulus, isq:second_axial_moment_of_area / isq:radial_distance).
units:quantity_parent(isq:kinematic_viscosity, isq:dynamic_viscosity / isq:mass_density).
units:quantity_parent(isq:power, isq:mass* isq:length**2 / isq:time**3).
units:quantity_parent(isq:mechanical_energy, isq:energy).
units:quantity_parent(isq:potential_energy, isq:mechanical_energy).
units:quantity_parent(isq:mechanical_efficiency, isq:mechanical_power / isq:mechanical_power).
units:quantity_parent(isq:mass_flow, isq:mass_density* isq:velocity).
units:quantity_parent(isq:mass_change_rate, isq:mass / isq:time).
units:quantity_parent(isq:action, isq:energy* isq:time).

% atomic and nuclear physics
units:quantity_parent(isq:activity, 1/isq:duration).
units:quantity_parent(isq:absorbed_dose, isq:energy/isq:mass).
units:quantity_parent(isq:ionizing_radiation_quality_factor, 1).
units:quantity_parent(isq:dose_equivalent, isq:absorbed_dose*isq:ionizing_radiation_quality_factor).

units:quantity_parent(isq:displacement, isq:length).
units:quantity_parent(isq:angular_velocity, isq:angular_displacement / isq:duration).
units:quantity_parent(isq:wave_vector, isq:repetency).
units:quantity_parent(isq:moment_of_inertia, isq:angular_momentum / isq:angular_velocity).
units:quantity_parent(isq:torque, isq:moment_of_force).
units:quantity_parent(isq:pressure, isq:force / isq:area).
units:quantity_parent(isq:stress, isq:pressure).
units:quantity_parent(isq:normal_stress, isq:pressure).
units:quantity_parent(isq:shear_stress, isq:pressure).
units:quantity_parent(isq:strain, 1).
units:quantity_parent(isq:rolling_resistance_factor, isq:force / isq:force).
units:quantity_parent(isq:dynamic_viscosity, isq:shear_stress* isq:length / isq:velocity).
units:quantity_parent(isq:surface_tension, isq:force / isq:length).
units:quantity_parent(isq:mechanical_work, isq:force* isq:displacement).
units:quantity_parent(isq:mass_flow_rate, isq:mass_flow* isq:area).
units:quantity_parent(isq:volume_flow_rate, isq:velocity* isq:area).
units:quantity_parent(isq:angular_measure, 1).
units:quantity_parent(isq:solid_angular_measure, 1).
units:quantity_parent(isq:rotational_displacement, isq:angular_measure).
units:quantity_parent(isq:velocity, isq:speed).
units:quantity_parent(isq:weight, isq:force).
units:quantity_parent(isq:poisson_number, 1).
units:quantity_parent(isq:kinetic_energy, isq:mechanical_energy).
units:quantity_parent(isq:shear_strain, 1).
units:quantity_parent(isq:static_friction_coefficient, 1).
units:quantity_parent(isq:kinetic_friction_factor, 1).
units:quantity_parent(isq:drag_coefficient, 1).
units:quantity_parent(isq:mechanical_power, isq:power).
units:quantity_parent(isq:logarithmic_decrement, 1).

units:quantity_formula(isq:angular_measure, isq:arc_length/isq:radius).
units:quantity_formula(isq:solid_angular_measure, isq:area/isq:radius**2).
units:quantity_formula(isq:rotational_displacement, isq:path_length / isq:radius).
units:quantity_formula(isq:velocity, isq:displacement / isq:duration).
units:quantity_formula(isq:weight, isq:mass* isq:acceleration_of_free_fall).
units:quantity_formula(isq:poisson_number, isq:width / isq:length).
units:quantity_formula(isq:kinetic_energy, isq:mass* isq:speed**2).
units:quantity_formula(isq:shear_strain, isq:displacement / isq:thickness).
units:quantity_formula(isq:static_friction_coefficient, isq:static_friction_force / isq:force).
units:quantity_formula(isq:kinetic_friction_factor, isq:kinetic_friction_force / isq:force).
units:quantity_formula(isq:drag_coefficient, isq:drag_force / (isq:mass_density * isq:speed**2 * isq:area)).
units:quantity_formula(isq:mechanical_power, isq:force* isq:velocity).
units:quantity_formula(isq:logarithmic_decrement, isq:damping_coefficient* isq:period_duration).

units:alias_quantity(isq:duration, isq:time).
units:alias_quantity(isq:breadth, isq:width).
units:alias_quantity(isq:arc_length, isq:path_length).
units:alias_quantity(isq:period, isq:period_duration).
units:alias_quantity(isq:depth, isq:height).
units:alias_quantity(isq:altitude, isq:height).
units:alias_quantity(isq:angular_displacement, isq:rotational_displacement).
units:alias_quantity(isq:wavenumber, isq:repetency).
units:alias_quantity(isq:angular_wavenumber, isq:angular_repetency).
units:alias_quantity(isq:extinction, isq:attenuation).
units:alias_quantity(isq:density, isq:mass_density).
units:alias_quantity(isq:relative_density, isq:relative_mass_density).
units:alias_quantity(isq:surface_density, isq:surface_mass_density).
units:alias_quantity(isq:linear_density, isq:linear_mass_density).
units:alias_quantity(isq:static_friction, isq:static_friction_force).
units:alias_quantity(isq:dynamic_friction_force, isq:kinetic_friction_force).
units:alias_quantity(isq:rolling_drag, isq:rolling_resistance).
units:alias_quantity(isq:rolling_friction_force, isq:rolling_resistance).
units:alias_quantity(isq:young_modulus, isq:modulus_of_elasticity).
units:alias_quantity(isq:shear_modulus, isq:modulus_of_rigidity).
units:alias_quantity(isq:bulk_modulus, isq:modulus_of_compression).
units:alias_quantity(isq:static_friction_factor, isq:static_friction_coefficient).
units:alias_quantity(isq:coefficient_of_static_friction, isq:static_friction_coefficient).
units:alias_quantity(isq:dynamic_friction_factor, isq:kinetic_friction_factor).
units:alias_quantity(isq:drag_factor, isq:drag_coefficient).
units:alias_quantity(isq:work, isq:mechanical_work).

% quantity_character_(displacement, isq:vector).
% quantity_character_(angular_velocity, isq:vector).
% quantity_character_(wave_vector, isq:vector).
% quantity_character_(moment_of_inertia, isq:tensor).
% quantity_character_(torque, isq:real_scalar).
% quantity_character_(pressure, isq:real_scalar).
% quantity_character_(stress, isq:tensor).
% quantity_character_(normal_stress, isq:real_scalar).
% quantity_character_(shear_stress, isq:real_scalar).
% quantity_character_(strain, isq:tensor).
% quantity_character_(shear_strain, isq:real_scalar).
% quantity_character_(static_friction_coefficient, isq:real_scalar).
% quantity_character_(kinetic_friction_factor, isq:real_scalar).
% quantity_character_(rolling_resistance_factor, isq:real_scalar).
% quantity_character_(drag_coefficient, isq:real_scalar).
% quantity_character_(dynamic_viscosity, isq:real_scalar).
% quantity_character_(surface_tension, isq:real_scalar).
% quantity_character_(mechanical_power, isq:real_scalar).
% quantity_character_(mechanical_work, isq:real_scalar).
% quantity_character_(mass_flow_rate, isq:real_scalar).
% quantity_character_(volume_flow_rate, isq:real_scalar).
% quantity_character_(1, isq:real_scalar).
% quantity_character_(time, isq:real_scalar).
%
% :- table quantity_character/2.
%
% quantity_character(Quantity, Character) :-
%    quantity_character_(Quantity, Character).
% quantity_character(Alias, Character) :-
%    alias(Alias, System:Quantity),
%    System:quantity_character(Quantity, Character).

% quantity_character(Quantity, Character) :-
%    quantity_call(quantity_character, Quantity, Character).
% quantity_character(AB, Character) :-
%    subsumes_term(A*B, AB),
%    AB = A*B,
%    quantity_character(A, AC),
%    quantity_character(B, BC),
%    character_op(AC*BC, Character).
% quantity_character(AB, Character) :-
%    subsumes_term(A/B, AB),
%    AB = A/B,
%    quantity_character(A, AC),
%    quantity_character(B, BC),
%    character_op(AC/BC, Character).
% quantity_character(Quantity, Character) :-
%    quantity_call(quantity_formula, Quantity, Parent),
%    quantity_character(Parent, Character).
% quantity_character(Quantity, Character) :-
%    \+ quantity_call(quantity_formula, Quantity, _),
%    \+ quantity_call(quantity_character, Quantity, _),
%    quantity_call(quantity_parent, Quantity, Parent),
%    quantity_character(Parent, Character).

% character_op(isq:real_scalar*isq:real_scalar, isq:real_scalar).
% character_op(isq:real_scalar/isq:real_scalar, isq:real_scalar).
% character_op(isq:real_scalar*isq:vector, isq:vector).
% character_op(isq:vector*isq:real_scalar, isq:vector).
% character_op(isq:real_scalar/isq:vector, isq:vector).
% character_op(isq:vector/isq:real_scalar, isq:vector).
% character_op(isq:real_scalar**_, isq:real_scalar).
% character_op(isq:vector**_, isq:vector).

units:kind(isq:angular_measure).
units:kind(isq:solid_angular_measure).
