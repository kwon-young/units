:- module(isq, [base_dimension/2, quantity_parent/2, quantity_formula/2,
                quantity_character/2, quantity/1, kind/1, alias/2]).

:- use_module(units_utils).

units_utils:system(quantity, isq).

% dimensions of base quantities
base_dimension_(length, 'L').
base_dimension_(mass, 'M').
base_dimension_(time, 'T').
base_dimension_(electric_current, 'I').
base_dimension_(thermodynamic_temperature, 'Θ').
base_dimension_(amount_of_substance, 'N').
base_dimension_(luminous_intensity, 'J').
base_dimension_(1, '').

:- table base_dimension/2.

base_dimension(Dimension, Symbol) :-
   base_dimension_(Dimension, Symbol).
base_dimension(Alias, Symbol) :-
   alias(Alias, System:Dimension),
   System:base_dimension(Dimension, Symbol).

% space and time
quantity_parent_(width, isq:length).
quantity_parent_(radius, isq:width).
quantity_parent_(path_length, isq:length).
quantity_parent_(area, isq:length**2).
quantity_parent_(period_duration, isq:duration).
quantity_parent_(frequency, 1/isq:period_duration).

quantity_parent_(height, isq:length).
quantity_parent_(thickness, isq:width).
quantity_parent_(diameter, isq:width).
quantity_parent_(distance, isq:path_length).
quantity_parent_(radial_distance, isq:distance).
quantity_parent_(position_vector, isq:displacement).
quantity_parent_(radius_of_curvature, isq:radius).
quantity_parent_(curvature, 1/isq:radius_of_curvature).
quantity_parent_(volume, isq:length**3).
quantity_parent_(phase_angle, isq:angular_measure).
quantity_parent_(speed, isq:length / isq:time).
quantity_parent_(acceleration, isq:velocity / isq:duration).
quantity_parent_(acceleration_of_free_fall, isq:acceleration).
quantity_parent_(angular_acceleration, isq:angular_velocity / isq:duration).
quantity_parent_(time_constant, isq:duration).
quantity_parent_(rotation, 1).
quantity_parent_(rotational_frequency, isq:rotation / isq:duration).
quantity_parent_(angular_frequency, isq:phase_angle / isq:duration).
quantity_parent_(wavelength, isq:length).
quantity_parent_(repetency, 1/isq:wavelength).
quantity_parent_(angular_repetency, 1/isq:wavelength).
quantity_parent_(phase_speed, isq:angular_frequency / isq:angular_repetency).
quantity_parent_(group_speed, isq:angular_frequency / isq:angular_repetency).
quantity_parent_(damping_coefficient, 1/isq:time_constant).
quantity_parent_(attenuation, 1/isq:distance).
quantity_parent_(phase_coefficient, isq:phase_angle / isq:path_length).
quantity_parent_(propagation_coefficient, 1/isq:length).

% mechanics
quantity_parent_(energy, isq:mass*isq:length**2/isq:time**2).
quantity_parent_(mass_density, isq:mass / isq:volume).
quantity_parent_(specific_volume, 1/isq:mass_density).
quantity_parent_(relative_mass_density, isq:mass_density / isq:mass_density).
quantity_parent_(surface_mass_density, isq:mass / isq:area).
quantity_parent_(linear_mass_density, isq:mass / isq:length).
quantity_parent_(momentum, isq:mass* isq:velocity).
quantity_parent_(force, isq:mass* isq:acceleration).
quantity_parent_(static_friction_force, isq:force).
quantity_parent_(kinetic_friction_force, isq:force).
quantity_parent_(rolling_resistance, isq:force).
quantity_parent_(drag_force, isq:force).
quantity_parent_(impulse, isq:force* isq:time).
quantity_parent_(angular_momentum, isq:position_vector* isq:momentum).
quantity_parent_(moment_of_force, isq:position_vector* isq:force).
quantity_parent_(angular_impulse, isq:moment_of_force* isq:time).
quantity_parent_(gauge_pressure, isq:pressure).
quantity_parent_(relative_linear_strain, isq:length / isq:length).
quantity_parent_(relative_volume_strain, isq:volume / isq:volume).
quantity_parent_(modulus_of_elasticity, isq:normal_stress / isq:relative_linear_strain).
quantity_parent_(modulus_of_rigidity, isq:shear_stress / isq:shear_strain).
quantity_parent_(modulus_of_compression, isq:pressure / isq:relative_volume_strain).
quantity_parent_(compressibility, 1/isq:volume * (isq:volume / isq:pressure)).
quantity_parent_(second_axial_moment_of_area, isq:radial_distance**2 * isq:area).
quantity_parent_(second_polar_moment_of_area, isq:radial_distance**2 * isq:area).
quantity_parent_(section_modulus, isq:second_axial_moment_of_area / isq:radial_distance).
quantity_parent_(kinematic_viscosity, isq:dynamic_viscosity / isq:mass_density).
quantity_parent_(power, isq:mass* isq:length**2 / isq:time**3).
quantity_parent_(mechanical_energy, isq:energy).
quantity_parent_(potential_energy, isq:mechanical_energy).
quantity_parent_(mechanical_efficiency, isq:mechanical_power / isq:mechanical_power).
quantity_parent_(mass_flow, isq:mass_density* isq:velocity).
quantity_parent_(mass_change_rate, isq:mass / isq:time).
quantity_parent_(action, isq:energy* isq:time).

% atomic and nuclear physics
quantity_parent_(activity, 1/isq:duration).
quantity_parent_(absorbed_dose, isq:energy/isq:mass).
quantity_parent_(ionizing_radiation_quality_factor, 1).
quantity_parent_(dose_equivalent, isq:absorbed_dose*isq:ionizing_radiation_quality_factor).

quantity_parent_(displacement, isq:length).
quantity_parent_(angular_velocity, isq:angular_displacement / isq:duration).
quantity_parent_(wave_vector, isq:repetency).
quantity_parent_(moment_of_inertia, isq:angular_momentum / isq:angular_velocity).
quantity_parent_(torque, isq:moment_of_force).
quantity_parent_(pressure, isq:force / isq:area).
quantity_parent_(stress, isq:pressure).
quantity_parent_(normal_stress, isq:pressure).
quantity_parent_(shear_stress, isq:pressure).
quantity_parent_(strain, 1).
quantity_parent_(rolling_resistance_factor, isq:force / isq:force).
quantity_parent_(dynamic_viscosity, isq:shear_stress* isq:length / isq:velocity).
quantity_parent_(surface_tension, isq:force / isq:length).
quantity_parent_(mechanical_work, isq:force* isq:displacement).
quantity_parent_(mass_flow_rate, isq:mass_flow* isq:area).
quantity_parent_(volume_flow_rate, isq:velocity* isq:area).
quantity_parent_(angular_measure, 1).
quantity_parent_(solid_angular_measure, 1).
quantity_parent_(rotational_displacement, isq:angular_measure).
quantity_parent_(velocity, isq:speed).
quantity_parent_(weight, isq:force).
quantity_parent_(poisson_number, 1).
quantity_parent_(kinetic_energy, isq:mechanical_energy).
quantity_parent_(shear_strain, 1).
quantity_parent_(static_friction_coefficient, 1).
quantity_parent_(kinetic_friction_factor, 1).
quantity_parent_(drag_coefficient, 1).
quantity_parent_(mechanical_power, isq:power).
quantity_parent_(logarithmic_decrement, 1).

:- table quantity_parent/2.

quantity_parent(Quantity, Parent) :-
   quantity_parent_(Quantity, Parent).
quantity_parent(Alias, Parent) :-
   alias(Alias, System:Quantity),
   System:quantity_parent(Quantity, Parent).

quantity_formula_(angular_measure, isq:arc_length/isq:radius).
quantity_formula_(solid_angular_measure, isq:area/isq:radius**2).
quantity_formula_(rotational_displacement, isq:path_length / isq:radius).
quantity_formula_(velocity, isq:displacement / isq:duration).
quantity_formula_(weight, isq:mass* isq:acceleration_of_free_fall).
quantity_formula_(poisson_number, isq:width / isq:length).
quantity_formula_(kinetic_energy, isq:mass* isq:speed**2).
quantity_formula_(shear_strain, isq:displacement / isq:thickness).
quantity_formula_(static_friction_coefficient, isq:static_friction_force / isq:force).
quantity_formula_(kinetic_friction_factor, isq:kinetic_friction_force / isq:force).
quantity_formula_(drag_coefficient, isq:drag_force / (isq:mass_density * isq:speed**2 * isq:area)).
quantity_formula_(mechanical_power, isq:force* isq:velocity).
quantity_formula_(logarithmic_decrement, isq:damping_coefficient* isq:period_duration).

:- table quantity_formula/2.

quantity_formula(Quantity, Formula) :-
   quantity_formula_(Quantity, Formula).
quantity_formula(Alias, Formula) :-
   alias(Alias, System:Quantity),
   System:quantity_formula(Quantity, Formula).

alias(duration, isq:time).
alias(breadth, isq:width).
alias(arc_length, isq:path_length).
alias(period, isq:period_duration).
alias(depth, isq:height).
alias(altitude, isq:height).
alias(angular_displacement, isq:rotational_displacement).
alias(wavenumber, isq:repetency).
alias(angular_wavenumber, isq:angular_repetency).
alias(extinction, isq:attenuation).
alias(density, isq:mass_density).
alias(relative_density, isq:relative_mass_density).
alias(surface_density, isq:surface_mass_density).
alias(linear_density, isq:linear_mass_density).
alias(static_friction, isq:static_friction_force).
alias(dynamic_friction_force, isq:kinetic_friction_force).
alias(rolling_drag, isq:rolling_resistance).
alias(rolling_friction_force, isq:rolling_resistance).
alias(young_modulus, isq:modulus_of_elasticity).
alias(shear_modulus, isq:modulus_of_rigidity).
alias(bulk_modulus, isq:modulus_of_compression).
alias(static_friction_factor, isq:static_friction_coefficient).
alias(coefficient_of_static_friction, isq:static_friction_coefficient).
alias(dynamic_friction_factor, isq:kinetic_friction_factor).
alias(drag_factor, isq:drag_coefficient).
alias(work, isq:mechanical_work).

quantity_character_(displacement, isq:vector).
quantity_character_(angular_velocity, isq:vector).
quantity_character_(wave_vector, isq:vector).
quantity_character_(moment_of_inertia, isq:tensor).
quantity_character_(torque, isq:real_scalar).
quantity_character_(pressure, isq:real_scalar).
quantity_character_(stress, isq:tensor).
quantity_character_(normal_stress, isq:real_scalar).
quantity_character_(shear_stress, isq:real_scalar).
quantity_character_(strain, isq:tensor).
quantity_character_(shear_strain, isq:real_scalar).
quantity_character_(static_friction_coefficient, isq:real_scalar).
quantity_character_(kinetic_friction_factor, isq:real_scalar).
quantity_character_(rolling_resistance_factor, isq:real_scalar).
quantity_character_(drag_coefficient, isq:real_scalar).
quantity_character_(dynamic_viscosity, isq:real_scalar).
quantity_character_(surface_tension, isq:real_scalar).
quantity_character_(mechanical_power, isq:real_scalar).
quantity_character_(mechanical_work, isq:real_scalar).
quantity_character_(mass_flow_rate, isq:real_scalar).
quantity_character_(volume_flow_rate, isq:real_scalar).
quantity_character_(1, isq:real_scalar).
quantity_character_(time, isq:real_scalar).

:- table quantity_character/2.

quantity_character(Quantity, Character) :-
   quantity_character_(Quantity, Character).
quantity_character(Alias, Character) :-
   alias(Alias, System:Quantity),
   System:quantity_character(Quantity, Character).

:- table quantity/1.

quantity(Quantity) :-
   (  base_dimension(Quantity, _)
   ;  quantity_parent(Quantity, _)
   ).

:- table kind/1.

kind(angular_measure).
kind(solid_angular_measure).
