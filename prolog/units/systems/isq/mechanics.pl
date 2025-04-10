:-module(mechanics,[]).
units:alias(isq:density,isq:mass_density).
units:alias(isq:relative_density,isq:relative_mass_density).
units:alias(isq:surface_density,isq:surface_mass_density).
units:alias(isq:linear_density,isq:linear_mass_density).
units:alias(isq:static_friction,isq:static_friction_force).
units:alias(isq:dynamic_friction_force,isq:kinetic_friction_force).
units:alias(isq:rolling_drag,isq:rolling_resistance).
units:alias(isq:rolling_friction_force,isq:rolling_resistance).
units:alias(isq:'Young_modulus',isq:modulus_of_elasticity).
units:alias(isq:shear_modulus,isq:modulus_of_rigidity).
units:alias(isq:bulk_modulus,isq:modulus_of_compression).
units:alias(isq:static_friction_factor,isq:static_friction_coefficient).
units:alias(isq:coefficient_of_static_friction,isq:static_friction_coefficient).
units:alias(isq:dynamic_friction_factor,isq:kinetic_friction_factor).
units:alias(isq:drag_factor,isq:drag_coefficient).
units:alias(isq:work,isq:mechanical_work).
units:quantity_character(isq:moment_of_inertia,quantity_character:tensor).
units:quantity_character(isq:torque,quantity_character:real_scalar).
units:quantity_character(isq:pressure,quantity_character:real_scalar).
units:quantity_character(isq:stress,quantity_character:tensor).
units:quantity_character(isq:normal_stress,quantity_character:real_scalar).
units:quantity_character(isq:shear_stress,quantity_character:real_scalar).
units:quantity_character(isq:strain,quantity_character:tensor).
units:quantity_character(isq:shear_strain,quantity_character:real_scalar).
units:quantity_character(isq:static_friction_coefficient,quantity_character:real_scalar).
units:quantity_character(isq:kinetic_friction_factor,quantity_character:real_scalar).
units:quantity_character(isq:rolling_resistance_factor,quantity_character:real_scalar).
units:quantity_character(isq:drag_coefficient,quantity_character:real_scalar).
units:quantity_character(isq:dynamic_viscosity,quantity_character:real_scalar).
units:quantity_character(isq:surface_tension,quantity_character:real_scalar).
units:quantity_character(isq:mechanical_power,quantity_character:real_scalar).
units:quantity_character(isq:mechanical_work,quantity_character:real_scalar).
units:quantity_character(isq:mass_flow_rate,quantity_character:real_scalar).
units:quantity_character(isq:volume_flow_rate,quantity_character:real_scalar).
units:quantity_formula(isq:weight,(isq:mass)*(isq:acceleration_of_free_fall)).
units:quantity_formula(isq:shear_strain,(isq:displacement)/(isq:thickness)).
units:quantity_formula(isq:'Poisson_number',(isq:width)/(isq:length)).
units:quantity_formula(isq:static_friction_coefficient,(isq:static_friction_force)/(isq:force)).
units:quantity_formula(isq:kinetic_friction_factor,(isq:kinetic_friction_force)/(isq:force)).
units:quantity_formula(isq:drag_coefficient,(isq:drag_force)/((isq:mass_density)*(isq:speed)**2*(isq:area))).
units:quantity_formula(isq:mechanical_power,(isq:force)*(isq:velocity)).
units:quantity_formula(isq:kinetic_energy,(isq:mass)*(isq:speed)**2).
units:quantity_parent(isq:mass_density,(isq:mass)/(isq:volume)).
units:quantity_parent(isq:specific_volume,1/(isq:mass_density)).
units:quantity_parent(isq:relative_mass_density,(isq:mass_density)/(isq:mass_density)).
units:quantity_parent(isq:surface_mass_density,(isq:mass)/(isq:area)).
units:quantity_parent(isq:linear_mass_density,(isq:mass)/(isq:length)).
units:quantity_parent(isq:momentum,(isq:mass)*(isq:velocity)).
units:quantity_parent(isq:force,(isq:mass)*(isq:acceleration)).
units:quantity_parent(isq:weight,isq:force).
units:quantity_parent(isq:static_friction_force,isq:force).
units:quantity_parent(isq:kinetic_friction_force,isq:force).
units:quantity_parent(isq:rolling_resistance,isq:force).
units:quantity_parent(isq:drag_force,isq:force).
units:quantity_parent(isq:impulse,(isq:force)*(isq:time)).
units:quantity_parent(isq:angular_momentum,(isq:position_vector)*(isq:momentum)).
units:quantity_parent(isq:moment_of_inertia,(isq:angular_momentum)/(isq:angular_velocity)).
units:quantity_parent(isq:moment_of_force,(isq:position_vector)*(isq:force)).
units:quantity_parent(isq:torque,isq:moment_of_force).
units:quantity_parent(isq:angular_impulse,(isq:moment_of_force)*(isq:time)).
units:quantity_parent(isq:pressure,(isq:force)/(isq:area)).
units:quantity_parent(isq:gauge_pressure,isq:pressure).
units:quantity_parent(isq:stress,isq:pressure).
units:quantity_parent(isq:normal_stress,isq:pressure).
units:quantity_parent(isq:shear_stress,isq:pressure).
units:quantity_parent(isq:strain,1).
units:quantity_parent(isq:relative_linear_strain,(isq:length)/(isq:length)).
units:quantity_parent(isq:shear_strain,1).
units:quantity_parent(isq:relative_volume_strain,(isq:volume)/(isq:volume)).
units:quantity_parent(isq:'Poisson_number',1).
units:quantity_parent(isq:modulus_of_elasticity,(isq:normal_stress)/(isq:relative_linear_strain)).
units:quantity_parent(isq:modulus_of_rigidity,(isq:shear_stress)/(isq:shear_strain)).
units:quantity_parent(isq:modulus_of_compression,(isq:pressure)/(isq:relative_volume_strain)).
units:quantity_parent(isq:compressibility,1/(isq:volume)*((isq:volume)/(isq:pressure))).
units:quantity_parent(isq:second_axial_moment_of_area,(isq:radial_distance)**2*(isq:area)).
units:quantity_parent(isq:second_polar_moment_of_area,(isq:radial_distance)**2*(isq:area)).
units:quantity_parent(isq:section_modulus,(isq:second_axial_moment_of_area)/(isq:radial_distance)).
units:quantity_parent(isq:static_friction_coefficient,1).
units:quantity_parent(isq:kinetic_friction_factor,1).
units:quantity_parent(isq:rolling_resistance_factor,(isq:force)/(isq:force)).
units:quantity_parent(isq:drag_coefficient,1).
units:quantity_parent(isq:dynamic_viscosity,(isq:shear_stress)*(isq:length)/(isq:velocity)).
units:quantity_parent(isq:kinematic_viscosity,(isq:dynamic_viscosity)/(isq:mass_density)).
units:quantity_parent(isq:surface_tension,(isq:force)/(isq:length)).
units:quantity_parent(isq:power,(isq:mass)*(isq:length)**2/(isq:time)**3).
units:quantity_parent(isq:mechanical_power,isq:power).
units:quantity_parent(isq:mechanical_energy,isq:energy).
units:quantity_parent(isq:potential_energy,isq:mechanical_energy).
units:quantity_parent(isq:kinetic_energy,isq:mechanical_energy).
units:quantity_parent(isq:mechanical_work,(isq:force)*(isq:displacement)).
units:quantity_parent(isq:mechanical_efficiency,(isq:mechanical_power)/(isq:mechanical_power)).
units:quantity_parent(isq:mass_flow,(isq:mass_density)*(isq:velocity)).
units:quantity_parent(isq:mass_flow_rate,(isq:mass_flow)*(isq:area)).
units:quantity_parent(isq:mass_change_rate,(isq:mass)/(isq:time)).
units:quantity_parent(isq:volume_flow_rate,(isq:velocity)*(isq:area)).
units:quantity_parent(isq:action,(isq:energy)*(isq:time)).
