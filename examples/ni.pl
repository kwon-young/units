:- use_module(library(units)).

units:kind(ni:beat_count).
units:kind(ni:midi_clock).
units:kind(ni:sample_count).
units:kind(ni:unit_sample_amount).
units:quantity_parent(ni:beat_count, 1).
units:quantity_parent(ni:midi_clock, 1).
units:quantity_parent(ni:sample_count, 1).
units:quantity_parent(ni:unit_sample_amount, 1).

units:quantity_parent(ni:sample_duration, isq:period_duration).
units:quantity_parent(ni:sampling_rate, isq:frequency).
units:quantity_parent(ni:power, ni:level*ni:level).
units:quantity_parent(ni:beat_duration, isq:period_duration).
units:quantity_parent(ni:tempo, isq:frequency).


units:quantity_formula(ni:sampling_rate, ni:sample_count / ni:sample_duration).
units:quantity_formula(ni:tempo, ni:beat_count / ni:beat_duration).

units:alias_quantity(ni:amplitude, ni:unit_sample_amount).
units:alias_quantity(ni:level, ni:unit_sample_amount).

units:unit_symbol_formula(ni:sample, smpl, 1).
units:unit_symbol_formula(ni:sample_value, pcm, 1).
units:unit_symbol_formula(ni:midi_pulse, p, 1).
units:unit_symbol_formula(ni:quarter_note, q, 1).
units:unit_symbol_formula(ni:half_note, h, 2*ni:quarter_note).
units:unit_symbol_formula(ni:dotted_half_note, 'h.', 3*ni:quarter_note).
units:unit_symbol_formula(ni:whole_note, w, 4*ni:quarter_note).
units:unit_symbol_formula(ni:eight_note, '8th', 1r2*ni:quarter_note).
units:unit_symbol_formula(ni:dotted_quarter_note, 'q.', 3*ni:eight_note).
units:unit_symbol_formula(ni:quarter_note_triplet, qt, 1r3*ni:half_note).
units:unit_symbol_formula(ni:sixteenth_note, '16th', 1r2*ni:eight_note).
units:unit_symbol_formula(ni:dotted_eight_note, '8th.', 3*ni:sixteenth_note).
units:unit_symbol_formula(ni:beat, beat, ni:quarter_note).
units:unit_symbol_formula(ni:beats_per_minute, bpm, ni:beat / si:minute).
units:unit_symbol_formula(ni:midi_pulse_per_quarter, ppqn, ni:midi_pulse / ni:quarter_note).

units:unit_kind(ni:sample, ni:sample_count).
units:unit_kind(ni:sample_value, ni:unit_sample_amount).
units:unit_kind(ni:midi_pulse, ni:midi_clock).
units:unit_kind(ni:quarter_note, ni:beat_count).

main :-
   qeval((
      Sr1 is 44100 * hertz,
      Sr2 is 48 000 * sample / s,
      Samples is 512 * sample,
      SampleTime1 is (Samples / Sr1) in s,
      SampleTime2 is (Samples / Sr2) in ms,
      SampleDuration1 is (1/Sr1) in ms,
      SampleDuration2 is (1/Sr2) in ms,
      RampTime is 35 * ms,
      RampSamples1 is force_as(RampTime * Sr1, ni:sample_count)
   )),
   format("Sample rate 1 is: ~p", [Sr1]), nl,
   format("Sample rate 2 is: ~p", [Sr2]), nl,
   format("~p @ ~p is ~p", [Samples, Sr1, SampleTime1]), nl,
   format("~p @ ~p is ~p", [Samples, Sr2, SampleTime2]), nl,
   format("One sample @ ~p is ~p", [Sr1, SampleDuration1]), nl,
   format("One sample @ ~p is ~p", [Sr2, SampleDuration2]), nl,
   format("~p is ~p @ ~p", [RampTime, RampSamples1, Sr1]), nl,
   true.
