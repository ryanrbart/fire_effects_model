# R

This folder contains the code for testing and analyzing the fire effects model in RHESSys. The code is designed to run in sequential order, with location specific scripts denoted with a *** referring to RS (Rattlesnake, Santa Barbara, CA), P300 (Southern Sierra CZO, CA), SF (Santa Fe, NM) and HJA (HJ Andrews, OR).

---
* `0.0_misc_setup_scripts.R` Misc. scripts for setting up datasets 
* `0.1_utilities.R` Script with libraries, path names and basic functions

---

* `1.1_simulation_***.R` Contains scripts for patch-level vegetation simulation to establish inital non-fire (e.g. soil and vegetion) parameters.
* `1.2_simulation_evaluation.R` Evaluates parameter sets generated from `1.1_simulation_***.R' and determines parameter set with 'typical' growth. This parameter set is then used for all subsequent models.
* `1.3_export_stand_age_***.R` Simulation of the top parameter set from `1.2_simulation_evaluation.R`. A copy of the landscape (worldfile) is exported at stand age 5, 12, 20, 30, 40, 60 and 80 years (Note: HJA exports stand ages at 5, 12, 20, 40, 70, 100, 140 years).

---

* `2.1_sobol_ps_setup_***.R` Generates an empty model of sobol parameter sets to run rhessys. 
* `2.2_sobol_rhessys_setup_***.R` Setup for running RHESSys using Sobol Sensitivity Parameters.
* `2.3_sobol_rhessys_simulation_***.R` Script that calls RHESSys using `2.2_sobol_rhessys_setup_***.R` 
* `2.4_sobol_rhessys_missing_runs_setup.R` Script for redoing sensitivity runs that had 'missing' results.
* `2.5_sobol_rhessys_missing_runs_***.R` This script isolates sobol calibration runs that are missing and reruns those parameters sets.
* `2.6_sobol_evaluation.R` Contains scripts for evaluating parameter sensitivity of fire effects model
* `2.7_sobol_evaluation_timeseries.R` Generates sensitivity plots that are analogous to those in 2.6, but with stand age on x-axis.

---

* `3.1_patch_setup_***.R` Setup of parameter sets for rhessys simulation.
* `3.2_patch_fire_***.R` Simulates patch-level fire effects using RHESSys
* `3.3_patch_fire_eval.R` Contains scripts for evaluating patch-level fire effects.

---

* `display_graphics.R` Contains scripts for producing graphics in Figure 1.

