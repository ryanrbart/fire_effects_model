# R

This folder contains the code for testing and analyzing the fire effects model in RHESSys. The code is run in sequential order, with location specific scripts denoted as RS (Rattlesnake, Santa Barbara, CA), P300 (Southern Sierra CZO, CA), SF (Santa Fe, NM) and HJA (HJ Andrews, OR).

---

* `0.1_utilities.R` Script with libraries, path names and basic functions

---

* `1.1_patch_simulation_p300.R` Contains scripts for patch-level vegetation simulation of P300 to establish inital non-fire (e.g. soil and vegetion) parameters.
* `1.2_patch_simulation_eval_p300.R` Evaluates parameter sets from `1.1_patch_simulation_p300.R' and determines parameter set with typical growth. This parameter set is then used for all subsequent models.
* `1.3_patch_sens_p300.R` Simulation of the top parameter set from `1.2_patch_simulation_eval_p300.R`.  A copy of the landscape (worldfile) is exported at stand age 5, 12, 20, 30, 40, 60 and 80 years.
Repeated for HJA, Rattlesnake and Santa Fe

---


* `2.1_patch_sens_p300_sobol.R` Global sensitivity analysis of fire model using Sobol variance-based sensitivity model. Conducted separately for stand age *, *, *. 
* `2.2_patch_sens_eval_p300_sobol.R` Analysis of Sobol sensitivity analysis
* `2.3_patch_sens_p300_prcc.R` Global sensitivity analysis of fire model using Partial Rank Correlation Coefficient, a regession-based sensitivity model. Conducted separately for stand age *, *, *. 
* `2.4_patch_sens_eval_p300_prcc.R` Analysis of prcc sensitivity analysis
Repeated for HJA, Rattlesnake and Santa Fe

---

* `3.1_patch_fire_p300.R` Fire simulated at each stand age.
* `3.2_patch_fire_eval_p300.R` 
Repeated for HJA, Rattlesnake and Santa Fe

---

* `4.1_synthesis.R` Prepares data from all watersheds for manuscript figures and tables
* `4.2_plots_tables.R` Figures and tables for manuscript

