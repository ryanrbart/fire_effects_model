# R

This folder contains the code for testing and analyzing the fire effects model in RHESSys. The code is run in sequential order, with location specific scripts denoted as RS (Rattlesnake, Santa Barbara, CA), P300 (Southern Sierra CZO, CA), SF (Santa Fe, NM) and HJA (HJ Andrews, OR).

---

* `1.1_patch_simulation_hja.R` : Contains scripts for patch-level vegetation simulation of HJA to establish inital soil parameters. Outputs to '???'
* `1.1_patch_simulation_p300.R` : Contains scripts for patch-level vegetation simulation of P300 to establish inital soil parameters. Outputs to '???'
* `1.1_patch_simulation_rs.R` : Contains scripts for patch-level vegetation simulation of Rattlesnake Creek to establish inital soil parameters. Outputs to '???'
* `1.1_patch_simulation_sf.R` : Contains scripts for patch-level vegetation simulation of Santa Fe to establish inital soil parameters. Outputs to '???'

---

* `1.2_patch_simulation_eval_hja.R` : Evaluates parameter sets from `1.1_patch_simulation_hja.R' and determines parameter set with typical growth.
* `1.2_patch_simulation_eval_p300.R` : Evaluates parameter sets from `1.1_patch_simulation_p300.R' and determines parameter set with typical growth.
* `1.2_patch_simulation_eval_rs.R` : Evaluates parameter sets from `1.1_patch_simulation_rs.R' and determines parameter set with typical growth.
* `1.2_patch_simulation_eval_sf.R` : Evaluates parameter sets from `1.1_patch_simulation_sf.R' and determines parameter set with typical growth.

---

* `2.1_patch_fire_hja.R` This script is used to simulate vegetation at patch level, beginning with no vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 50, and 100 years for patch level fire analysis. Outputs to '???'. Fire then simulated at each stand age.
* `2.1_patch_fire_p300.R` This script is used to simulate vegetation at patch level, beginning with no vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 50, and 100 years for patch level fire analysis. Outputs to '???'. Fire then simulated at each stand age.
* `2.1_patch_fire_rs.R` This script is used to simulate vegetation at patch level, beginning with no vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 50, and 100 years for patch level fire analysis. Outputs to '???'. Fire then simulated at each stand age.
* `2.1_patch_fire_sf.R` This script is used to simulate vegetation at patch level, beginning with no vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 50, and 100 years for patch level fire analysis. Outputs to '???'. Fire then simulated at each stand age.

---

* `2.2_patch_fire_eval_hja.R` : 
* `2.2_patch_fire_eval_p300.R` : 
* `2.2_patch_fire_eval_rs.R` : 
* `2.2_patch_fire_eval_sf.R` : 

---

* `3.1_plots_tables.R` Table and figures for manuscript

