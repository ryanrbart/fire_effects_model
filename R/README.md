# R

This folder contains the code for testing and analyzing the fire effects model in RHESSys. The code is run in sequential order, with location specific scripts denoted as RS (Rattlesnake, Santa Barbara, CA), P300 (Southern Sierra CZO, CA), SF (Santa Fe, NM) and HJA (HJ Andrews, OR).

---

* `1.1_spinup_hja.R` : Contains scripts for initial watershed spin-up of HJ Andrews to establish vegetation, soils and litter for watershed calibration. Outputs to '???'
* `1.1_spinup_p300.R` : Contains scripts for initial watershed spin-up of P300 to establish vegetation, soils and litter for watershed calibration. Outputs to '???'
* `1.1_spinup_rs.R` : Contains scripts for initial watershed spin-up of Rattlesnake Creek to establish vegetation, soils and litter for watershed calibration. Outputs to '???'
* `1.1_spinup_sf.R` : Contains scripts for initial watershed spin-up of Santa Fe to establish vegetation, soils and litter for watershed calibration. Outputs to '???'
* `1.2_spinup_eval.R` : Contains scripts for evaluating spinup.

---

* `2.1_calibrate_hja.R` This script is used for RHESSys calibration. Exported variables include streamflow, LAI and ???. Outputs to '???'
* `2.1_calibrate_p300.R` This script is used for RHESSys calibration. Exported variables include streamflow, LAI and ???. Outputs to '???'
* `2.1_calibrate_rs.R` This script is used for RHESSys calibration. Exported variables include streamflow, LAI and ???. Outputs to '???'
* `2.1_calibrate_sf.R` This script is used for RHESSys calibration. Exported variables include streamflow, LAI and ???. Outputs to '???'
* `2.2_calibrate_eval.R` : This script consolidates calibration results and is used to evaluate watershed calibration in order to identify subsurface parameters for simulation and testing of fire effects model. Parameter sets to be used for simulation exported to '???'

---

* `3.1_patch_simulation.R` This script is used to simulate vegetation at patch level, beginning with no vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 50, and 100 years for patch level fire analysis. Outputs to '???'
* `3.2_patch_simulation_eval.R` : Contains scripts for evaluating patch spinup.

---

* `4.1_patch_fire.R` Patch level analysis of fire effects model TBD

* `5.1_fire_return_interval.R` Watershed-level analysis of fire-return interval TBD

* `6.1_plots_tables.R` Table and figures for manuscript

