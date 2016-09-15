# R

This folder contains the code for testing and analyzing the fire effects model in RHESSys. The code is run in sequential order, with location specific scripts denoted as SB (Santa Barbara), SS (Southern Sierra) and SF (Santa Fe).

---

* '1.0_spinup.R' : Contains scripts for initial watershed spin-up to establish vegetation, soils and litter for watershed calibration.  
* '1.1_spinup_sb.R' : Calls '1.0_spinup.R' with parameters for Santa Barbara. Outputs to '???'
* '1.1_spinup_ss.R' : Calls '1.0_spinup.R' with parameters for Southern Sierra. Outputs to '???'
* '1.1_spinup_sf.R' : Calls '1.0_spinup.R' with parameters for Santa Fe. Outputs to '???'

---

* '2.0_calibrate.R' This script is used for RHESSys calibration. Outputs variables include streamflow, LAI and ???.
* '2.1_calibrate_sb.R' : Calls '2.0_calibrate.R' with parameters for Santa Barbara. Outputs to '???'
* '2.1_calibrate_ss.R' : Calls '2.0_calibrate.R' with parameters for Southern Sierra. Outputs to '???'
* '2.1_calibrate_sf.R' : Calls '2.0_calibrate.R' with parameters for Santa Fe. Outputs to '???'

---

* '3.0_evaluate.R' This script is used to evaluate watershed calibration in order to identify subsurface parameters to be used for simulation and testing of fire effects model.
* '3.1_evaluate_sb.R' : Calls '3.0_evaluate.R' with parameters for Santa Barbara. Outputs to '???'
* '3.1_evaluate_ss.R' : Calls '3.0_evaluate.R' with parameters for Southern Sierra. Outputs to '???'
* '3.1_evaluate_sf.R' : Calls '3.0_evaluate.R' with parameters for Santa Fe. Outputs to '???'

---

* '4.0_patch_simulation.R' This script is used simulate vegetation at patch level, beginning with no vegetation, for 5, 10, 20, 50, and 100 years.
* '4.1_patch_simulation_sb.R' : Calls '4.0_patch_simulation.R' with parameters for Santa Barbara. Outputs to '???'
* '4.1_patch_simulation_ss.R' : Calls '4.0_patch_simulation.R' with parameters for Southern Sierra. Outputs to '???'
* '4.1_patch_simulation_sf.R' : Calls '4.0_patch_simulation.R' with parameters for Santa Fe. Outputs to '???'

---

'5.0_patch_fire.R' TBD

'6.0_fire_return_interval.R' TBD

'7.0_plots_tables.R' TBD

