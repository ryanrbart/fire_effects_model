# Run scripts in batch

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Tune the model and find top parameter set

source("R/1.1_patch_simulation_hja.R")
source("R/1.1_patch_simulation_p300.R")
source("R/1.1_patch_simulation_rs.R")
source("R/1.1_patch_simulation_sf.R")

# 
sink(file("backup.log"), append=TRUE)
sink(file=NULL)
#closeAllConnections()

source("R/1.2_patch_simulation_eval.R", echo=TRUE)

source("R/1.3_patch_export_stand_age_hja.R", echo=TRUE)
source("R/1.3_patch_export_stand_age_p300.R", echo=TRUE)
source("R/1.3_patch_export_stand_age_rs.R", echo=TRUE)
source("R/1.3_patch_export_stand_age_sf.R", echo=TRUE)

# ---------------------------------------------------------------------
# Sensitivity test

source("R/2.1_patch_fire_p300.R")
source("R/2.1_patch_fire_rs.R")

source("R/2.2_patch_fire_eval_p300.R")
source("R/2.2_patch_fire_eval_rs.R")

# ---------------------------------------------------------------------
# Patch Simulation



# ---------------------------------------------------------------------
# Final Figures and tables



