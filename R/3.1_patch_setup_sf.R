# Setup of parameter sets for simulation
# 
# SF

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Cross world files and dated_seq (pspread) options
world <- c("ws_sf/worldfiles/sf_2can_patch_2777.world.Y1947M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1954M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1962M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1972M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1982M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2002M10D1H1.state",
           "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2022M10D1H1.state")

pspread_levels <- seq(0.1,1.0,by=0.1)

world_pspread <- tidyr::crossing(world, pspread_levels)


# ---------------------------------------------------------------------
# Do monte carlo for each parameter set
# LHC was not done since it was not (as far as I can tell) possible to input log distributed variables into lhc

# Number of parameter sets for each stand age/pspread value
n_par <- 100
# Total parameter sets
n_sim <- n_par * nrow(world_pspread)

# Replicate world_pspread by the number of unique parameters to be used
world_pspread_rep <- n_par %>% 
  purrr::rerun(world_pspread) %>% 
  purrr::invoke(dplyr::bind_rows, .)


tmp1 <- setNames(data.frame(runif(n_sim, min=7,max=7)), "ws_sf/defs/soil_sf_303verydeepsandyloam.def:overstory_height_thresh")
tmp2 <- setNames(data.frame(runif(n_sim, min=4,max=4)), "ws_sf/defs/soil_sf_303verydeepsandyloam.def:understory_height_thresh")
tmp3 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_sf/defs/veg_sf_understory.def:understory_mort")
tmp4 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_sf/defs/veg_sf_understory.def:consumption")
tmp5 <- setNames(data.frame(runif(n_sim, min=-10,max=-10)), "ws_sf/defs/veg_sf_understory.def:overstory_mort_k1")
tmp6 <- setNames(data.frame(runif(n_sim, min=1,max=1)), "ws_sf/defs/veg_sf_understory.def:overstory_mort_k2")
tmp7 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_sf/defs/veg_sf_1ponderosapine.def:understory_mort")
tmp8 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_sf/defs/veg_sf_1ponderosapine.def:consumption")
tmp9 <- setNames(data.frame(runif(n_sim, min=-20,max=-1)), "ws_sf/defs/veg_sf_1ponderosapine.def:overstory_mort_k1")
tmp10 <- setNames(data.frame(runif(n_sim, min=0.2,max=0.9)), "ws_sf/defs/veg_sf_1ponderosapine.def:overstory_mort_k2")

out <- bind_cols(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10)

# ---------------------------------------------------------------------
# Cross all parameter options with each world/pspread option
# 70 world/ pspread options times n parameter options

world_pspread_par <- dplyr::bind_cols(world_pspread_rep, out)

# Export the parameter sets to be used in the sobol model.
write.csv(world_pspread_par, RHESSYS_PAR_SIM_3.1_SF, row.names = FALSE, quote=FALSE) # Parameter sets


