# Setup of parameter sets for simulation
# 
# HJA

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Cross world files and dated_seq (pspread) options
world <- c("ws_hja/worldfiles/hja_2can_patch_6018.world.Y1963M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1970M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1978M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1998M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2028M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2058M10D1H1.state",
           "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2098M10D1H1.state")

pspread_levels <- seq(0.1,1.0,by=0.1)

world_pspread <- tidyr::crossing(world, pspread_levels)


# ---------------------------------------------------------------------
# Do monte carlo for each parameter set
# LHC was not done since it was not (as far as I can tell) possible to input log distributed variables into lhc

# Number of parameter sets for each stand age/pspread value
n_par <- 40
# Total parameter sets
n_sim <- n_par * nrow(world_pspread)

# Replicate world_pspread by the number of unique parameters to be used
world_pspread_rep <- n_par %>% 
  purrr::rerun(world_pspread) %>% 
  purrr::invoke(dplyr::bind_rows, .)


tmp1 <- setNames(data.frame(runif(n_sim, min=7,max=7)), "ws_hja/defs/patch_hja.def:overstory_height_thresh")
tmp2 <- setNames(data.frame(runif(n_sim, min=4,max=4)), "ws_hja/defs/patch_hja.def:understory_height_thresh")
tmp3 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_hja/defs/veg_hja_understory.def:understory_mort")
tmp4 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_hja/defs/veg_hja_understory.def:consumption")
tmp5 <- setNames(data.frame(runif(n_sim, min=-10,max=-10)), "ws_hja/defs/veg_hja_understory.def:overstory_mort_k1")
tmp6 <- setNames(data.frame(runif(n_sim, min=1,max=1)), "ws_hja/defs/veg_hja_understory.def:overstory_mort_k2")
tmp7 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_hja/defs/veg_hja_7dougfir.def:understory_mort")
tmp8 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_hja/defs/veg_hja_7dougfir.def:consumption")
tmp9 <- setNames(data.frame(runif(n_sim, min=-20,max=-1)), "ws_hja/defs/veg_hja_7dougfir.def:overstory_mort_k1")
tmp10 <- setNames(data.frame(runif(n_sim, min=0.4,max=1.7)), "ws_hja/defs/veg_hja_7dougfir.def:overstory_mort_k2")

out <- bind_cols(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10)

# ---------------------------------------------------------------------
# Cross all parameter options with each world/pspread option
# 70 world/ pspread options times n parameter options

world_pspread_par <- dplyr::bind_cols(world_pspread_rep, out)

# Export the parameter sets to be used in the sobol model.
write.csv(world_pspread_par, RHESSYS_PAR_SIM_3.1_HJA, row.names = FALSE, quote=FALSE) # Parameter sets


