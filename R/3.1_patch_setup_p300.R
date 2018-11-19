# Setup of parameter sets for simulation
# 
# 

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Cross world files and dated_seq (pspread) options

world <- c("ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state",
                              "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state")

pspread_levels <- seq(0.1,1.0,by=0.1)

world_pspread <- tidyr::crossing(world, pspread_levels)


# ---------------------------------------------------------------------
# Do Latin Hypercube for each parameter set

n_sim <- 10

input_list <- list("ws_p300/defs/patch_p300.def:overstory_height_thresh" = c(7,7,n_sim),
                   "ws_p300/defs/patch_p300.def:understory_height_thresh" = c(4.5,4.5,n_sim),
                   "ws_p300/defs/veg_p300_shrub.def:understory_mort" = c(0.01,100,n_sim),
                   "ws_p300/defs/veg_p300_shrub.def:consumption" = c(0.01,100,n_sim),
                   "ws_p300/defs/veg_p300_shrub.def:overstory_mort_k1" = c(-5,-5,n_sim),
                   "ws_p300/defs/veg_p300_shrub.def:overstory_mort_k2" = c(1,1,n_sim),
                   "ws_p300/defs/veg_p300_conifer.def:understory_mort" = c(0.01,100,n_sim),
                   "ws_p300/defs/veg_p300_conifer.def:consumption" = c(0.01,100,n_sim),
                   "ws_p300/defs/veg_p300_conifer.def:overstory_mort_k1" = c(-20,-1,n_sim),
                   "ws_p300/defs/veg_p300_conifer.def:overstory_mort_k2" = c(0.2,2,n_sim)
                   )

k <- length(input_list)

# Inputs for Latin Hypercube
min_par <- sapply(seq_len(k), function(x,y) y[[x]][1], y=input_list)
max_par <- sapply(seq_len(k), function(x,y) y[[x]][2], y=input_list)
n <- input_list[[1]][3]

# Transform a Latin hypercube
grid <- lhs::randomLHS(n, k) # Generate generic hypercube
out <- mapply(function(w, x, y, z) qunif(w[,x], min=y, max=z), 
              x=seq_len(k), 
              y=min_par, 
              z=max_par, 
              MoreArgs = list(w=grid))
out <- matrix(out,nrow=n)
out <- as.data.frame(out)
colnames(out) <- names(input_list)


# ---------------------------------------------------------------------
# Cross all parameter options with each world/pspread option
# 70 world/ pspread options times n parameter options

world_pspread_par <- tidyr::crossing(world_pspread, out)

# Export the parameter sets to be used in the sobol model.
write.csv(world_pspread_par, RHESSYS_PAR_SIM_3.1_P300, row.names = FALSE, quote=FALSE) # Parameter sets


