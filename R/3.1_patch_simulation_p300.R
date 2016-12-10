# Patch Simulation for P300
# 
# This script is used to simulate vegetation at patch level, beginning with no
# vegetation. A copy of the landscape (worldfile) is exported at 5, 10, 20, 40,
# and 80 years for patch level fire analysis.

library(rhessysR)

# ---------------------------------------------------------------------
# Model inputs

# RHESSys Inputs
rhessys_version <- "bin/rhessys5.20.fire_off"
tec_file <- "ws_p300/tecfiles/tec.p300_patch_simulation"
world_file <- "ws_p300/worldfiles/world.p300_30m_2can_patch_9445"
world_hdr_file <- "ws_p300/worldfiles/world.p300_30m_2can_1942_2453.hdr"
flow_file <- "ws_p300/flowtables/flow.p300_30m_patch_9445"
start_date <- "1941 10 1 1"
end_date <- "2041 10 1 1"
output_folder <- "ws_p300/out/p300_patch_simulation/"     # Must end with '/'
output_filename <- "patch_sim"
command_options <- "-b -g -c -p"
parameter_type <- "all_combinations"
m <- c(1.792761)
k <- c(1.566492)
m_v <- c(1.792761)
k_v <- c(1.566492)
pa <- c(7.896941)
po <- c(1.179359)
gw1 <- c(0.168035)
gw2 <- c(0.178753)


# List of lists containing parameters, awk_file, input_file, output_file
#parameter_change_list <- NULL
parameter_change_list <- list()
parameter_change_list[[1]] <- list(c(0.1),"awks/change.def.epc.livewood_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.def", "ws_p300/defs/veg_p300_shrub.tmp1")
parameter_change_list[[2]] <- list(c(0.9),"awks/change.def.epc.alloc_livewoodc_woodc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp1", "ws_p300/defs/veg_p300_shrub.tmp2")
parameter_change_list[[3]] <- list(c(0.1),"awks/change.def.epc.branch_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp3")
parameter_change_list[[4]] <- list(c(6),"awks/change.def.overstory_height_thresh.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp3", "ws_p300/defs/veg_p300_shrub.tmp4")
parameter_change_list[[5]] <- list(c(4),"awks/change.def.understory_height_thresh.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp4", "ws_p300/defs/veg_p300_shrub.tmp5")
parameter_change_list[[6]] <- list(c(.2),"awks/change.def.pspread_loss_rel.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp5", "ws_p300/defs/veg_p300_shrub.tmp6")
parameter_change_list[[7]] <- list(c(.2),"awks/change.def.vapor_loss_rel.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp6", "ws_p300/defs/veg_p300_shrub.tmp7")
parameter_change_list[[8]] <- list(c(-0.05),"awks/change.def.biomass_loss_rel_k1.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp7", "ws_p300/defs/veg_p300_shrub.tmp8")
parameter_change_list[[9]] <- list(c(0.25),"awks/change.def.biomass_loss_rel_k2.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp8", "ws_p300/defs/veg_p300_shrub.tmp_final")


# Make tec-file
#tec_data <- NULL
tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
tec_data[3,] <- data.frame(1947, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[4,] <- data.frame(1954, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[5,] <- data.frame(1962, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[6,] <- data.frame(1972, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[7,] <- data.frame(1982, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[7,] <- data.frame(2002, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[8,] <- data.frame(2022, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)

# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
#output_variables <- NULL
output_variables <- list()
output_variables[[1]] <- list("lai", "awks/output_var_bd_lai.awk","patch_sim_basin.daily")
output_variables[[2]] <- list("et", "awks/output_var_bd_et.awk","patch_sim_basin.daily")

# ---------------------------------------------------------------------

run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_change_list = parameter_change_list,  
            tec_data = tec_data, output_variables = output_variables)



