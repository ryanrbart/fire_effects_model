# Test patch-level fire effects for P300
# 
# This script evaluates fire effects for landscapes simulated at various time intervals.

source("R/0.1_utilities")

# ---------------------------------------------------------------------
# Import parameter sets

# Note: If multiple parameter sets, will need to for loop this file and produce unique outputs
ps <- read.csv(file.path(OUTPUT_DIR_1, "1_selected_ps.csv"), header = TRUE)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# First RHESSys Simulation
# Spin-ups vegetation as in 1.1 but outputs stand-age at appropriate intervals

# ---------------------------------------------------------------------
# Model inputs

# RHESSys Inputs
rhessys_version <- "bin/rhessys5.20.1" # "bin/rhessys5.20.fire_off"
tec_file <- "ws_p300/tecfiles/tec.p300_patch_simulation"
world_file <- "ws_p300/worldfiles/world.p300_30m_2can_patch_9445"
world_hdr_file <- "ws_p300/worldfiles/world.p300_30m_2can_1942_2453.hdr"
flow_file <- "ws_p300/flowtables/flow.p300_30m_patch_9445"
start_date <- "1941 10 1 1"
end_date <- "2041 10 1 1"
output_folder <- "ws_p300/out/1.1_p300_patch_simulation/"     # Must end with '/'
output_filename <- "patch_sim"
command_options <- "-b -g -c -p -tchange 0 0"
parameter_type <- "all_combinations"
m <- ps$m
k <- ps$k
m_v <- ps$m_v
k_v <- ps$k_v
pa <- ps$pa
po <- ps$po
gw1 <- ps$gw1
gw2 <- ps$gw2

# List of lists containing parameters, awk_file, input_file, output_file
#parameter_change_list <- NULL
parameter_change_list <- list()
parameter_change_list[[1]] <- list(c(ps$awks.change.def.epc.leaf_turnover.awk),"awks/change.def.epc.leaf_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.def", "ws_p300/defs/veg_p300_shrub.tmp1")
parameter_change_list[[2]] <- list(c(ps$awks.change.def.epc.livewood_turnover.awk),"awks/change.def.epc.livewood_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp1", "ws_p300/defs/veg_p300_shrub.tmp2")
parameter_change_list[[3]] <- list(c(ps$awks.change.def.epc.alloc_frootc_leafc.awk),"awks/change.def.epc.alloc_frootc_leafc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp3")
parameter_change_list[[4]] <- list(c(ps$awks.change.def.epc.alloc_crootc_stemc.awk),"awks/change.def.epc.alloc_crootc_stemc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp3", "ws_p300/defs/veg_p300_shrub.tmp4")
parameter_change_list[[5]] <- list(c(ps$awks.change.def.epc.alloc_stemc_leafc.awk),"awks/change.def.epc.alloc_stemc_leafc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp4", "ws_p300/defs/veg_p300_shrub.tmp5")
parameter_change_list[[6]] <- list(c(ps$awks.change.def.epc.alloc_livewoodc_woodc.awk),"awks/change.def.epc.alloc_livewoodc_woodc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp5", "ws_p300/defs/veg_p300_shrub.tmp6")
parameter_change_list[[7]] <- list(c(ps$awks.change.def.epc.branch_turnover.awk),"awks/change.def.epc.branch_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp6", "ws_p300/defs/veg_p300_shrub.tmp7")
parameter_change_list[[8]] <- list(c(ps$awks.change.def.epc.height_to_stem_exp.awk),"awks/change.def.epc.height_to_stem_exp.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp7", "ws_p300/defs/veg_p300_shrub.tmp8")
parameter_change_list[[9]] <- list(c(ps$awks.change.def.epc.height_to_stem_coef.awk),"awks/change.def.epc.height_to_stem_coef.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp8", "ws_p300/defs/veg_p300_shrub.tmp_final")

parameter_change_list[[10]] <- list(c(ps$awks.change.def.epc.height_to_stem_exp.awk.1),"awks/change.def.epc.height_to_stem_exp.awk",
                                    "ws_p300/defs/veg_p300_conifer.def", "ws_p300/defs/veg_p300_conifer.tmp1")
parameter_change_list[[11]] <- list(c(ps$awks.change.def.epc.height_to_stem_coef.awk.1),"awks/change.def.epc.height_to_stem_coef.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp1", "ws_p300/defs/veg_p300_conifer.tmp_final")

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
tec_data[8,] <- data.frame(2002, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[9,] <- data.frame(2022, 10, 1, 1, "output_current_state", stringsAsFactors=FALSE)

# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- NULL
#output_variables <- list()

# ---------------------------------------------------------------------

run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_change_list = parameter_change_list,  
            tec_data = tec_data, output_variables = output_variables)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Second RHESSys Simulation
# Computes fire effects at each stand age

# ---------------------------------------------------------------------
# Model inputs

# RHESSys Inputs
rhessys_version <- "bin/rhessys5.20.fire_off"
tec_file <- "ws_p300/tecfiles/tec.p300_patch_fire"
world_file <- c("ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1947M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1954M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1962M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1972M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1982M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y2002M10D1H1.state",
                "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y2022M10D1H1.state")
world_hdr_file <- "ws_p300/worldfiles/world.p300_30m_2can_1942_2453.hdr"
flow_file <- "ws_p300/flowtables/flow.p300_30m_patch_9445"
start_date <- "1941 10 1 1"
end_date <- "1941 10 15 1"
output_folder <- "ws_p300/out/2.1_p300_patch_fire/"     # Must end with '/'
output_filename <- "patch_fire"
command_options <- "-b -g -c -p"
parameter_type <- "all_combinations"
m <- ps$m
k <- ps$k
m_v <- ps$m_v
k_v <- ps$k_v
pa <- ps$pa
po <- ps$po
gw1 <- ps$gw1
gw2 <- ps$gw2


# List of lists containing parameters, awk_file, input_file, output_file
#parameter_change_list <- NULL
parameter_change_list <- list()
parameter_change_list[[1]] <- list(c(ps$awks.change.def.epc.leaf_turnover.awk),"awks/change.def.epc.leaf_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.def", "ws_p300/defs/veg_p300_shrub.tmp1")
parameter_change_list[[2]] <- list(c(ps$awks.change.def.epc.livewood_turnover.awk),"awks/change.def.epc.livewood_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp1", "ws_p300/defs/veg_p300_shrub.tmp2")
parameter_change_list[[3]] <- list(c(ps$awks.change.def.epc.alloc_frootc_leafc.awk),"awks/change.def.epc.alloc_frootc_leafc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp3")
parameter_change_list[[4]] <- list(c(ps$awks.change.def.epc.alloc_crootc_stemc.awk),"awks/change.def.epc.alloc_crootc_stemc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp3", "ws_p300/defs/veg_p300_shrub.tmp4")
parameter_change_list[[5]] <- list(c(ps$awks.change.def.epc.alloc_stemc_leafc.awk),"awks/change.def.epc.alloc_stemc_leafc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp4", "ws_p300/defs/veg_p300_shrub.tmp5")
parameter_change_list[[6]] <- list(c(ps$awks.change.def.epc.alloc_livewoodc_woodc.awk),"awks/change.def.epc.alloc_livewoodc_woodc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp5", "ws_p300/defs/veg_p300_shrub.tmp6")
parameter_change_list[[7]] <- list(c(ps$awks.change.def.epc.branch_turnover.awk),"awks/change.def.epc.branch_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp6", "ws_p300/defs/veg_p300_shrub.tmp7")
parameter_change_list[[8]] <- list(c(ps$awks.change.def.epc.height_to_stem_exp.awk),"awks/change.def.epc.height_to_stem_exp.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp7", "ws_p300/defs/veg_p300_shrub.tmp8")
parameter_change_list[[9]] <- list(c(ps$awks.change.def.epc.height_to_stem_coef.awk),"awks/change.def.epc.height_to_stem_coef.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp8", "ws_p300/defs/veg_p300_shrub.tmp_final")

parameter_change_list[[10]] <- list(c(ps$awks.change.def.epc.height_to_stem_exp.awk.1),"awks/change.def.epc.height_to_stem_exp.awk",
                                    "ws_p300/defs/veg_p300_conifer.def", "ws_p300/defs/veg_p300_conifer.tmp1")
parameter_change_list[[11]] <- list(c(ps$awks.change.def.epc.height_to_stem_coef.awk.1),"awks/change.def.epc.height_to_stem_coef.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp1", "ws_p300/defs/veg_p300_conifer.tmp_final")


parameter_change_list[[1]] <- list(c(0.1),"awks/change.def.epc.livewood_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.def", "ws_p300/defs/veg_p300_shrub.tmp1")
parameter_change_list[[2]] <- list(c(0.9),"awks/change.def.epc.alloc_livewoodc_woodc.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp1", "ws_p300/defs/veg_p300_shrub.tmp2")
parameter_change_list[[3]] <- list(c(0.1),"awks/change.def.epc.branch_turnover.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp3")
parameter_change_list[[4]] <- list(c(7),"awks/change.def.overstory_height_thresh.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp3", "ws_p300/defs/veg_p300_shrub.tmp4")
parameter_change_list[[5]] <- list(c(4),"awks/change.def.understory_height_thresh.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp4", "ws_p300/defs/veg_p300_shrub.tmp5")
parameter_change_list[[6]] <- list(c(1),"awks/change.def.pspread_loss_rel.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp5", "ws_p300/defs/veg_p300_shrub.tmp6")
paramzaeter_change_list[[7]] <- list(c(1),"awks/change.def.vapor_loss_rel.awk",
                                     "ws_p300/defs/veg_p300_shrub.tmp6", "ws_p300/defs/veg_p300_shrub.tmp7")
parameter_change_list[[8]] <- list(c(-10),"awks/change.def.biomass_loss_rel_k1.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp7", "ws_p300/defs/veg_p300_shrub.tmp8")
parameter_change_list[[9]] <- list(c(1),"awks/change.def.biomass_loss_rel_k2.awk",
                                   "ws_p300/defs/veg_p300_shrub.tmp8", "ws_p300/defs/veg_p300_shrub.tmp_final")

parameter_change_list[[10]] <- list(c(7),"awks/change.def.overstory_height_thresh.awk",
                                    "ws_p300/defs/veg_p300_conifer.def", "ws_p300/defs/veg_p300_conifer.tmp1")
parameter_change_list[[11]] <- list(c(4),"awks/change.def.understory_height_thresh.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp1", "ws_p300/defs/veg_p300_conifer.tmp2")
parameter_change_list[[12]] <- list(c(1),"awks/change.def.pspread_loss_rel.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp2", "ws_p300/defs/veg_p300_conifer.tmp3")
parameter_change_list[[13]] <- list(c(1),"awks/change.def.vapor_loss_rel.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp3", "ws_p300/defs/veg_p300_conifer.tmp4")
parameter_change_list[[14]] <- list(c(-10),"awks/change.def.biomass_loss_rel_k1.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp4", "ws_p300/defs/veg_p300_conifer.tmp5")
parameter_change_list[[15]] <- list(c(1),"awks/change.def.biomass_loss_rel_k2.awk",
                                    "ws_p300/defs/veg_p300_conifer.tmp5", "ws_p300/defs/veg_p300_conifer.tmp_final")


# Make tec-file
tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# Make a list of dated sequence data.frames (year, month, day, hour, value)
#dated_seq_file <- NULL
#dated_seq_data <- NULL
dated_seq_file = "ws_p300/clim/lowProv.pspread"
dated_seq_data = list()
dated_seq_data[[1]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[1]][1,] <- data.frame(1941, 10, 7, 1, .1)
dated_seq_data[[2]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[2]][1,] <- data.frame(1941, 10, 7, 1, .2)
dated_seq_data[[3]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[3]][1,] <- data.frame(1941, 10, 7, 1, .3)
dated_seq_data[[4]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[4]][1,] <- data.frame(1941, 10, 7, 1, .4)
dated_seq_data[[5]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[5]][1,] <- data.frame(1941, 10, 7, 1, .5)
dated_seq_data[[6]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[6]][1,] <- data.frame(1941, 10, 7, 1, .6)
dated_seq_data[[7]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[7]][1,] <- data.frame(1941, 10, 7, 1, .7)
dated_seq_data[[8]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[8]][1,] <- data.frame(1941, 10, 7, 1, .8)
dated_seq_data[[9]] <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),value=numeric())
dated_seq_data[[9]][1,] <- data.frame(1941, 10, 7, 1, .9)


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
#output_variables <- NULL
output_variables <- list()
output_variables[[1]] <- list("litrc", "awks/output_var_bd_litrc.awk","patch_fire_basin.daily")
output_variables[[2]] <- list("soil1c", "awks/output_var_pdg_soil1c.awk","patch_fire_grow_patch.daily")
output_variables[[3]] <- list("lai", "awks/output_var_cd_lai.awk","patch_fire_stratum.daily")
output_variables[[4]] <- list("height", "awks/output_var_cd_height.awk","patch_fire_stratum.daily")
output_variables[[5]] <- list("cwdc", "awks/output_var_cdg_cwdc.awk","patch_fire_grow_stratum.daily")
output_variables[[6]] <- list("stemc", "awks/output_var_cdg_stemc.awk","patch_fire_grow_stratum.daily")
output_variables[[7]] <- list("leafc", "awks/output_var_cdg_leafc.awk","patch_fire_grow_stratum.daily")
output_variables[[8]] <- list("live_stemc", "awks/output_var_cdg_live_stemc.awk","patch_fire_grow_stratum.daily")
output_variables[[9]] <- list("dead_stemc", "awks/output_var_cdg_dead_stemc.awk","patch_fire_grow_stratum.daily")

# ---------------------------------------------------------------------

run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_change_list = parameter_change_list,  
            tec_data = tec_data, dated_seq_file = dated_seq_file, 
            dated_seq_data = dated_seq_data, output_variables = output_variables)




