# rhessysR testing script
# Ryan Bart
# September 2016

# ---------------------------------------------------------------------

rhessys_version <- "/Users/ryanrbart/bin/rhessys5.20.fire_off"
tec_file <- "ws_300/tecfiles/tec.p300_sim"
world_file <- "ws_300/worldfiles/world.p300_patch_2canopy_dated_seq"
world_hdr_file <- "ws_300/worldfiles/world.p300_patch_2canopy_dated_seq.hdr"
flow_file <- "ws_300/flowtables/flow.patch"
start_date <- "1945 10 1 1"
end_date <- "1995 10 7 1"
output_folder <- "ws_300/out/cal1"
output_filename <- "test"
command_options <- "-b"
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
parameter_sub_list <- NULL
#parameter_sub_list <- list()
#parameter_sub_list[[1]] <- list(c(4,5,6),"awks/change.def.overstory_height_thresh.awk",
#                                "defs/veg_p301_conifer_cec.def", "defs/veg1.tmp")
#parameter_sub_list[[2]] <- list(c(2),"awks/change.def.understory_height_thresh.awk",
#                                "defs/veg_p301_conifer_cec.def", "defs/veg2.tmp")
#parameter_sub_list[[3]] <- list(c(1000),"awks/change.def.pspread_loss_rel.awk",
#                                "defs/veg_p301_conifer_cec.def", "defs/veg3.tmp")


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- list()
output_variables[[1]] <- list("lai", "../../../awks/extlai.awk","../test_basin.daily")
output_variables[[2]] <- list("et", "../../../awks/extet.awk","../test_basin.daily")

# ---------------------------------------------------------------------

run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_sub_list = parameter_sub_list,  
            output_variables = output_variables)

