# Spinup for P300
#
# Contains scripts for initial watershed spin-up of P300 to establish
# vegetation, soils and litter for watershed calibration.

library(rhessysR)

# ---------------------------------------------------------------------
# Model inputs

rhessys_version <- "bin/rhessys5.19_bart"
tec_file <- "ws_p300/tecfiles/tec.p300_spinup"
world_file <- "ws_p300/worldfiles/world.p300_30m_2can_spinup"
world_hdr_file <- "ws_p300/worldfiles/world.p300_30m_2can_spinup.hdr"
flow_file <- "ws_p300/flowtables/flow.p300_30m"
start_date <- "1941 10 1 1"
end_date <- "2091 10 1 1"
output_folder <- "ws_p300/out/p300_spinup"
output_filename <- "spinup"
command_options <- "-b -g -c 1 181 9445 9445 -p 1 181 9445 9445"
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
                                   "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp_final")


# Make tec-file
#tec_data <- NULL
tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
tec_data[1,] <- data.frame(1945, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
tec_data[2,] <- data.frame(1945, 10, 1, 1, "print_daily_growth_on", stringsAsFactors=FALSE)
tec_data[3,] <- data.frame(2041, 09, 30, 1, "output_current_state", stringsAsFactors=FALSE)
tec_data[4,] <- data.frame(2091, 09, 30, 1, "output_current_state", stringsAsFactors=FALSE)


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- NULL
 



# ---------------------------------------------------------------------
# Run RHESSys


run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_change_list = parameter_change_list,  
            tec_data = tec_data, output_variables = output_variables)

