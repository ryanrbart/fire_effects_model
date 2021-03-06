# Patch Simulation for HJA
# 
# This script is used to simulate vegetation at patch level, beginning with no
# vegetation.

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "lhc"


# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "bin/rhessys5.20.1_self_thinning_salsa"    # Code for self thinning
input_rhessys$tec_file <- "ws_hja/tecfiles/hja_patch_simulation.tec"
input_rhessys$world_file <- "ws_hja/worldfiles/hja_2can_patch_6018_ts.world"  # Worldfile for not using dated sequence. hja_2can_patch_6018.world is for dated sequence. 
input_rhessys$world_hdr_prefix <- "1.1"
input_rhessys$flow_file <- "ws_hja/flowtables/hja_patch_6018.flow"
input_rhessys$start_date <- "1957 10 1 1"
input_rhessys$end_date <- "2107 10 1 1"
input_rhessys$output_folder <- "ws_hja/out/1.1_hja_patch_simulation"
input_rhessys$output_filename <- "patch_sim"
input_rhessys$command_options <- c("-b -g -c -p -tchange 0 0")


# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("ws_hja/defs/basin_hja.def")
input_hdr_list$hillslope_def <- c("ws_hja/defs/hill_hja.def")
input_hdr_list$zone_def <- c("ws_hja/defs/zone_hja.def")
input_hdr_list$soil_def <- c("ws_hja/defs/patch_hja.def")
input_hdr_list$landuse_def <- c("ws_hja/defs/lu_hja.def")
input_hdr_list$stratum_def <- c("ws_hja/defs/veg_hja_7dougfir.def", "ws_hja/defs/veg_hja_understory.def")
input_hdr_list$base_stations <- c("ws_hja/clim/cs2met_450yr.base")


# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL

n_sim = 250

# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  # Patch parameters
  list(input_hdr_list$soil_def, "soil_depth", c(1.5, 4.0, n_sim)),
  
  # -----
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.alloc_frootc_leafc", c(1.32, 1.32, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_crootc_stemc", c(0.2, 0.3, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_stemc_leafc", c(1, 1, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_livewoodc_woodc", c(0.05, 0.3, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.1, 0.2, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.livewood_turnover", c(0.5, 0.8, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(0.003, 0.007, n_sim)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(10, 12, n_sim)),
  
  # -----
  # Lower canopy parameters
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(1.4, 1.4, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(0.25, 0.45, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(0.2, 0.2, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(0.8, 0.95, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.2, 0.4, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(0.05, 0.2, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(0.01, 0.03, n_sim)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(3.0, 4.0, n_sim))
)

# Standard sub-surface parameters
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(2, 2, n_sim),
  k = c(2, 2, n_sim),
  m_v = c(2, 2, n_sim),
  k_v = c(2, 2, n_sim),
  pa = c(1, 10, n_sim),
  po = c(0.3, 1.2, n_sim),
  gw1 = c(0.001, 0.4, n_sim),
  gw2 = c(0.2, 0.2, n_sim)
)

# Make climate base station file
#input_clim_base_list <- NULL
input_clim_base_list <- list(
  list(core = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       annual = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       monthly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       daily = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       hourly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE)
  )
)
input_clim_base_list[[1]][[1]][1,] <- data.frame(c1=101, c2="base_station_id",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=485, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="ws_hja/clim/cs2met_450yr", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=0, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)


# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
input_dated_seq_list <- NULL
#input_dated_seq_list = list()
#input_dated_seq_list[[1]] <- data.frame(name="cs2met_dated",type="pspread",year=1957,month=10,day=7,hour=1,value=0.95,stringsAsFactors=FALSE)
# Dated sequence doesn't work when combined with redefine world (which is needed
# to change cover fraction). Therefore, I do not use an initial disturbance to
# kill off vegetation. I also use hja_2can_patch_6018_ts.world instead of
# hja_2can_patch_6018.world because the latter doesn't have patch level base
# station needed for dated sequence.


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1957, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1957, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
# Change cover fraction of upper canopy
input_tec_data[3,] <- data.frame(2017, 10, 1, 1, "redefine_world", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
# output_variables <- NULL
output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("litrc", "awks/output_var_bd_litrc.awk","patch_sim_basin.daily",stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("height", "awks/output_var_cd_height.awk","patch_sim_stratum.daily",stringsAsFactors=FALSE)


# ---------------------------------------------------------------------


system.time(
  run_rhessys(parameter_method = parameter_method,
              output_method = "awk",
              input_rhessys = input_rhessys,
              input_hdr_list = input_hdr_list,
              input_preexisting_table = input_preexisting_table,
              input_def_list = input_def_list,
              input_standard_par_list = input_standard_par_list,
              input_clim_base_list = input_clim_base_list,
              input_dated_seq_list = input_dated_seq_list,
              input_tec_data = input_tec_data,
              output_variables = output_variables)
)

beep(1)

