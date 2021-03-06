# Patch-level fire effects for HJA
# 
# This script evaluates fire effects for landscapes simulated at various time intervals.

source("R/0.1_utilities.R")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# RHESSys Simulation
# Computes fire effects at each stand age

# ---------------------------------------------------------------------
# Import parameter sets

ps <- read_csv(OUTPUT_DIR_1_HJA_TOP_PS)

# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "all_combinations"

# RHESSys Inputs
input_rhessys <- list()
#input_rhessys$rhessys_version <- "bin/rhessys5.20.1"
input_rhessys$rhessys_version <- "bin/rhessys6.0"
input_rhessys$tec_file <- "ws_hja/tecfiles/hja_patch_simulation.tec"
input_rhessys$world_file <- "assigned_below"
input_rhessys$world_hdr_prefix <- "3.2_hja"
input_rhessys$flow_file <- "ws_hja/flowtables/hja_patch_6018.flow"
input_rhessys$start_date <- "1957 10 1 1"
input_rhessys$end_date <- "1957 10 15 1"
input_rhessys$output_folder <- "ws_hja/out/3.2_hja"
input_rhessys$output_filename <- "patch_fire"
input_rhessys$command_options <- c("-b -g -c -p -f")


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

n_sim <- NULL

# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  # Patch parameters
  list(input_hdr_list$soil_def, "soil_depth", c(ps$`ws_hja/defs/patch_hja.def:soil_depth`)),
  
  # -----
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.alloc_frootc_leafc", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.alloc_frootc_leafc`)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_crootc_stemc", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.alloc_crootc_stemc`)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_stemc_leafc", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.alloc_stemc_leafc`)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_livewoodc_woodc", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.alloc_livewoodc_woodc`)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.leaf_turnover`)),
  list(input_hdr_list$stratum_def[1], "epc.livewood_turnover", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.livewood_turnover`)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.branch_turnover`)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(ps$`ws_hja/defs/veg_hja_7dougfir.def:epc.height_to_stem_coef`)),
  
  # -----
  # Lower canopy parameters
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.alloc_frootc_leafc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.alloc_crootc_stemc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.alloc_stemc_leafc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.alloc_livewoodc_woodc`)),
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.leaf_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.livewood_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.branch_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(ps$`ws_hja/defs/veg_hja_understory.def:epc.height_to_stem_coef`))
)


# Standard sub-surface parameters
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(ps$m),
  k = c(ps$k),
  m_v = c(ps$m_v),
  k_v = c(ps$k_v),
  pa = c(ps$pa),
  po = c(ps$po),
  gw1 = c(ps$gw1),
  gw2 = c(ps$gw2)
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
# input_dated_seq_list <- NULL
input_dated_seq_list = list()


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1957, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1957, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
# output_variables <- NULL
output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("leafc", "awks/output_var_cdg_leafc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("stemc", "awks/output_var_cdg_stemc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[3,] <- data.frame("litrc", "awks/output_var_bd_litrc.awk","patch_fire_basin.daily",stringsAsFactors=FALSE)
output_variables[4,] <- data.frame("cwdc", "awks/output_var_cdg_cwdc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[5,] <- data.frame("soil1c", "awks/output_var_pdg_soil1c.awk","patch_fire_grow_patch.daily",stringsAsFactors=FALSE)
output_variables[6,] <- data.frame("height", "awks/output_var_cd_height.awk","patch_fire_stratum.daily",stringsAsFactors=FALSE)
output_variables[7,] <- data.frame("canopy_target_prop_mort", "awks/output_var_fd_canopy_target_prop_mort.awk","patch_fire_fire.daily",stringsAsFactors=FALSE)
output_variables[8,] <- data.frame("canopy_target_prop_mort_consumed", "awks/output_var_fd_canopy_target_prop_mort_consumed.awk","patch_fire_fire.daily",stringsAsFactors=FALSE)
output_variables[9,] <- data.frame("canopy_target_prop_c_consumed", "awks/output_var_fd_canopy_target_prop_c_consumed.awk","patch_fire_fire.daily",stringsAsFactors=FALSE)
output_variables[10,] <- data.frame("canopy_target_prop_c_remain", "awks/output_var_fd_canopy_target_prop_c_remain.awk","patch_fire_fire.daily",stringsAsFactors=FALSE)








# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Simulation Analysis


world_pspread_par <- read_csv(RHESSYS_PAR_SIM_3.1_HJA)
n_runs <- nrow(world_pspread_par)
output_init <- c(1,rep(0,(n_runs-1)))   # Used for output_initiation argument in run_rhessys 

# ---------------------------------------------------------------------

# Step through parameter sets
system.time(
  for (aa in seq_len(nrow(world_pspread_par))){
    
    # ----
    # Assign worldfile
    input_rhessys$world_file <- world_pspread_par$world[aa]
    
    # ----
    # Assign parameters
    
    # Patch fire parameters
    input_def_list[[18]] <- list(input_hdr_list$soil_def[1], "overstory_height_thresh", world_pspread_par$`ws_hja/defs/patch_hja.def:overstory_height_thresh`[aa])
    input_def_list[[19]] <- list(input_hdr_list$soil_def[1], "understory_height_thresh", world_pspread_par$`ws_hja/defs/patch_hja.def:understory_height_thresh`[aa])
    # Lower canopy fire parameters
    input_def_list[[20]] <- list(input_hdr_list$stratum_def[2], "understory_mort", world_pspread_par$`ws_hja/defs/veg_hja_understory.def:understory_mort`[aa])
    input_def_list[[21]] <- list(input_hdr_list$stratum_def[2], "consumption", world_pspread_par$`ws_hja/defs/veg_hja_understory.def:consumption`[aa])
    input_def_list[[22]] <- list(input_hdr_list$stratum_def[2], "overstory_mort_k1", world_pspread_par$`ws_hja/defs/veg_hja_understory.def:overstory_mort_k1`[aa])
    input_def_list[[23]] <- list(input_hdr_list$stratum_def[2], "overstory_mort_k2", world_pspread_par$`ws_hja/defs/veg_hja_understory.def:overstory_mort_k2`[aa])
    # Upper canopy fire parameters
    input_def_list[[24]] <- list(input_hdr_list$stratum_def[1], "understory_mort", world_pspread_par$`ws_hja/defs/veg_hja_7dougfir.def:understory_mort`[aa])
    input_def_list[[25]] <- list(input_hdr_list$stratum_def[1], "consumption", world_pspread_par$`ws_hja/defs/veg_hja_7dougfir.def:consumption`[aa])
    input_def_list[[26]] <- list(input_hdr_list$stratum_def[1], "overstory_mort_k1", world_pspread_par$`ws_hja/defs/veg_hja_7dougfir.def:overstory_mort_k1`[aa])
    input_def_list[[27]] <- list(input_hdr_list$stratum_def[1], "overstory_mort_k2", world_pspread_par$`ws_hja/defs/veg_hja_7dougfir.def:overstory_mort_k2`[aa])
    
    # ----
    input_dated_seq_list = list()
    input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1957,month=10,day=7,hour=1,value=world_pspread_par$pspread_levels[aa],stringsAsFactors=FALSE)
    
    
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
                output_variables = output_variables,
                output_initiation = output_init[aa])
    
    print("----------------------------------------------------------")
    print(paste("Simulation: Run #", aa, "of", n_runs))
  }
)
beep(1)


