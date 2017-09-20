# Sensitivity test using Sobol for P300
# 
# Global sensitivity analysis of fire model using Sobol variance-based
# sensitivity model. Conducted separately for stand age **, **, **.

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Assemble Parameter Sets for Sobol Sensitivity Test (Sobol)

n_sim <- 50

input_sobol <- list()
input_sobol <- lapply(seq(1,2), function(x){

  tmp1 <- setNames(data.frame(runif(n_sim, min=5,max=8)), "ws_p300/defs/patch_p300.def:overstory_height_thresh")
  tmp2 <- setNames(data.frame(runif(n_sim, min=2,max=4)), "ws_p300/defs/patch_p300.def:understory_height_thresh")
  tmp3 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_p300/defs/veg_p300_shrub.def:pspread_loss_rel")
  tmp4 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_p300/defs/veg_p300_shrub.def:vapor_loss_rel")
  tmp5 <- setNames(data.frame(runif(n_sim, min=-20,max=-1)), "ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k1")
  tmp6 <- setNames(data.frame(runif(n_sim, min=0.2,max=2)), "ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k2")
  tmp7 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_p300/defs/veg_p300_conifer.def:pspread_loss_rel")
  tmp8 <- setNames(data.frame(rlunif(n_sim, min=0.01,max=100)), "ws_p300/defs/veg_p300_conifer.def:vapor_loss_rel")
  tmp9 <- setNames(data.frame(runif(n_sim, min=-20,max=-1)), "ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k1")
  tmp10 <- setNames(data.frame(runif(n_sim, min=0.2,max=2)), "ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k2")
  
  tmp_pspread <- setNames(data.frame(runif(n_sim, min=0.001,max=1)), "pspread")
  
  input_sobol[[x]] <- bind_cols(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp_pspread)
})

# Produce empty model so that non-R model (RHESSys) can be used.
sobol_model <- sobolmartinez(model = NULL, X1 = input_sobol[[1]], X2 = input_sobol[[2]], order = 1, nboot = 200)   # runs=n_sim*(p+2)
#sobol_model <- sobol2002(model = NULL, X1 = input_sobol[[1]], X2 = input_sobol[[2]])  # runs=n_sim*(p+2)

# Export the parameter sets to be used in the sobol model.
write.csv(sobol_model$X, RHESSYS_PAR_SOBOL_2.1_P300, row.names = FALSE, quote=FALSE)

n_runs <- length(sobol_model$X[,1])
output_init <- c(1,rep(0,(n_runs-1)))   # Used for output_initiation argument in run_rhessys 


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Set up RHESSys model 

# Import top parameter sets
ps <- read_csv(OUTPUT_DIR_1_P300_TOP_PS)

# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "all_combinations"

# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "bin/rhessys5.20.1"
input_rhessys$tec_file <- "ws_p300/tecfiles/p300_patch_fire.tec"
input_rhessys$world_file <- "assigned below"
input_rhessys$world_hdr_prefix <- "p300_30m_2can_patch_9445"
input_rhessys$flow_file <- "ws_p300/flowtables/p300_30m_patch_9445.flow"
input_rhessys$start_date <- "1941 10 1 1"
input_rhessys$end_date <- "1941 10 15 1"
input_rhessys$output_folder <- "assigned below"
input_rhessys$output_filename <- "patch_fire"
input_rhessys$command_options <- c("-b -g -c -p")


# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("ws_p300/defs/basin_p300.def")
input_hdr_list$hillslope_def <- c("ws_p300/defs/hill_p300.def")
input_hdr_list$zone_def <- c("ws_p300/defs/zone_p300.def")
input_hdr_list$soil_def <- c("ws_p300/defs/patch_p300.def")
input_hdr_list$landuse_def <- c("ws_p300/defs/lu_p300.def")
input_hdr_list$stratum_def <- c("ws_p300/defs/veg_p300_conifer.def", "ws_p300/defs/veg_p300_shrub.def")
input_hdr_list$base_stations <- c("ws_p300/clim/Grove_lowprov_clim_1942_2453.base")

# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL

#n_sim <- 50

# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  # Lower canopy parameters
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.leaf_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.livewood_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.alloc_frootc_leafc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.alloc_crootc_stemc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.alloc_stemc_leafc`)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.alloc_livewoodc_woodc`)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.branch_turnover`)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_exp", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.height_to_stem_exp`)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(ps$`ws_p300/defs/veg_p300_shrub.def:epc.height_to_stem_coef`)),
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_exp", c(ps$`ws_p300/defs/veg_p300_conifer.def:epc.height_to_stem_exp`)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(ps$`ws_p300/defs/veg_p300_conifer.def:epc.height_to_stem_coef`))
)
  # -----
  # Patch fire parameters (assigned below as model steps through SOBOL parameter sets)
  # list(input_hdr_list$soil_def[1], "overstory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:overstory_height_thresh`[aa]),
  # list(input_hdr_list$soil_def[1], "understory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:understory_height_thresh`[aa]),
  # # Lower canopy fire parameters
  # list(input_hdr_list$stratum_def[2], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:pspread_loss_rel`[aa]),
  # list(input_hdr_list$stratum_def[2], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:vapor_loss_rel`[aa]),
  # list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k1`[aa]),
  # list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k2`[aa]),
  # # Upper canopy fire parameters
  # list(input_hdr_list$stratum_def[1], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:pspread_loss_rel`[aa]),
  # list(input_hdr_list$stratum_def[1], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:vapor_loss_rel`[aa]),
  # list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k1`[aa]),
  # list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k2`[aa])


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
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=1748, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="ws_p300/clim/Grove_lowprov_clim_1942_2453", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=0, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)


# Make a list of dated sequence data.frames (file name, year, month, day, hour, value) (assigned below)
# input_dated_seq_list <- NULL
# input_dated_seq_list = list()
# input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=7,hour=1,value=sobol_model$X$pspread[aa],stringsAsFactors=FALSE)


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
# output_variables <- NULL
output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("lai", "awks/output_var_bd_lai.awk","patch_fire_basin.daily",stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("leafc", "awks/output_var_cdg_leafc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[3,] <- data.frame("stemc", "awks/output_var_cdg_stemc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[4,] <- data.frame("live_stemc", "awks/output_var_cdg_live_stemc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[5,] <- data.frame("dead_stemc", "awks/output_var_cdg_dead_stemc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[6,] <- data.frame("rootc", "awks/output_var_cdg_rootc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)

output_variables[7,] <- data.frame("litrc", "awks/output_var_bd_litrc.awk","patch_fire_basin.daily",stringsAsFactors=FALSE)
output_variables[8,] <- data.frame("cwdc", "awks/output_var_cdg_cwdc.awk","patch_fire_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[9,] <- data.frame("soil1c", "awks/output_var_pdg_soil1c.awk","patch_fire_grow_patch.daily",stringsAsFactors=FALSE)

output_variables[10,] <- data.frame("height", "awks/output_var_cd_height.awk","patch_fire_stratum.daily",stringsAsFactors=FALSE)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Conduct sensitivity test of fire effects at each stand age

stand_ages <- c("ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state")

# ---------------------------------------------------------------------
# Sobol Sensitivity Analysis for Stand 1

# Step through Sobol parameter sets
system.time(
  for (aa in seq_len(nrow(sobol_model$X))){
    
    input_rhessys$world_file <- stand_ages[1]
    input_rhessys$output_folder <- RHESSYS_OUT_DIR_2.1_P300_STAND1
    
    # Patch fire parameters
    input_def_list[[12]] <- list(input_hdr_list$soil_def[1], "overstory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:overstory_height_thresh`[aa])
    input_def_list[[13]] <- list(input_hdr_list$soil_def[1], "understory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:understory_height_thresh`[aa])
    # Lower canopy fire parameters
    input_def_list[[14]] <- list(input_hdr_list$stratum_def[2], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:pspread_loss_rel`[aa])
    input_def_list[[15]] <- list(input_hdr_list$stratum_def[2], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:vapor_loss_rel`[aa])
    input_def_list[[16]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k1`[aa])
    input_def_list[[17]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k2`[aa])
    # Upper canopy fire parameters
    input_def_list[[18]] <- list(input_hdr_list$stratum_def[1], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:pspread_loss_rel`[aa])
    input_def_list[[19]] <- list(input_hdr_list$stratum_def[1], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:vapor_loss_rel`[aa])
    input_def_list[[20]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k1`[aa])
    input_def_list[[21]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k2`[aa])
    
    # Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
    # input_dated_seq_list <- NULL
    input_dated_seq_list = list()
    input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=7,hour=1,value=sobol_model$X$pspread[aa],stringsAsFactors=FALSE)
    
    
    run_rhessys(parameter_method = parameter_method,
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
    
    print(paste("Simulation 1: Run #", aa, "of", nrow(sobol_model$X)))
  }
)

beep(1)

# ---------------------------------------------------------------------
# Sobol Sensitivity Analysis for Stand 2

# Step through Sobol parameter sets
system.time(
  for (aa in seq_len(nrow(sobol_model$X))){
    
    input_rhessys$world_file <- stand_ages[4]
    input_rhessys$output_folder <- RHESSYS_OUT_DIR_2.1_P300_STAND2
    
    # Patch fire parameters
    input_def_list[[12]] <- list(input_hdr_list$soil_def[1], "overstory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:overstory_height_thresh`[aa])
    input_def_list[[13]] <- list(input_hdr_list$soil_def[1], "understory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:understory_height_thresh`[aa])
    # Lower canopy fire parameters
    input_def_list[[14]] <- list(input_hdr_list$stratum_def[2], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:pspread_loss_rel`[aa])
    input_def_list[[15]] <- list(input_hdr_list$stratum_def[2], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:vapor_loss_rel`[aa])
    input_def_list[[16]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k1`[aa])
    input_def_list[[17]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k2`[aa])
    # Upper canopy fire parameters
    input_def_list[[18]] <- list(input_hdr_list$stratum_def[1], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:pspread_loss_rel`[aa])
    input_def_list[[19]] <- list(input_hdr_list$stratum_def[1], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:vapor_loss_rel`[aa])
    input_def_list[[20]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k1`[aa])
    input_def_list[[21]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k2`[aa])
    
    # Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
    # input_dated_seq_list <- NULL
    input_dated_seq_list = list()
    input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=7,hour=1,value=sobol_model$X$pspread[aa],stringsAsFactors=FALSE)
    
    
    run_rhessys(parameter_method = parameter_method,
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
    
    print(paste("Simulation 1: Run #", aa, "of", nrow(sobol_model$X)))
  }
)

beep(1)

# ---------------------------------------------------------------------
# Sobol Sensitivity Analysis for Stand 3

# Step through Sobol parameter sets
system.time(
  for (aa in seq_len(nrow(sobol_model$X))){
    
    input_rhessys$world_file <- stand_ages[7]
    input_rhessys$output_folder <- RHESSYS_OUT_DIR_2.1_P300_STAND3
    
    # Patch fire parameters
    input_def_list[[12]] <- list(input_hdr_list$soil_def[1], "overstory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:overstory_height_thresh`[aa])
    input_def_list[[13]] <- list(input_hdr_list$soil_def[1], "understory_height_thresh", sobol_model$X$`ws_p300/defs/patch_p300.def:understory_height_thresh`[aa])
    # Lower canopy fire parameters
    input_def_list[[14]] <- list(input_hdr_list$stratum_def[2], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:pspread_loss_rel`[aa])
    input_def_list[[15]] <- list(input_hdr_list$stratum_def[2], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:vapor_loss_rel`[aa])
    input_def_list[[16]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k1`[aa])
    input_def_list[[17]] <- list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_shrub.def:biomass_loss_rel_k2`[aa])
    # Upper canopy fire parameters
    input_def_list[[18]] <- list(input_hdr_list$stratum_def[1], "pspread_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:pspread_loss_rel`[aa])
    input_def_list[[19]] <- list(input_hdr_list$stratum_def[1], "vapor_loss_rel", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:vapor_loss_rel`[aa])
    input_def_list[[20]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k1", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k1`[aa])
    input_def_list[[21]] <- list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k2", sobol_model$X$`ws_p300/defs/veg_p300_conifer.def:biomass_loss_rel_k2`[aa])
    
    # Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
    # input_dated_seq_list <- NULL
    input_dated_seq_list = list()
    input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=7,hour=1,value=sobol_model$X$pspread[aa],stringsAsFactors=FALSE)
    
    
    run_rhessys(parameter_method = parameter_method,
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
    
    print(paste("Simulation 1: Run #", aa, "of", nrow(sobol_model$X)))
  }
)

beep(1)




