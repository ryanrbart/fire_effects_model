# Patch Missing Sobol Output
#
# This script isolates sobol calibration runs that are missing and reruns those
# parameters sets. 
#
# It is unclear why output is missing from periodic rhessys runs,
# but approximately 1 in ~500-1000 runs is missing. Sobol_martinez can handle
# missing data, but other Sobol tests cannot.
#

source("R/0.1_utilities.R")
source("R/2.2_sobol_rhessys_setup_p300.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Isolate missing parameter sets

patch_missing_runs <- function(sobol_par, rhessys_allsim_path, output_path){
  
  # Read in allsim file to detect whether there are any missing data It
  # appears that the same error shows up in each allsim file, so only need to
  # look for missing data in one file.

  var_names <- c("canopy_target_prop_mort", "canopy_target_prop_mort_consumed",
                 "canopy_target_prop_c_consumed", "canopy_target_prop_c_remain")
  allsim_file <- read_tsv(file.path(rhessys_allsim_path, var_names[1]), skip = 2, col_names = FALSE,
                col_types = cols(X1 = col_skip()))
  
  # Identify rows with na
  tmp1 <- allsim_file %>% 
    sapply(function(x) sum(is.na(x)))
  na_rows <- names(tmp1[tmp1>0])
  
  # Establish parameter sets (ps) for missing data
  if (identical(na_rows, character(0))){
    print(paste("Stand", aa, ": Data looks awesome. Time to move on"))
    ps_missing <- FALSE
  } else {
    print(paste("Stand", aa, ": Data missing at:", na_rows))
    ps_all <-  read_csv(sobol_par, col_names = TRUE)
    ps_na_rows <- bind_cols(run = sapply(seq_len(nrow(ps_all)), function (x) paste("X",as.character(x+1),sep="")), ps_all)
    ps_patch <- ps_na_rows %>% 
      dplyr::filter(run%in%na_rows)
    ps_missing <- TRUE
    
    # Write out sobol missing parameter sets
    write.csv(ps_patch, output_path, row.names = FALSE, quote=FALSE)
  }

  return(ps_missing)
}


# Call patch_missing_runs
# ----
# P300

allsim_2.3_p300 <- c(RHESSYS_ALLSIM_DIR_2.3_P300_STAND1,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND2,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND3,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND4,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND5,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND6,
                     RHESSYS_ALLSIM_DIR_2.3_P300_STAND7)

sobol_par_missing_p300 <- c(RHESSYS_PAR_MISSING_2.4_P300_STAND1,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND2,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND3,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND4,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND5,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND6,
                            RHESSYS_PAR_MISSING_2.4_P300_STAND7)

ps_missing_p300 <- vector()
for (aa in seq_along(allsim_2.3_p300)){
  # Find missing parameters
  ps_missing <- patch_missing_runs(sobol_par = RHESSYS_PAR_SOBOL_2.1_P300,
                                   rhessys_allsim_path = allsim_2.3_p300[aa],
                                   output_path=sobol_par_missing_p300[aa])
  ps_missing_p300[aa] <- ps_missing
}

# ----
# RS

# ******* Repeat for each location *******




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# RHESSys simulations the missing parameter sets


# ----
# P300

# Set worldfile at various stand ages 
world_ages_p300 <- c("ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state",
                     "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state")

hdr_p300 <- c("2.4_p300_stand1",
              "2.4_p300_stand2",
              "2.4_p300_stand3",
              "2.4_p300_stand4",
              "2.4_p300_stand5",
              "2.4_p300_stand6",
              "2.4_p300_stand7")

output_2.4_p300 <- c(RHESSYS_OUT_DIR_2.4_P300_STAND1,
                     RHESSYS_OUT_DIR_2.4_P300_STAND2,
                     RHESSYS_OUT_DIR_2.4_P300_STAND3,
                     RHESSYS_OUT_DIR_2.4_P300_STAND4,
                     RHESSYS_OUT_DIR_2.4_P300_STAND5,
                     RHESSYS_OUT_DIR_2.4_P300_STAND6,
                     RHESSYS_OUT_DIR_2.4_P300_STAND7)


for (aa in seq_along(world_ages_p300)){
  if (ps_missing_p300[aa] == TRUE){
    sobol_runs_p300(
      core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
      sobol_par_path=sobol_par_missing_p300[aa],
      world=world_ages_p300[aa],
      hdr=hdr_p300[aa],
      output_path=output_2.4_p300[aa]
    )
  }
}

# ******* Repeat for each location *******


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Stick data back together again

# humpty_dumpty name comes from two things. First, recent read a humpty dumpty
# book to kids. Second, this function puts the results back together again.

humpty_dumpty <- function(var_names,
                          rhessys_allsim_path_orig,
                          sobol_par_patched, 
                          rhessys_allsim_path_patched){

  # Loop through variables 
  for (aa in seq_along(var_names)){
    
    # Import Original data
    allsim_file_orig <- read_tsv(file.path(rhessys_allsim_path_orig, var_names[aa]), skip = 0, col_names = FALSE)
    
    # Import Patched data
    allsim_file_patched <- read_tsv(file.path(rhessys_allsim_path_patched, var_names[aa]), skip = 0, 
                                    col_names = FALSE, col_types = cols(X1 = col_skip()))
    sobol_ps_patched <- read_csv(sobol_par_patched)
    
    # Get runs id's that need patching and add to allsim_file_patched
    patched_runs <- sobol_ps_patched$run
    names(allsim_file_patched) <- patched_runs
    
    # Substitue patched runs for NA runs in allsim_file_orig
    allsim_file_new <- allsim_file_orig %>% 
      dplyr::select(-patched_runs) %>%         # Eliminate columns with missing data
      dplyr::bind_cols(allsim_file_patched)     # Add back those colums with patched data
    
    allsim_file_new <- dplyr::select(allsim_file_new, mixedorder(names(allsim_file_new))) # Rearrange columns to original order 
    
    # Export allsim_file_new
    write_tsv(allsim_file_new, file.path(rhessys_allsim_path_patched,paste(var_names[aa],"_patched",sep="")),
              col_names=FALSE, na="")
  }
}

not_humpty_dumpty <- function(var_names,
                              rhessys_allsim_path_orig,
                              rhessys_allsim_path_patched){
  # Loop through variables 
  for (aa in seq_along(var_names)){
    # Import Original data
    allsim_file_orig <- read_tsv(file.path(rhessys_allsim_path_orig, var_names[aa]), skip = 0, col_names = FALSE)
    # Export allsim_file_new
    write_tsv(allsim_file_orig, file.path(rhessys_allsim_path_patched,paste(var_names[aa],"_patched",sep="")),
              col_names=FALSE, na="")
  }
}


# ----
# P300

var_names <- c("canopy_target_prop_mort", "canopy_target_prop_mort_consumed",
               "canopy_target_prop_c_consumed", "canopy_target_prop_c_remain")

allsim_2.4_p300 <- c(RHESSYS_ALLSIM_DIR_2.4_P300_STAND1,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND2,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND3,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND4,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND5,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND6,
                     RHESSYS_ALLSIM_DIR_2.4_P300_STAND7)

for (aa in seq_along(allsim_2.3_p300)){
  if (ps_missing_p300[aa] == TRUE){
    # Copy rhessys output from 2.3 directory, patch missing, copy to 2.4 directory
    humpty_dumpty(var_names = var_names,
                  rhessys_allsim_path_orig = allsim_2.3_p300[aa],
                  sobol_par_patched = sobol_par_missing_p300[aa],
                  rhessys_allsim_path_patched = allsim_2.4_p300[aa])
  } else {
    # Copy rhessys output from 2.3 directory to 2.4 directory
    not_humpty_dumpty(var_names = var_names,
                      rhessys_allsim_path_orig = allsim_2.3_p300[aa],
                      rhessys_allsim_path_patched = allsim_2.4_p300[aa])
  }
}

# ----
# RS

# ******* Repeat for each location *******







