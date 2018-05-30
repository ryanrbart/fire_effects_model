# Patch Missing Sobol Output
#
# This script isolates sobol calibration runs that are missing and reruns those
# parameters sets. 
#
# It is unclear output is missing from periodic rhessys runs,
# but approximately 1 in ~500-1000 runs is missing. Sobol_martinez can handle
# missing data, but other Sobol tests cannot.
#
# 

source("R/0.1_utilities.R")
source("R/2.2_sobol_rhessys_setup_p300.R")

# ---------------------------------------------------------------------
# Isolate missing parameter sets

patch_missing_runs <- function(sobol_par, rhessys_allsim_path, output_path){
  
  # ---- 
  # Read in allsim file to detect whether there are any missing data It
  # appears that the same error shows up in each allsim file, so only need to
  # looko for missing data in one file.

  var_names <- c("canopy_target_prop_mort", "canopy_target_prop_mort_consumed",
                 "canopy_target_prop_c_consumed", "canopy_target_prop_c_remain")
  allsim_file <- read_tsv(file.path(rhessys_allsim_path, var_names[1]), skip = 2, col_names = FALSE,
                col_types = cols(X1 = col_skip()))
  
  # ----
  # Identify rows with na
  tmp1 <- allsim_file %>% 
    sapply(function(x) sum(is.na(x)))
  na_rows <- names(tmp1[tmp1>0])
  
  # ----
  # Establish parameter sets (ps) for missing data
  if (identical(na_rows, character(0))){
    print("Data looks awesome. Time to move on")
  } else {
    ps_all <-  read_csv(sobol_par, col_names = TRUE)
    ps_na_rows <- bind_cols(run = sapply(seq_len(nrow(ps_all)), function (x) paste("X",as.character(x+1),sep="")), ps_all)
    ps_patch <- ps_na_rows %>% 
      dplyr::filter(run==c(na_rows))
  }
  
  # Write out sobol missing parameter sets
  write.csv(ps_patch, output_path, row.names = FALSE, quote=FALSE)
}


# Find missing parameters (repeat for each location/stand age)
patch_missing_runs(sobol_par = RHESSYS_PAR_SOBOL_2.1_P300,
                   rhessys_allsim_path = RHESSYS_ALLSIM_DIR_2.3_P300_STAND1,
                   output_path=RHESSYS_PAR_MISSING_2.4_P300_STAND1)

# ******* Repeat for each location/stand age *******

# ---------------------------------------------------------------------
# RHESSys simulations the missing parameter sets

# ******* Make for loop to call rhessys multiple times *******

# Set worldfile at various stand ages 
stand_ages <- c("ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state")

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_MISSING_2.4_P300_STAND1,
  world=stand_ages[1],
  hdr="2.4_stand1",
  output_path=RHESSYS_OUT_DIR_2.4_P300_STAND1
)


# ---------------------------------------------------------------------
# Stick data back together again

# humpty_dumpty name comes from two things. First, recent read a humpty dumpty
# book to kids. Second, this function puts the results back together again.

humpty_dumpty <- function(sobol_par_orig, 
                          rhessys_allsim_path_orig,
                          sobol_par_patched, 
                          rhessys_allsim_path_patched){
  
  # Variable names
  var_names <- c("canopy_target_prop_mort", "canopy_target_prop_mort_consumed",
                 "canopy_target_prop_c_consumed", "canopy_target_prop_c_remain")

  # Loop through variables 
  for (aa in seq_along(var_names)){
    
    # Original data
    allsim_file_orig <- read_tsv(file.path(rhessys_allsim_path_orig, var_names[aa]), skip = 0, col_names = FALSE)
    sobol_ps_orig <- read_csv(sobol_par_orig)
    
    column_seq <- factor(names(allsim_file_orig), levels=names(allsim_file_orig))
    
    # Patched data
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
    
    # Export allsim_file_orig
    write_tsv(allsim_file_new, file.path(rhessys_allsim_path_patched,paste(var_names[aa],"_patched",sep="")),
              col_names=FALSE, na="")
  }
}


humpty_dumpty(sobol_par_orig = RHESSYS_PAR_SOBOL_2.1_P300,
              rhessys_allsim_path_orig = RHESSYS_ALLSIM_DIR_2.3_P300_STAND1,
              sobol_par_patched = RHESSYS_PAR_MISSING_2.4_P300_STAND1,
              rhessys_allsim_path_patched = RHESSYS_ALLSIM_DIR_2.4_P300_STAND1)



# ---------------------------------------------------------------------
# Todo list notes

# Further steps
# Need to test up test that will skip rhessys runs for data that doesn't need it.

# How do I put the data back together again?
# Import new results. Using column names that I know are junk, insert new data.
# Should I then export data as csv so I can import with readin_rhessys_output_cal, or make modified version of readin_rhessys_output_cal








  



