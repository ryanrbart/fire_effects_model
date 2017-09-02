# Utilities for fire effects analysis
# Includes files/directories and functions
 

# ---------------------------------------------------------------------
# Libraries
library(RHESSysIOinR)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(sensitivity)
library(beepr)
library(lhs)
library(KScorrect)


# ---------------------------------------------------------------------
# Files and Directories

# RHESSys outputs
RHESSYS_OUT_DIR_1.1_P300 <- "ws_p300/out/1.1_p300_patch_simulation"
RHESSYS_ALLSIM_DIR_1.1_P300 <- file.path(RHESSYS_OUT_DIR_1.1_P300, "allsim")
RHESSYS_PAR_FILE_1.1_P300 <- file.path(RHESSYS_OUT_DIR_1.1_P300, "patch_sim_parameter_sets.csv")

RHESSYS_OUT_DIR_1.3_P300 <- "ws_p300/out/1.3_p300_patch_simulation"
RHESSYS_PAR_FILE_1.3_P300 <- file.path(RHESSYS_OUT_DIR_1.3_P300, "patch_sim_parameter_sets.csv")

RHESSYS_OUT_DIR_2.1_P300 <- "ws_p300/out/2.1_p300_patch_sens"
RHESSYS_PAR_SOBOL_2.1_P300 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "patch_fire_par_sobol.csv")
RHESSYS_OUT_DIR_2.1_P300_STAND1 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "stand1")
RHESSYS_ALLSIM_DIR_2.1_P300_STAND1 <- file.path(RHESSYS_OUT_DIR_2.1_P300_STAND1, "allsim")
RHESSYS_OUT_DIR_2.1_P300_STAND2 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "stand2")
RHESSYS_ALLSIM_DIR_2.1_P300_STAND2 <- file.path(RHESSYS_OUT_DIR_2.1_P300_STAND2, "allsim")
RHESSYS_OUT_DIR_2.1_P300_STAND3 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "stand3")
RHESSYS_ALLSIM_DIR_2.1_P300_STAND3 <- file.path(RHESSYS_OUT_DIR_2.1_P300_STAND3, "allsim")
RHESSYS_OUT_DIR_2.1_P300_STAND4 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "stand4")
RHESSYS_ALLSIM_DIR_2.1_P300_STAND4 <- file.path(RHESSYS_OUT_DIR_2.1_P300_STAND4, "allsim")

# ---

# RHESSys outputs
RHESSYS_OUT_DIR_1.1_RS <- "ws_rs/out/1.1_rs_patch_simulation"
RHESSYS_ALLSIM_DIR_1.1_RS <- file.path(RHESSYS_OUT_DIR_1.1_RS, "allsim")
RHESSYS_PAR_FILE_1.1_RS <- file.path(RHESSYS_OUT_DIR_1.1_RS, "patch_sim_parameter_sets.csv")

RHESSYS_OUT_DIR_1.3_RS <- "ws_rs/out/1.3_rs_patch_simulation"
RHESSYS_PAR_FILE_1.3_RS <- file.path(RHESSYS_OUT_DIR_1.3_RS, "patch_sim_parameter_sets.csv")

RHESSYS_OUT_DIR_2.1_RS <- "ws_rs/out/2.1_rs_patch_sens"
RHESSYS_PAR_SOBOL_2.1_RS <- file.path(RHESSYS_OUT_DIR_2.1_RS, "patch_fire_par_sobol.csv")
RHESSYS_OUT_DIR_2.1_RS_STAND1 <- file.path(RHESSYS_OUT_DIR_2.1_RS, "stand1")
RHESSYS_ALLSIM_DIR_2.1_RS_STAND1 <- file.path(RHESSYS_OUT_DIR_2.1_RS_STAND1, "allsim")
RHESSYS_OUT_DIR_2.1_RS_STAND2 <- file.path(RHESSYS_OUT_DIR_2.1_RS, "stand2")
RHESSYS_ALLSIM_DIR_2.1_RS_STAND2 <- file.path(RHESSYS_OUT_DIR_2.1_RS_STAND2, "allsim")
RHESSYS_OUT_DIR_2.1_RS_STAND3 <- file.path(RHESSYS_OUT_DIR_2.1_RS, "stand3")
RHESSYS_ALLSIM_DIR_2.1_RS_STAND3 <- file.path(RHESSYS_OUT_DIR_2.1_RS_STAND3, "allsim")
RHESSYS_OUT_DIR_2.1_RS_STAND4 <- file.path(RHESSYS_OUT_DIR_2.1_RS, "stand4")
RHESSYS_ALLSIM_DIR_2.1_RS_STAND4 <- file.path(RHESSYS_OUT_DIR_2.1_RS_STAND4, "allsim")

# ---


# RHESSYS_OUT_DIR_2.1_P300 <- "ws_p300/out/2.1_p300_patch_sens1"
# RHESSYS_ALLSIM_DIR_2.1_P300 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "allsim")
# RHESSYS_PAR_FILE_2.1_P300 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "patch_fire_parameter_sets.csv")
# RHESSYS_ALL_OPTION_2.1_P300 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "patch_fire_all_options.csv")
# RHESSYS_PAR_SOBOL_2.1_P300 <- file.path(RHESSYS_OUT_DIR_2.1_P300, "patch_fire_par_sobol.csv")
# 
# 
# RHESSYS_OUT_DIR_P300_3.1 <- "ws_p300/out/3.1_p300_patch_fire"
# RHESSYS_ALLSIM_DIR_P300_3.1 <- file.path(RHESSYS_DIR_p300_3.1, "allsim")
# RHESSYS_PARAMETER_FILE_P300_3.1_SIM <- file.path(RHESSYS_DIR_p300_3.1, "patch_sim_parameter_sets.csv")
# RHESSYS_PARAMETER_FILE_P300_3.1_FIRE <- file.path(RHESSYS_DIR_p300_3.1, "patch_fire_all_options.csv")

# ---
# R Outputs
OUTPUT_DIR <- "outputs"

OUTPUT_DIR_1 <- file.path(OUTPUT_DIR, "1_patch_sim")
OUTPUT_DIR_1_P300_TOP_PS <- file.path(OUTPUT_DIR_1, "1_p300_selected_ps.csv")
OUTPUT_DIR_1_RS_TOP_PS <- file.path(OUTPUT_DIR_1, "1_rs_selected_ps.csv")
OUTPUT_DIR_1_SF_TOP_PS <- file.path(OUTPUT_DIR_1, "1_sf_selected_ps.csv")
OUTPUT_DIR_1_HJA_TOP_PS <- file.path(OUTPUT_DIR_1, "1_hja_selected_ps.csv")

OUTPUT_DIR_2 <- file.path(OUTPUT_DIR, "2_patch_sens")

OUTPUT_DIR_3 <- file.path(OUTPUT_DIR, "3_patch_fire")


# ---
# Display Figures
DISPLAY_FIGURES_DIR <- file.path(OUTPUT_DIR, "display_graphics")


# ---------------------------------------------------------------------
# Functions

y_to_wy = function(year, month, start.month=10){
  wateryear <- ifelse(month >= start.month, year + 1, year)
  return(wateryear)
}



