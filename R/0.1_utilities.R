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


# ---------------------------------------------------------------------
# Files and Directories

# RHESSys outputs
RHESSYS_DIR_p300_1.1 <- "ws_p300/out/1.1_p300_patch_simulation"
ALLSIM_DIR_p300_1.1 <- file.path(RHESSYS_DIR_p300_1.1, "allsim")
PARAMETER_FILE_p300_1.1<- file.path(RHESSYS_DIR_p300_1.1, "patch_sim_parameter_sets.txt")

RHESSYS_DIR_p300_2.1 <- "ws_p300/out/2.1_p300_patch_fire"
ALLSIM_DIR_p300_2.1 <- file.path(RHESSYS_DIR_p300_2.1, "allsim")
PARAMETER_FILE_p300_2.1_SIM <- file.path(RHESSYS_DIR_p300_2.1, "patch_sim_parameter_sets.txt")
PARAMETER_FILE_p300_2.1_FIRE <- file.path(RHESSYS_DIR_p300_2.1, "patch_fire_parameter_sets.txt")

# ---
# R Outputs
OUTPUT_DIR <- "outputs"
OUTPUT_DIR_1 <- file.path(OUTPUT_DIR, "1_patch_sim")
OUTPUT_DIR_2 <- file.path(OUTPUT_DIR, "2_patch_sim")

# ---
# Display Figures
DISPLAY_FIGURES_DIR <- file.path(OUTPUT_DIR, "display_graphics")


# ---------------------------------------------------------------------
# Functions

y_to_wy = function(year, month, start.month=10){
  wateryear <- ifelse(month >= start.month, year + 1, year)
  return(wateryear)
}



