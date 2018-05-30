# Run RHESSys sensitivity runs
# 
# This script calls sobol_runs_p300 in 2.2.


source("R/0.1_utilities.R")
source("R/2.2_sobol_rhessys_setup_p300.R")

# ---------------------------------------------------------------------
# P300

# Set worldfile at various stand ages 
stand_ages <- c("ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state",
                "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state")

# ----
# Run model for Sobol sensitivity

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[1],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND1
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[2],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND2
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[3],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND3
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[4],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND4
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[5],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND5
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[6],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND6
)

sobol_runs_p300(
  core_par_path=OUTPUT_DIR_1_P300_TOP_PS,
  sobol_par_path=RHESSYS_PAR_SOBOL_2.1_P300,
  world=stand_ages[7],
  hdr="2.3",
  output_path=RHESSYS_OUT_DIR_2.3_P300_STAND7
)


# ---------------------------------------------------------------------
# 
