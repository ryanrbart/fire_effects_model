# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

library(rhessysR)
library(ggplot2)


# ---------------------------------------------------------------------
# Inputs



OUTPUT_DIR <- "outputs"
PATCH_SIM_DIR <- file.path(OUTPUT_DIR, "patch_sim")

P300_MODEL_PATCH_SIM <- "ws_p300/out/p300_patch_simulation/patch_sim"

# ---------------------------------------------------------------------
# Functions


make_basic_timeseries_plot = function(plot_name, dataframe, variable, path){
  x <- ggplot(data = dataframe) +
    geom_line(aes(x=date,y=variable, linetype=as.character(names)))
  plot(x)
  ggsave(plot_name,plot = x, path = path)
}


# ---------------------------------------------------------------------
# P300 Patch Simulation Output

p300_patch_sim = readin_rhessys_output(P300_MODEL_PATCH_SIM, b=1, g=1, c=1, p=1)

bd = p300_spinup$bd
bdg = p300_spinup$bdg
pd = p300_spinup$pd
pdg = p300_spinup$pdg
cd = separate_canopy_output(p300_spinup$cd, 2)
cdg = separate_canopy_output(p300_spinup$cdg, 2)


# ---------------------------------------------------------------------
# P300 Evaluation

make_basic_timeseries_plot(plot_name = "p300_patch_sim_height.pdf", dataframe = cd, variable = cd$height, path = PATCH_SIM_DIR)

make_basic_timeseries_plot(plot_name = "p300_patch_sim_lai.pdf", dataframe = cd, variable = cd$lai, path = PATCH_SIM_DIR)








