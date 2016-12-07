# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

library(rhessysR)
library(ggplot2)


# ---------------------------------------------------------------------
# Input and output paths 

P300_PATCH_SIM <- "ws_p300/out/p300_patch_simulation/patch_sim"

OUTPUT_DIR <- "outputs"
PATCH_SIM_DIR <- file.path(OUTPUT_DIR, "3_patch_sim")

# ---------------------------------------------------------------------
# Functions

make_basic_timeseries_plot_1can = function(plot_name, dataframe, variable, path){
  x <- ggplot(data = dataframe) +
    geom_line(aes(x=date,y=variable))
  plot(x)
  ggsave(plot_name,plot = x, path = path)
}

make_basic_timeseries_plot_2can = function(plot_name, dataframe, variable, path){
  x <- ggplot(data = dataframe) +
    geom_line(aes(x=date,y=variable, linetype=as.character(names)))
  plot(x)
  ggsave(plot_name,plot = x, path = path)
}


# ---------------------------------------------------------------------
# P300 Patch Simulation Output

p300_patch_sim <- readin_rhessys_output(P300_PATCH_SIM, b=1, g=1, c=1, p=1)

bd <- p300_patch_sim$bd
bdg <- p300_patch_sim$bdg
pd <- p300_patch_sim$pd
pdg <- p300_patch_sim$pdg
cd <- separate_canopy_output(p300_patch_sim$cd, 2)
cdg <- separate_canopy_output(p300_patch_sim$cdg, 2)

bd.wy <- p300_patch_sim$bd.wy
bdg.wy <- p300_patch_sim$bdg.wy
pd.wy <- p300_patch_sim$pd.wy
pdg.wy <- p300_patch_sim$pdg.wy
cd.wy <- p300_patch_sim$cd.wy
cdg.wy <- p300_patch_sim$cdg.wy

bd.wyd <- p300_patch_sim$bd.wyd
bdg.wyd <- p300_patch_sim$bdg.wyd
pd.wyd <- p300_patch_sim$pd.wyd
pdg.wyd <- p300_patch_sim$pdg.wyd
cd.wyd <- p300_patch_sim$cd.wyd
cdg.wyd <- p300_patch_sim$cdg.wyd


# ----
# AGU 2016 graphics

# Make Leaf/stem/root proportion figures

#Create new variables
cdg2 <- cdg %>%
  dplyr::mutate(., stemc = live_stemc + dead_stemc) %>%              # Aboveground stem carbon
  dplyr::mutate(., rootc = frootc + live_crootc + dead_crootc) %>%   # Aboveground stem carbon
  dplyr::mutate(., leaf_frac = leafc / plantc) %>%
  dplyr::mutate(., stem_frac = stemc / plantc) %>%
  dplyr::mutate(., root_frac = rootc / plantc)

cdg3 <- cdg2 %>%
  group_by(wy, names) %>%
  summarize(avg_leaf_frac = mean(leaf_frac), avg_stem_frac = mean(stem_frac), avg_root_frac = mean(root_frac))

x <- cdg3 %>%
  tidyr::gather("frac_type", "c_frac", c(avg_leaf_frac, avg_stem_frac, avg_root_frac)) %>%
  dplyr::filter(names == 1) %>%
  ggplot() +
    geom_line(aes(x = wy, y = c_frac, linetype=as.character(frac_type))) +
    geom_vline(xintercept= c(1946,1951,1961,1981,2021), color="red")
plot(x)
ggsave("agu16_c_frac_layer_1.pdf",plot = x, path = PATCH_SIM_DIR)

x <- cdg3 %>%
  tidyr::gather("frac_type", "c_frac", c(avg_leaf_frac, avg_stem_frac, avg_root_frac)) %>%
  dplyr::filter(names == 2) %>%
  ggplot() +
  geom_line(aes(x = wy, y = c_frac, linetype=as.character(frac_type))) +
  geom_vline(xintercept= c(1946,1951,1961,1981,2021), color="red")
plot(x)
ggsave("agu16_c_frac_layer_2.pdf",plot = x, path = PATCH_SIM_DIR)


# Make height comparison plot
x <- cd %>%
  group_by(wy, names) %>%
  summarize(avg_height = mean(height)) %>%
  ggplot() +
    geom_line(aes(x=wy,y=avg_height, linetype=as.character(names))) +
    geom_vline(xintercept= c(1946,1951,1961,1981,2021), color="red")
plot(x)
ggsave("agu16_height.pdf",plot = x, path = PATCH_SIM_DIR)


# Make litter, soil, cwdc comparison plot
x <- cdg %>%
  group_by(date, wy) %>%
  summarize(cwd = sum(cwdc)) %>%
  left_join(., select(pdg, soil1c, date), by = "date") %>%
  left_join(., select(bd, litrc, date), by = "date") %>%
  tidyr::gather("var_type", "patch_var", c(cwd, soil1c, litrc)) %>%
  group_by(wy, var_type) %>%
  summarize(avg_patch_var = mean(patch_var)) %>%
  ggplot() +
    geom_line(aes(x=wy,y=avg_patch_var, linetype=as.character(var_type))) +
    geom_vline(xintercept= c(1946,1951,1961,1981,2021), color="red")
plot(x)
ggsave("agu16_litter_soil_cwd.pdf",plot = x, path = PATCH_SIM_DIR)



# ---------------------------------------------------------------------
# P300 Evaluation



make_basic_timeseries_plot_1can(plot_name = "p300_patch_sim_litrc.pdf", dataframe = bd, variable = bd$litrc, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_1can(plot_name = "p300_patch_sim_soil1c.pdf", dataframe = pdg, variable = pdg$soil1c, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_cwdc.pdf", dataframe = cdg, variable = cdg$cwdc, path = PATCH_SIM_DIR)


make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_height.pdf", dataframe = cd, variable = cd$height, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_lai.pdf", dataframe = cd, variable = cd$lai, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_leafc.pdf", dataframe = cdg, variable = cdg$leafc, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_live_stemc.pdf", dataframe = cdg, variable = cdg$live_stemc, path = PATCH_SIM_DIR)

make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_dead_stemc.pdf", dataframe = cdg, variable = cdg$dead_stemc, path = PATCH_SIM_DIR)







