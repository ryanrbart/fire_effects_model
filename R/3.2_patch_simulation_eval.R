# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

library(rhessysR)
library(ggplot2)

theme_set(theme_bw(base_size = 18))


# ---------------------------------------------------------------------
# Input and output paths 

P300_PATCH_SIM <- "ws_p300/out/3.1_p300_patch_simulation/patch_sim_d"

OUTPUT_DIR <- "outputs"
PATCH_SIM_DIR <- file.path(OUTPUT_DIR, "3_patch_sim")

# ---------------------------------------------------------------------
# Functions


# ---------------------------------------------------------------------
# P300 Patch Simulation Output

p300_patch_sim <- readin_rhessys_output(P300_PATCH_SIM, b=1, g=1, c=1, p=1)


bd <- p300_patch_sim$bd
bd <- mutate(bd, litrc_new = litrc * 1000)
bdg <- p300_patch_sim$bdg
pd <- p300_patch_sim$pd
pdg <- p300_patch_sim$pdg
pdg <- mutate(pdg, soil1c_new = soil1c * 1000, soil2c_new = soil2c * 1000, soil3c_new = soil3c * 1000, soil4c_new = soil4c * 1000)
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
# Simulation time-series graphics

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
  summarize(avg_leaf_c = mean(leafc), avg_stem_c = mean(stemc), avg_root_c = mean(rootc))

# ----

# Make canopy 1 comparison plot
x <- cdg3 %>%
  tidyr::gather("c_type", "c_frac", c(avg_leaf_c, avg_stem_c, avg_root_c)) %>%
  dplyr::filter(names == 1) %>%
  ggplot() +
    geom_line(aes(x = wy, y = c_frac, color=c_type), size = 1.2) +
    geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
    labs(title = "Conifer", x = "Wateryear", y = "Carbon (g/m2)") +
    scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
    theme(legend.position = "bottom")
plot(x)
ggsave("p300_c_frac_layer_1_3_3.pdf",plot = x, path = PATCH_SIM_DIR)

# ----

# Make canopy 2 comparison plot
x <- cdg3 %>%
  tidyr::gather("c_type", "c_frac", c(avg_leaf_c, avg_stem_c, avg_root_c)) %>%
  dplyr::filter(names == 2) %>%
  ggplot() +
  geom_line(aes(x = wy, y = c_frac, color=c_type), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "Shrub", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
  theme(legend.position = "bottom")
plot(x)
ggsave("p300_c_frac_layer_2_3_3.pdf",plot = x, path = PATCH_SIM_DIR)

# ----

# Make height comparison plot
x <- cd %>%
  group_by(wy, names) %>%
  summarize(avg_height = mean(height)) %>%
  ggplot() +
    geom_line(aes(x=wy,y=avg_height, color=as.character(names)), size = 1.2) +
#    geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
#    geom_hline(yintercept= c(4,7), linetype=1, size=.4, color = "olivedrab3") +
#    xlim(1941,1960) +
#    ylim(0,.6) +
    labs(title = "Height", x = "Wateryear", y = "Height (meters)") +
    scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Overstory","Understory")) +
    theme(legend.position = "bottom")
plot(x)
ggsave("p300_height_3_3.pdf",plot = x, path = PATCH_SIM_DIR)

# ----

# Make litter, soil, cwdc comparison plot (Uses single soil store)
x <- cdg %>%
  group_by(date, wy) %>%
  summarize(cwd = sum(cwdc)) %>%
  left_join(., select(pdg, soil1c_new, date), by = "date") %>%
  left_join(., select(bd, litrc_new, date), by = "date") %>%
  tidyr::gather("var_type", "patch_var", c(cwd, soil1c_new, litrc_new)) %>%
  group_by(wy, var_type) %>%
  summarize(avg_patch_var = mean(patch_var)) %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_patch_var, color=as.character(var_type)), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "Ground Stores", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c(cwd = "Coarse Woody Debris", 
                                                                     litrc_new = "Litter",
                                                                     soil1c_new = "Soil1")) +
  theme(legend.position = "bottom")
plot(x)
ggsave("p300_litter_soil_cwd_3_3.pdf",plot = x, path = PATCH_SIM_DIR)


# Make litter, soil, cwdc comparison plot (Compares all soil stores)
# x <- cdg %>%
#   group_by(date, wy) %>%
#   summarize(cwd = sum(cwdc)) %>%
#   left_join(., select(pdg, soil1c_new, date), by = "date") %>%
#   left_join(., select(pdg, soil2c_new, date), by = "date") %>%
#   left_join(., select(pdg, soil3c_new, date), by = "date") %>%
#   left_join(., select(pdg, soil4c_new, date), by = "date") %>%
#   left_join(., select(bd, litrc_new, date), by = "date") %>%
#   tidyr::gather("var_type", "patch_var", c(cwd, soil1c_new, soil2c_new, soil3c_new, soil4c_new, litrc_new)) %>%
#   group_by(wy, var_type) %>%
#   summarize(avg_patch_var = mean(patch_var)) %>%
#   ggplot() +
#     geom_line(aes(x=wy,y=avg_patch_var, color=as.character(var_type)), size = 1.2) +
#     geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
#     labs(title = "Ground Stores", x = "Wateryear", y = "Carbon (g/m2)") +
#     scale_color_brewer(palette = "Set2", name="Store Type", labels = c(cwd = "Coarse Woody Debris",
#                                                                        litrc_new = "Litter",
#                                                                        soil1c_new = "Soil1",
#                                                                        soil2c_new = "Soil2",
#                                                                        soil3c_new = "Soil3",
#                                                                        soil4c_new = "Soil4")) +
#     theme(legend.position = "bottom") #  +
# #    coord_cartesian(ylim = c(0,1000))
# plot(x)
# ggsave("p300_litter_soil_cwd.pdf",plot = x, path = PATCH_SIM_DIR)

# ----


# Exploration of height bug

x <- cd %>%
  group_by(wy, names) %>%
  summarize(avg_ga = mean(ga)) %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_ga, color=as.character(names)), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "GA", x = "Wateryear", y = "GA") +
  scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Overstory","Understory")) +
  theme(legend.position = "bottom")
plot(x)
ggsave("p300_ga_11_11.pdf",plot = x, path = PATCH_SIM_DIR)


x <- pd %>%
  group_by(wy) %>%
  summarize(avg_ga = mean(ga)) %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_ga), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "GA", x = "Wateryear", y = "GA") +
  scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Overstory","Understory")) +
  theme(legend.position = "bottom")
plot(x)
ggsave("p300_ga_11_11.pdf",plot = x, path = PATCH_SIM_DIR)



x <- pd %>%
  group_by(wy) %>%
  summarize(avg_ga = mean(ga)) %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_ga), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "GA", x = "Wateryear", y = "GA") +
  scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Overstory","Understory")) +
  theme(legend.position = "bottom")
plot(x)

#x <- select(cd, wyd, names, height, lai, ga, psi, date)
# 
# 
# x <- select(cd, names, lai, date)
# lai <- tidyr::spread(x, names, lai)
# View(lai)
# 
# x <- select(cd, names, height, date)
# height <- tidyr::spread(x, names, height)
# View(height)
# 
# x <- select(cd, names, psi, date)
# psi <- tidyr::spread(x, names, psi)
# View(psi)
# 
# x <- select(cd, names, evap, date)
# evap <- tidyr::spread(x, names, evap)
# View(evap)
# 
# x <- select(cd, names, trans, date)
# trans <- tidyr::spread(x, names, trans)
# View(trans)
# 
# x <- select(cd, names, ga, date)
# ga <- tidyr::spread(x, names, ga)
# View(ga)
# 
# x <- select(cd, names, psn_to_cpool, date)
# psn_to_cpool <- tidyr::spread(x, names, psn_to_cpool)
# View(psn_to_cpool)
# 
# x <- select(cdg, names, dead_stemc, date)
# dead_stemc <- tidyr::spread(x, names, dead_stemc)
# View(dead_stemc)
# 
# x <- select(cdg, names, live_stemc, date)
# live_stemc <- tidyr::spread(x, names, live_stemc)
# View(live_stemc)
# 
# x <- select(cdg, names, leafc, date)
# leafc <- tidyr::spread(x, names, leafc)
# View(leafc)
# 
# x <- select(cdg, names, frootc, date)
# frootc <- tidyr::spread(x, names, frootc)
# View(frootc)
# 
# x <- select(cdg, names, live_crootc, date)
# live_crootc <- tidyr::spread(x, names, live_crootc)
# View(live_crootc)
# 
# x <- select(cdg, names, mresp, date)
# mresp <- tidyr::spread(x, names, mresp)
# View(mresp)

# 
# # ---------------------------------------------------------------------
# # Functions
# 
# make_basic_timeseries_plot_1can = function(plot_name, dataframe, variable, path){
#   x <- ggplot(data = dataframe) +
#     geom_line(aes(x=date,y=variable))
#   plot(x)
#   ggsave(plot_name,plot = x, path = path)
# }
# 
# make_basic_timeseries_plot_2can = function(plot_name, dataframe, variable, path){
#   x <- ggplot(data = dataframe) +
#     geom_line(aes(x=date,y=variable, linetype=as.character(names)))
#   plot(x)
#   ggsave(plot_name,plot = x, path = path)
# }
# 
# # ---------------------------------------------------------------------
# # P300 Evaluation
# 
# 
# make_basic_timeseries_plot_1can(plot_name = "p300_patch_sim_litrc.pdf", dataframe = bd, variable = bd$litrc, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_1can(plot_name = "p300_patch_sim_soil1c.pdf", dataframe = pdg, variable = pdg$soil1c, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_cwdc.pdf", dataframe = cdg, variable = cdg$cwdc, path = PATCH_SIM_DIR)
# 
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_height.pdf", dataframe = cd, variable = cd$height, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_lai.pdf", dataframe = cd, variable = cd$lai, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_leafc.pdf", dataframe = cdg, variable = cdg$leafc, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_live_stemc.pdf", dataframe = cdg, variable = cdg$live_stemc, path = PATCH_SIM_DIR)
# 
# make_basic_timeseries_plot_2can(plot_name = "p300_patch_sim_dead_stemc.pdf", dataframe = cdg, variable = cdg$dead_stemc, path = PATCH_SIM_DIR)
# 






