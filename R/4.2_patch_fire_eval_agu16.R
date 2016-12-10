# Patch Fire Evaluation
#
# Contains scripts for evaluating patch-level fire effects

library(rhessysR)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

theme_set(theme_grey(base_size = 16))


# ---------------------------------------------------------------------
# Input and output paths 

INPUT_DIR <- "ws_p300/out/p300_patch_fire_agu16"
ALLSIM_DIR <- file.path(INPUT_DIR, "allsim")

OUTPUT_DIR <- "outputs"
PATCH_FIRE_DIR <- file.path(OUTPUT_DIR, "4_patch_fire_agu16")

# ---------------------------------------------------------------------
# Functions


# ---------------------------------------------------------------------
# P300 Patch Fire Output

# Vector of outputs variable names
var_names_1can <- c("litrc", "soil1c")
var_names_2can <- c("lai", "height", "stemc", "leafc", "live_stemc", "dead_stemc", "cwdc")

# Parameter File
tmp = "patch_fire_parameter_sets.csv"
parameter_file <- file.path(INPUT_DIR, tmp)



p300_patch_fire1 = readin_rhessys_output2(var_names = var_names_1can,
                               path = ALLSIM_DIR,
                               initial_date = ymd("1941-10-1"),
                               parameter_file = parameter_file,
                               num_canopies = 1)

p300_patch_fire2 = readin_rhessys_output2(var_names = var_names_2can,
                                         path = ALLSIM_DIR,
                                         initial_date = ymd("1941-10-1"),
                                         parameter_file = parameter_file,
                                         num_canopies = 2)

head(p300_patch_fire1)
ls(p300_patch_fire1)
unique(p300_patch_fire1$var)
unique(p300_patch_fire2$var)


# Compute pre and post-effect differences
# For single layer data
p300_patch_fire_change1 = p300_patch_fire1 %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)

# For two layer data
p300_patch_fire_change2 = p300_patch_fire2 %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)

# For two layer data converting to single layer
p300_patch_fire_change3 = p300_patch_fire2 %>%
  group_by(run, dates, var, world_file, dated_seq_data) %>%
  summarize(value = sum(value)) %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)


head(p300_patch_fire_change1)

# ---------------------------------------------------------------------
# P300 Post-fire Change Evaluation

world_file_yr <- c(
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1947M10D1H1.state" = "5 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1954M10D1H1.state" = "12 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1962M10D1H1.state" = "20 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1972M10D1H1.state" = "30 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y1982M10D1H1.state" = "40 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y2002M10D1H1.state" = "60 yrs",
  "ws_p300/worldfiles/world.p300_30m_2can_patch_9445.Y2022M10D1H1.state" = "80 yrs"
)

canopy <- c("1" = "Overstory","2" = "Understory")


tmp = dplyr::filter(p300_patch_fire_change1, var == "litrc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr)) +
  theme(legend.position = "none") +
  labs(title = "Litter Carbon", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_litrc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire_change1, var == "soil1c")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr)) +
  theme(legend.position = "none") +
  labs(title = "Soil Carbon", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_soil1c.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire_change3, var == "cwdc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr, names = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Coarse Woody Debris Carbon", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_cwdc.pdf",plot = x, path = PATCH_FIRE_DIR)


# ----

tmp = dplyr::filter(p300_patch_fire_change2,var == "leafc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity", aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(names~world_file, labeller = labeller(world_file = world_file_yr, names = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Leaf Carbon", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_leafc.pdf",plot = x, path = PATCH_FIRE_DIR)


tmp = dplyr::filter(p300_patch_fire_change2,var == "stemc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(names~world_file, labeller = labeller(world_file = world_file_yr, names = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Stem Carbon", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)


tmp = dplyr::filter(p300_patch_fire_change2,var == "lai")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(names~world_file, labeller = labeller(world_file = world_file_yr, names = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Leaf Area Index", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_lai.pdf",plot = x, path = PATCH_FIRE_DIR)


tmp = dplyr::filter(p300_patch_fire_change2,var == "height")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_data,y=relative_change)) +
  facet_grid(names~world_file, labeller = labeller(world_file = world_file_yr, names = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Height", x = "Pspread", y = "Change (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
ggsave("p300_patch_sim_change_height.pdf",plot = x, path = PATCH_FIRE_DIR)


# # ---------------------------------------------------------------------
# # P300 Time Series Evaluation
# 
# tmp = dplyr::filter(p300_patch_fire1,var == "litrc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value)) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_litrc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire1,var == "soil1c")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value)) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_soil1c.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# 
# # ---
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "stemc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "lai")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_lai.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "height")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_height.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "leafc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_leafc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "live_stemc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_live_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "dead_stemc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_dead_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 
# tmp = dplyr::filter(p300_patch_fire2,var == "cwdc")
# x <- ggplot(data = tmp) +
#   geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
#   facet_grid(dated_seq_data~world_file) +
#   theme(legend.position = "none")
# plot(x)
# #ggsave("p300_patch_sim_cwdc.pdf",plot = x, path = PATCH_FIRE_DIR)
# 

















# __________
# Sensitivity Analysis

# ---------------------------------------------------------------------
# P300 Patch Fire Output



# Input and output paths 

INPUT_DIR <- "ws_p300/out/p300_patch_fire_agu16_sens"
ALLSIM_DIR <- file.path(INPUT_DIR, "allsim")

OUTPUT_DIR <- "outputs"
PATCH_FIRE_DIR <- file.path(OUTPUT_DIR, "4_patch_fire_agu16")



# Vector of outputs variable names
var_names_1can <- c("litrc", "soil1c")
var_names_2can <- c("lai", "height", "stemc", "leafc", "live_stemc", "dead_stemc", "cwdc")

# Parameter File
tmp = "patch_fire_parameter_sets.csv"
parameter_file <- file.path(INPUT_DIR, tmp)



p300_patch_fire1 = readin_rhessys_output2(var_names = var_names_1can,
                                          path = ALLSIM_DIR,
                                          initial_date = ymd("1941-10-1"),
                                          parameter_file = parameter_file,
                                          num_canopies = 1)

p300_patch_fire2 = readin_rhessys_output2(var_names = var_names_2can,
                                          path = ALLSIM_DIR,
                                          initial_date = ymd("1941-10-1"),
                                          parameter_file = parameter_file,
                                          num_canopies = 2)

# Rename parameters
p300_patch_fire1 <- p300_patch_fire1 %>%
  rename(k1 = awks.change.def.biomass_loss_rel_k1.awk) %>%
  rename(k2 = awks.change.def.biomass_loss_rel_k2.awk) %>%
  rename(k1_1 = awks.change.def.biomass_loss_rel_k1.awk.1) %>%
  rename(k2_1 = awks.change.def.biomass_loss_rel_k2.awk.1) %>%
  rename(over_thresh = awks.change.def.overstory_height_thresh.awk) %>%
  rename(over_thresh_1 = awks.change.def.overstory_height_thresh.awk.1) %>%
  rename(pspread_loss = awks.change.def.pspread_loss_rel.awk) %>%
  rename(pspread_loss_1 = awks.change.def.pspread_loss_rel.awk.1) %>%
  rename(under_thresh = awks.change.def.understory_height_thresh.awk) %>%
  rename(under_thresh_1 = awks.change.def.understory_height_thresh.awk.1) %>%
  rename(vapor_loss = awks.change.def.vapor_loss_rel.awk) %>%
  rename(vapor_loss_1 = awks.change.def.vapor_loss_rel.awk.1)
  

p300_patch_fire2 <- p300_patch_fire2 %>%
  rename(k1 = awks.change.def.biomass_loss_rel_k1.awk) %>%
  rename(k2 = awks.change.def.biomass_loss_rel_k2.awk) %>%
  rename(k1_1 = awks.change.def.biomass_loss_rel_k1.awk.1) %>%
  rename(k2_1 = awks.change.def.biomass_loss_rel_k2.awk.1) %>%
  rename(over_thresh = awks.change.def.overstory_height_thresh.awk) %>%
  rename(over_thresh_1 = awks.change.def.overstory_height_thresh.awk.1) %>%
  rename(pspread_loss = awks.change.def.pspread_loss_rel.awk) %>%
  rename(pspread_loss_1 = awks.change.def.pspread_loss_rel.awk.1) %>%
  rename(under_thresh = awks.change.def.understory_height_thresh.awk) %>%
  rename(under_thresh_1 = awks.change.def.understory_height_thresh.awk.1) %>%
  rename(vapor_loss = awks.change.def.vapor_loss_rel.awk) %>%
  rename(vapor_loss_1 = awks.change.def.vapor_loss_rel.awk.1)
  
  
#head(p300_patch_fire1)
#ls(p300_patch_fire1)
#unique(p300_patch_fire1$var)
#unique(p300_patch_fire2$var)


# Delete duplicate runs
p300_patch_fire1_sens <- p300_patch_fire1 %>%
  mutate(k1_eqv = k1 - k1_1) %>%
  mutate(k2_eqv = k2 - k2_1) %>%
  dplyr::filter(k1_eqv == 0 & k2_eqv == 0)

# Delete duplicate runs
p300_patch_fire2_sens <- p300_patch_fire2 %>%
  mutate(k1_eqv = k1 - k1_1) %>%
  mutate(k2_eqv = k2 - k2_1) %>%
  dplyr::filter(k1_eqv == 0 & k2_eqv == 0)


# Compute pre and post-effect differences
p300_patch_fire_change1 = p300_patch_fire1_sens %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = (`1941-10-11` - `1941-10-03`)/`1941-10-03`, absolute_change = `1941-10-11` - `1941-10-03`)

p300_patch_fire_change2 = p300_patch_fire2_sens %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = (`1941-10-11` - `1941-10-03`)/`1941-10-03`, absolute_change = `1941-10-11` - `1941-10-03`)

ls(p300_patch_fire_change1)

# ---------------------------------------------------------------------
# P300 Post-fire Change Evaluation

canopy <- c("1" = "Overstory","2" = "Understory")

tmp = dplyr::filter(p300_patch_fire_change1,var == "litrc")
x <- ggplot(data = tmp) +
  geom_point(aes(x=k2,y=relative_change, shape = as.factor(k1), color = as.factor(k1)), size=3) +
  scale_shape_discrete(name="K1") +
  scale_color_discrete(name="K1") +
  labs(title = "Litter", y = "Change (%)", x = "K2")
plot(x)
ggsave("p300_patch_sens_litrc.pdf",plot = x, path = PATCH_FIRE_DIR, width = 6, height = 6)


tmp = dplyr::filter(p300_patch_fire_change1,var == "soil1c")
x <- ggplot(data = tmp) +
  geom_point(aes(x=k2,y=relative_change, shape = as.factor(k1), color = as.factor(k1)), size=3) +
  scale_shape_discrete(name="K1") +
  scale_color_discrete(name="K1") +
  labs(title = "Soil Carbon", y = "Change (%)", x = "K2")
plot(x)
ggsave("p300_patch_sens_soil1c.pdf",plot = x, path = PATCH_FIRE_DIR, width = 6, height = 6)


tmp = dplyr::filter(p300_patch_fire_change1,var == "litrc")
x <- ggplot(data = tmp) +
  geom_point(aes(x=k2,y=relative_change, shape = as.factor(k1), color = as.factor(k1)), size=3) +
  scale_shape_discrete(name="K1") +
  scale_color_discrete(name="K1") +
  labs(title = "Coarse Woody Debris", y = "Change (%)", x = "K2")
plot(x)
#ggsave("p300_patch_sens_litrc.pdf",plot = x, path = PATCH_FIRE_DIR, width = 6, height = 6)


# -----

tmp = dplyr::filter(p300_patch_fire_change2,var == "stemc")
x <- ggplot(data = tmp) +
  geom_point(aes(x=k2,y=relative_change, shape = as.factor(k1), color = as.factor(k1)), size=3) +
  scale_shape_discrete(name="K1") +
  scale_color_discrete(name="K1") +
  facet_grid(.~names, labeller = labeller(names = canopy)) +
  labs(title = "Above Ground Carbon", y = "Change (%)", x = "K2")
plot(x)
ggsave("p300_patch_sens_stemc.pdf",plot = x, path = PATCH_FIRE_DIR, width = 6, height = 6)










