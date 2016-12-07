# Patch Fire Evaluation
#
# Contains scripts for evaluating patch-level fire effects

library(rhessysR)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)


# ---------------------------------------------------------------------
# Input and output paths 

INPUT_DIR <- "ws_p300/out/p300_patch_fire"
ALLSIM_DIR <- file.path(INPUT_DIR, "allsim")

OUTPUT_DIR <- "outputs"
PATCH_FIRE_DIR <- file.path(OUTPUT_DIR, "4_patch_fire")

# ---------------------------------------------------------------------
# Functions



# ---------------------------------------------------------------------
# P300 Patch Fire Output

# Vector of outputs variable names
var_names_1can <- c("litrc", "soil1c")
var_names_2can <- c("lai", "height", "agc", "leafc", "live_stemc", "dead_stemc", "cwdc")

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


# ---------------------------------------------------------------------
# P300 Evaluation

hh = select(p300_patch_fire2, names,dates, value, var, run, dated_seq_data, world_file)

tmp = dplyr::filter(p300_patch_fire1,var == "litrc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_litrc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire1,var == "soil1c")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_soil1c.pdf",plot = x, path = PATCH_FIRE_DIR)


# ---

tmp = dplyr::filter(p300_patch_fire2,var == "agc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_agc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "lai")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_lai.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "height")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_height.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "leafc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_leafc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "live_stemc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_live_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "dead_stemc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_dead_stemc.pdf",plot = x, path = PATCH_FIRE_DIR)

tmp = dplyr::filter(p300_patch_fire2,var == "cwdc")
x <- ggplot(data = tmp) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~world_file) +
  theme(legend.position = "none")
plot(x)
#ggsave("p300_patch_sim_cwdc.pdf",plot = x, path = PATCH_FIRE_DIR)






