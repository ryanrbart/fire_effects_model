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
var_names_1can <- c("litter", "soil1c")
var_names_2can <- c("lai", "agc", "leafc", "live_stemc","dead_stemc", "cwdc")

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

# ---------------------------------------------------------------------
# P300 Evaluation


hh <- select(p300_patch_fire2, names,dates, value, var, run, dated_seq_data)

gg = dplyr::filter(p300_patch_fire2,dated_seq_data > 0.6)

gg = dplyr::filter(p300_patch_fire2, run == c("V1","V2"))


x <- ggplot(data = p300_patch_fire2) +
  geom_line(aes(x=dates,y=value, linetype=as.character(names))) +
  facet_grid(dated_seq_data~var) +
  theme(legend.position = "none")
plot(x)
ggsave("p300_patch_sim_height.pdf",plot = x, path = PATCH_FIRE_DIR)

x <- ggplot(data = gg) +
  geom_line(aes(x=dates,y=value, linetype=as.character(world_file))) +
  facet_grid(dated_seq_data~var)
plot(x)
#ggsave(plot_name,plot = x, path = path)





