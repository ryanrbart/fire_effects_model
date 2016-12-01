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
PATCH_FIRE_DIR <- file.path(OUTPUT_DIR, "patch_fire")

# ---------------------------------------------------------------------
# Functions



# ---------------------------------------------------------------------
# P300 Patch Fire Output

# Vector of outputs variable names
var_names <- c("et", "lai")

# Parameter File
tmp = "patch_fire_parameter_sets.csv"
parameter_file <- file.path(INPUT_DIR, tmp)


p300_patch_fire = readin_rhessys_output2(var_names = var_names,
                               path = ALLSIM_DIR,
                               initial_date = ymd("1941-10-1"),
                               parameter_file)


ls(p300_patch_fire)

# ---------------------------------------------------------------------
# P300 Evaluation

x <- ggplot(data = p300_patch_fire) +
  geom_line(aes(x=dates,y=value, linetype=as.character(run))) +
  facet_grid(dated_seq_data~var)
plot(x)
ggsave(plot_name,plot = x, path = path)






