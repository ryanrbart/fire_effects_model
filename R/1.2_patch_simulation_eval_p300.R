# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))

# ---------------------------------------------------------------------
# P300 Patch Simulation data processing


p300_patch_canopy <- readin_rhessys_output2(var_names = c("leafc", "stemc", "rootc"),
                                           path = ALLSIM_DIR_p300_1.1,
                                           initial_date = ymd("1941-10-01"),
                                           parameter_file = PARAMETER_FILE_p300_1.1,
                                           num_canopies = 2)
p300_patch_canopy$wy <- y_to_wy(lubridate::year(p300_patch_canopy$dates),lubridate::month(p300_patch_canopy$dates))
p300_patch_canopy_sum <- p300_patch_canopy %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))

p300_patch_ground <- readin_rhessys_output2(var_names = c("litrc", "soil1c"),
                                           path = ALLSIM_DIR_p300_1.1,
                                           initial_date = ymd("1941-10-01"),
                                           parameter_file = PARAMETER_FILE_p300_1.1,
                                           num_canopies = 1)
p300_patch_ground$wy <- y_to_wy(lubridate::year(p300_patch_ground$dates),lubridate::month(p300_patch_ground$dates))
p300_patch_ground_sum <- p300_patch_ground %>%
  group_by(wy, run, var_type) %>%
  summarize(avg_value = mean(value)*1000)     # Ground stores are originally in Kg/m2

p300_patch_cwdc <- readin_rhessys_output2(var_names = c("cwdc"),
                                           path = ALLSIM_DIR_p300_1.1,
                                           initial_date = ymd("1941-10-01"),
                                           parameter_file = PARAMETER_FILE_p300_1.1,
                                           num_canopies = 2)
p300_patch_cwdc$wy <- y_to_wy(lubridate::year(p300_patch_cwdc$dates),lubridate::month(p300_patch_cwdc$dates))
p300_patch_cwdc_sum <- p300_patch_cwdc %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))

p300_patch_height <- readin_rhessys_output2(var_names = c("height"),
                                            path = ALLSIM_DIR_p300_1.1,
                                            initial_date = ymd("1941-10-01"),
                                            parameter_file = PARAMETER_FILE_p300_1.1,
                                            num_canopies = 2)
p300_patch_height$wy <- y_to_wy(lubridate::year(p300_patch_height$dates),lubridate::month(p300_patch_height$dates))
p300_patch_height_sum <- p300_patch_height %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))

#tail(p300_patch_canopy)
#tail(p300_patch_ground)
#tail(p300_patch_cwdc)
#tail(p300_patch_height)


# ---------------------------------------------------------------------
# Time-series graphics for Upper Canopy, Lower Canopy, Ground Stores and Height

# Canopy 1 comparison plot
x <- p300_patch_canopy_sum %>%
  dplyr::filter(canopy_layer == 1) %>%
  ggplot() +
  geom_line(aes(x = wy, y = avg_value, color=var_type), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "Upper Canopy", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_upper_canopy.pdf",plot = x, path = OUTPUT_R_DIR_1)


# ----

# Canopy 2 comparison plot
x <- p300_patch_canopy_sum %>%
  dplyr::filter(canopy_layer == 2) %>%
  ggplot() +
  geom_line(aes(x = wy, y = avg_value, color=var_type), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "Lower Canopy", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_lower_canopy.pdf",plot = x, path = OUTPUT_R_DIR_1)


# ----

# Height comparison plot
x <- p300_patch_height_sum %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_value, color=as.character(canopy_layer)), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  geom_hline(yintercept= c(4,7), linetype=1, size=.4, color = "olivedrab3") +
  #    xlim(1941,1950) +
  #    ylim(0,1) +
  labs(title = "Height", x = "Wateryear", y = "Height (meters)") +
  scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Upper Canopy","Lower Canopy")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_height.pdf",plot = x, path = OUTPUT_R_DIR_1)

# Shrub maximum height
max_height = p300_patch_height_sum %>%
  dplyr::filter(canopy_layer == 2) %>% 
  group_by(run) %>%
  summarize(height = max(avg_value))
#plot(max_height$height)

# ----

# Litter, soil, cwdc comparison plot (Uses single soil store)
x <- p300_patch_cwdc_sum %>%
  group_by(wy, run, var_type) %>%
  summarize(avg_value = sum(avg_value)) %>%     # Collapse cwdc from multiple canopies to a single patch total
  dplyr::bind_rows(p300_patch_ground_sum) %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_value, color=as.character(var_type)), size = 1.2) +
  geom_vline(xintercept= c(1947,1954,1962,1972,1982,2002,2022), linetype=2, size=.4) +
  labs(title = "Ground Stores", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c(cwdc = "Coarse Woody Debris", litrc  = "Litter", soil1c = "Soil")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_ground.pdf",plot = x, path = OUTPUT_R_DIR_1)

# ---
# Extra Data checks

# Check Lower Canopy maximum height
max_height = p300_patch_height_sum %>%
  dplyr::filter(canopy_layer == 2) %>% 
  group_by(run) %>%
  summarize(height = max(avg_value))
#plot(max_height$height)



# ---------------------------------------------------------------------
# Identify parameter set for simulation with 2.1_patch_fire

ps <- read.csv(PARAMETER_FILE_p300_1.1, header = TRUE)







# ---
beep()




