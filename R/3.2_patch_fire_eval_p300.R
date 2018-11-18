# Patch Fire Evaluation
#
# Contains scripts for evaluating patch-level fire effects

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))
#theme_set(theme_bw(base_size = 16))


# ---------------------------------------------------------------------
# P300 Patch Fire data processing

# Gather dated sequence values for binding to all_options table 
dated_data <- data.frame(dated_id = seq_along(input_dated_seq_list),
                         dated_seq_values = unlist(sapply(input_dated_seq_list, function(x) x[7])))

p300_patch_canopy <- readin_rhessys_output_cal(var_names = c("leafc", "stemc"),
                                              path = RHESSYS_ALLSIM_DIR_3.1_P300,
                                              initial_date = ymd("1941-10-01"),
                                              parameter_file = RHESSYS_ALL_OPTION_3.1_P300,
                                              num_canopies = 2)
p300_patch_canopy_diff <- p300_patch_canopy %>%
  full_join(dated_data, by="dated_id") %>% 
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
         absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_canopy)


p300_patch_ground <- readin_rhessys_output_cal(var_names = c("litrc", "soil1c"),
                                              path = RHESSYS_ALLSIM_DIR_3.1_P300,
                                              initial_date = ymd("1941-10-01"),
                                              parameter_file = RHESSYS_ALL_OPTION_3.1_P300,
                                              num_canopies = 1)
p300_patch_ground_diff <- p300_patch_ground %>%
  full_join(dated_data, by="dated_id") %>% 
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
         absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_ground)


p300_patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                            path = RHESSYS_ALLSIM_DIR_3.1_P300,
                                            initial_date = ymd("1941-10-01"),
                                            parameter_file = RHESSYS_ALL_OPTION_3.1_P300,
                                            num_canopies = 2)
p300_patch_cwdc_diff <- p300_patch_cwdc %>%
  full_join(dated_data, by="dated_id") %>% 
  group_by(run, dates, var_type, world_file, dated_seq_values) %>%
  summarize(value = sum(value)) %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
         absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_cwdc)


p300_patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                              path = RHESSYS_ALLSIM_DIR_3.1_P300,
                                              initial_date = ymd("1941-10-01"),
                                              parameter_file = RHESSYS_ALL_OPTION_3.1_P300,
                                              num_canopies = 2)
p300_patch_height_diff <- p300_patch_height %>%
  full_join(dated_data, by="dated_id") %>% 
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
         absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_height)

p300_patch_fire <- readin_rhessys_output_cal(var_names = c("canopy_target_prop_mort",
                                                           "canopy_target_prop_mort_consumed",
                                                           "canopy_target_prop_c_consumed",
                                                           "canopy_target_prop_c_remain"),
                                             path = RHESSYS_ALLSIM_DIR_3.1_P300,
                                             initial_date = ymd("1941-10-01"),
                                             parameter_file = RHESSYS_ALL_OPTION_3.1_P300,
                                             num_canopies = 2)
p300_patch_fire_diff <- p300_patch_fire %>%
  full_join(dated_data, by="dated_id") %>% 
  dplyr::filter(dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = `1941-10-11`*100)
rm(p300_patch_fire)


# ---
world_file_yr <- c(
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state" = "Stand\nage:\n5 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state" = "Stand\nage:\n12 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state" = "Stand\nage:\n20 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state" = "Stand\nage:\n30 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state" = "Stand\nage:\n40 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state" = "Stand\nage:\n60 yrs",
  "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state" = "Stand\nage:\n80 yrs"
)

canopy <- c("1" = "Upper Canopy","2" = "Lower Canopy")


# ---------------------------------------------------------------------
# Figures: 

tmp <- dplyr::filter(p300_patch_ground_diff, var_type == "litrc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr)) +
  theme(legend.position = "none") +
  labs(title = "Litter Carbon", x = "Intensity", y = "Change in Litter Carbon (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_litrc.pdf",plot = x, path = OUTPUT_DIR_2)

tmp <- dplyr::filter(p300_patch_ground_diff, var_type == "soil1c")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr)) +
  theme(legend.position = "none") +
  labs(title = "Soil Carbon", x = "Intensity", y = "Change in Soil Carbon (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_soil1c.pdf",plot = x, path = OUTPUT_DIR_2)

tmp <- dplyr::filter(p300_patch_cwdc_diff, var_type == "cwdc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(.~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Coarse Woody Debris Carbon", x = "Intensity", y = "Change in CWD Carbon (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_cwdc.pdf",plot = x, path = OUTPUT_DIR_2)


# ----

tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "leafc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity", aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Leaf Carbon", x = "Intensity", y = "Change in Leaf Carbon (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_leafc.pdf",plot = x, path = OUTPUT_DIR_2)


tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "stemc")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Stem Carbon", x = "Intensity", y = "Change in Stem Carbon (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_stemc.pdf",plot = x, path = OUTPUT_DIR_2)


tmp <- dplyr::filter(p300_patch_height_diff,var_type == "height")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Height", x = "Intensity", y = "Change in Height (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_change_height.pdf",plot = x, path = OUTPUT_DIR_2)

# ---
tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_mort")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Mortality", x = "Intensity", y = "Mortality (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_mortality.pdf",plot = x, path = OUTPUT_DIR_2)

tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_mort_consumed")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Consumption", x = "Intensity", y = "Proportion of Mortality Consumed (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_mortality_consumed.pdf",plot = x, path = OUTPUT_DIR_2)

tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_c_consumed")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Canopy Consumption", x = "Intensity", y = "Canopy Carbon Consumed (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_c_consumed.pdf",plot = x, path = OUTPUT_DIR_2)

tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_c_remain")
x <- ggplot(data = tmp) +
  geom_bar(stat="identity",aes(x=dated_seq_values,y=relative_change), color="olivedrab3") +
  facet_grid(canopy_layer~world_file, labeller = labeller(world_file = world_file_yr, canopy_layer = canopy)) +
  theme(legend.position = "none") +
  labs(title = "Canopy Remaining as Litter", x = "Intensity", y = "Canopy Carbon Remaining as Litter (%)") +
  scale_x_continuous(breaks = c(0.2, 0.8))
plot(x)
#ggsave("p300_patch_sim_c_remain.pdf",plot = x, path = OUTPUT_DIR_2)




