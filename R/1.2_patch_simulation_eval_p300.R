# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))

# ---------------------------------------------------------------------
# P300 Patch Simulation data processing


p300_patch_canopy <- readin_rhessys_output_cal(var_names = c("leafc", "stemc", "rootc"),
                                               path = ALLSIM_DIR_p300_1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_1.1,
                                               num_canopies = 2)
p300_patch_canopy$wy <- y_to_wy(lubridate::year(p300_patch_canopy$dates),lubridate::month(p300_patch_canopy$dates))
p300_patch_canopy_sum <- p300_patch_canopy %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))
rm(p300_patch_canopy)

p300_patch_ground <- readin_rhessys_output_cal(var_names = c("litrc", "soil1c"),
                                               path = ALLSIM_DIR_p300_1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_1.1,
                                               num_canopies = 1)
p300_patch_ground$wy <- y_to_wy(lubridate::year(p300_patch_ground$dates),lubridate::month(p300_patch_ground$dates))
p300_patch_ground_sum <- p300_patch_ground %>%
  group_by(wy, run, var_type) %>%
  summarize(avg_value = mean(value)*1000)     # Ground stores are originally in Kg/m2
rm(p300_patch_ground)

p300_patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                             path = ALLSIM_DIR_p300_1.1,
                                             initial_date = ymd("1941-10-01"),
                                             parameter_file = PARAMETER_FILE_p300_1.1,
                                             num_canopies = 2)
p300_patch_cwdc$wy <- y_to_wy(lubridate::year(p300_patch_cwdc$dates),lubridate::month(p300_patch_cwdc$dates))
p300_patch_cwdc_sum <- p300_patch_cwdc %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))
rm(p300_patch_cwdc)

p300_patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                               path = ALLSIM_DIR_p300_1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_1.1,
                                               num_canopies = 2)
p300_patch_height$wy <- y_to_wy(lubridate::year(p300_patch_height$dates),lubridate::month(p300_patch_height$dates))
p300_patch_height_sum <- p300_patch_height %>%
  group_by(wy, canopy_layer, run, var_type) %>%
  summarize(avg_value = mean(value))
rm(p300_patch_height)

#tail(p300_patch_canopy)
#tail(p300_patch_ground)
#tail(p300_patch_cwdc)
#tail(p300_patch_height)


# ---------------------------------------------------------------------
# Figures: Time-series for Upper Canopy, Lower Canopy, Ground Stores and Height

stand_age_vect <- c(1947,1954,1962,1972,1982,2002,2022)

# Canopy 1 comparison plot
x <- p300_patch_canopy_sum %>%
  dplyr::filter(canopy_layer == 1) %>%
  ggplot() +
  geom_line(aes(x = wy, y = avg_value, color=var_type), size = 1.2) +
  geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
  labs(title = "Upper Canopy", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_upper_canopy.pdf",plot = x, path = OUTPUT_DIR_1)


# ----

# Canopy 2 comparison plot
x <- p300_patch_canopy_sum %>%
  dplyr::filter(canopy_layer == 2) %>%
  ggplot() +
  geom_line(aes(x = wy, y = avg_value, color=var_type), size = 1.2) +
  geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
  labs(title = "Lower Canopy", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c("Leaf","Root","Stem")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_lower_canopy.pdf",plot = x, path = OUTPUT_DIR_1)


# ----

# Height comparison plot
x <- p300_patch_height_sum %>%
  ggplot() +
  geom_line(aes(x=wy,y=avg_value, color=as.character(canopy_layer)), size = 1.2) +
  geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
  geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "olivedrab3") +
  #    xlim(1941,1950) +
  #    ylim(0,1) +
  labs(title = "Height", x = "Wateryear", y = "Height (meters)") +
  scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Upper Canopy","Lower Canopy")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_height.pdf",plot = x, path = OUTPUT_DIR_1)

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
  geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
  labs(title = "GroundStores", x = "Wateryear", y = "Carbon (g/m2)") +
  scale_color_brewer(palette = "Set2", name="Store Type", labels = c(cwdc = "Coarse Woody Debris", litrc  = "Litter", soil1c = "Soil")) +
  theme(legend.position = "bottom") +
  facet_wrap(~run)
plot(x)
ggsave("ts_p300_ground.pdf",plot = x, path = OUTPUT_DIR_1)

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
ps <- read_table2(PARAMETER_FILE_p300_1.1)

# Select the parameter set that most consistently produces the rank median
# value of overstory height and litter across the stand ages. The selection 
# also emphasizes lower levels of rank median variance between the stand 
# ages, but only half as much as the actual values.

# Process height ranks (Produces for each parameter set, the mean and sd of rank values across stand ages)
rank_height <- p300_patch_height_sum %>% 
  dplyr::filter(wy %in% stand_age_vect) %>% 
  dplyr::filter(canopy_layer == 1) %>% 
  dplyr::group_by(wy) %>% 
  dplyr::mutate(rank = dense_rank(avg_value)) %>% 
  dplyr::group_by(run) %>% 
  dplyr::summarise(mean_rank = mean(rank), sd_rank = sd(rank))

# Relative to the other parameter sets, ranks which parameter set is closest to the median rank of each target.
center_pos <- (length(rank_height$mean_rank)+1)/2
rank_height$mean_rank_rank <- abs(rank_height$mean_rank-center_pos)
rank_h <- rank_height %>% 
  dplyr::mutate(h_mean_rank = dense_rank(mean_rank_rank), h_sd_rank = dense_rank(sd_rank)) %>% 
  select(run, h_mean_rank, h_sd_rank)

# Process litter ranks  (Produces for each parameter set, the mean and sd of rank values across stand ages)
rank_litter <- p300_patch_ground_sum %>% 
  dplyr::filter(var_type %in% "litrc") %>% 
  dplyr::filter(wy %in% stand_age_vect) %>% 
  dplyr::group_by(wy) %>% 
  dplyr::mutate(rank = dense_rank(avg_value)) %>% 
  dplyr::group_by(run) %>% 
  dplyr::summarise(mean_rank = mean(rank), sd_rank = sd(rank))

# Relative to the other parameter sets, ranks which parameter set is closest to the median rank of each target.
center_pos <- (length(rank_litter$mean_rank)+1)/2
rank_litter$mean_rank_rank <- abs(rank_litter$mean_rank-center_pos)
rank_l <- rank_litter %>% 
  dplyr::mutate(l_mean_rank = dense_rank(mean_rank_rank), l_sd_rank = dense_rank(sd_rank)) %>% 
  select(run, l_mean_rank, l_sd_rank)

# Bind ranks for all targets and produce weighted 'total rank' 
rank_final <- rank_h %>% 
  dplyr::left_join(rank_l, by = "run")
rank_final$total <- rank_final$h_mean_rank + 0.5*rank_final$h_sd_rank + rank_final$l_mean_rank + 0.5*rank_final$l_sd_rank
rank_final <- mutate(rank_final, total_rank = dense_rank(total))
print(rank_final)

# Unconcatenate 'run' variable and order the results
run_split <- unlist(strsplit(rank_final$run, "V"))
run_number <- as.numeric(run_split[seq(2, length(run_split), 2)])
rank_final$run_number <- run_number
rank_final2 <- arrange(rank_final, run_number)

# Determine row of selected parameter set
ps_row <- which(rank_final2$total_rank == 1)
ps_row <- ps_row[1]   # Optional: If tie, select the first ps

# Select best parameter set
ps_selected_1 <- ps[ps_row,]

#Write output
write.table(ps_selected_1, file.path(OUTPUT_DIR_1, "1_selected_ps.txt"), row.names = FALSE, quote=FALSE)





# ---
beep()




