# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))

# ---------------------------------------------------------------------
# Patch simulation evaluation function

patch_simulation_eval <- function(num_canopies,
                                  allsim_path,
                                  initial_date,
                                  parameter_file,
                                  stand_age_vect,
                                  top_par_output){
  
  # Import parameter set file
  ps <- read_csv(parameter_file)
  
  # Set Stand Ages
  stand_age_vect <- stand_age_vect
  
  # ---------------------------------------------------------------------
  # P300 Patch Simulation data processing
  
  p300_patch_ground <- readin_rhessys_output_cal(var_names = c("litrc"),
                                                 path = allsim_path,
                                                 initial_date = ymd(initial_date),
                                                 parameter_file = parameter_file,
                                                 num_canopies = 1)
  p300_patch_ground$value <- as.numeric(p300_patch_ground$value)
  p300_patch_ground$wy <- y_to_wy(lubridate::year(p300_patch_ground$dates),lubridate::month(p300_patch_ground$dates))
  p300_patch_ground_sum <- p300_patch_ground %>%
    group_by(wy, run, var_type) %>%
    summarize(avg_value = mean(value)*1000)     # Ground stores are originally in Kg/m2
  rm(p300_patch_ground)
  
  
  p300_patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                                 path = allsim_path,
                                                 initial_date = ymd(initial_date),
                                                 parameter_file = parameter_file,
                                                 num_canopies = num_canopies)
  p300_patch_height$wy <- y_to_wy(lubridate::year(p300_patch_height$dates),lubridate::month(p300_patch_height$dates))
  p300_patch_height_sum <- p300_patch_height %>%
    group_by(wy, canopy_layer, run, var_type) %>%
    summarize(avg_value = mean(as.numeric(value)))
  rm(p300_patch_height)
  
  #tail(p300_patch_ground)
  #tail(p300_patch_height)
  
  
  # ---------------------------------------------------------------------
  # Identify parameter set for simulation with 1.3_patch_fire
  
  # Select the parameter set that most consistently produces the rank median
  # value of overstory height and litter across the stand ages.
  
  # Process upper canopy height ranks (Produces for each parameter set, the mean and sd of rank values across stand ages)
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
  rank_h1 <- rank_height %>% 
    dplyr::mutate(h1_mean_rank = dense_rank(mean_rank_rank), h1_sd_rank = dense_rank(sd_rank)) %>% 
    select(run, h1_mean_rank, h1_sd_rank)
  
  # ----
  # Process lower canopy height ranks (Produces for each parameter set, the mean and sd of rank values across stand ages)
  if (num_canopies == 2){
    rank_height <- p300_patch_height_sum %>% 
      dplyr::filter(wy %in% stand_age_vect) %>% 
      dplyr::filter(canopy_layer == 2) %>% 
      dplyr::group_by(wy) %>% 
      dplyr::mutate(rank = dense_rank(avg_value)) %>% 
      dplyr::group_by(run) %>% 
      dplyr::summarise(mean_rank = mean(rank), sd_rank = sd(rank))
    
    # Relative to the other parameter sets, ranks which parameter set is closest to the median rank of each target.
    center_pos <- (length(rank_height$mean_rank)+1)/2
    rank_height$mean_rank_rank <- abs(rank_height$mean_rank-center_pos)
    rank_h2 <- rank_height %>% 
      dplyr::mutate(h2_mean_rank = dense_rank(mean_rank_rank), h2_sd_rank = dense_rank(sd_rank)) %>% 
      select(run, h2_mean_rank, h2_sd_rank)
  } else {
    rank_h2 <- rank_h1
    rank_h2 <- dplyr::rename(rank_h2, h2_mean_rank = h1_mean_rank, h2_sd_rank = h1_sd_rank)
    rank_h2$h2_mean_rank <- rep(1, length(rank_h2$h2_mean_rank))
    rank_h2$h2_sd_rank <- rep(1, length(rank_h2$h2_sd_rank))
  }
  
  # ----
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
  
  # ---------------------
  # Bind ranks for all targets and produce weighted 'total rank' 
  rank_final <- rank_h1 %>% 
    dplyr::left_join(rank_h2, by = "run") %>% 
    dplyr::left_join(rank_l, by = "run")
  rank_final$total <- rank_final$h1_mean_rank + 
    rank_final$h2_mean_rank +
    rank_final$l_mean_rank
  rank_final <- mutate(rank_final, total_rank = dense_rank(total))
  print(rank_final)
  
  # Unconcatenate 'run' variable and order the results
  run_split <- unlist(strsplit(rank_final$run, "X"))
  run_number <- as.numeric(run_split[seq(2, length(run_split), 2)])
  rank_final$run_number <- run_number
  rank_final2 <- arrange(rank_final, run_number)
  
  # Determine row of selected parameter set
  ps_row <- which(rank_final2$total_rank == 1)
  ps_row <- ps_row[1]   # Optional: If tie, select the first ps
  
  # Select best parameter set
  ps_selected_1 <- ps[ps_row,]
  
  #Write output
  write.csv(ps_selected_1, top_par_output, row.names = FALSE, quote=FALSE)
  
  
  # ---------------------------------------------------------------------
  # Figures: Time-series for Height and Litter
  
  this_one <- rank_final2[ps_row,]$run
  
  # ----
 
  # Height plot
  x <- ggplot() +
    geom_line(data=dplyr::filter(p300_patch_height_sum, canopy_layer==1),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "gray70") +
    geom_line(data=dplyr::filter(p300_patch_height_sum, canopy_layer==2),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "gray80") +
    geom_line(data=dplyr::filter(p300_patch_height_sum, run==this_one),
              aes(x=wy,y=avg_value, linetype=as.factor(canopy_layer), group=as.factor(canopy_layer)), color="black",size = 1.2) +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "olivedrab3") +
    labs(title = "Height", x = "Wateryear", y = "Height (meters)") +
    scale_color_brewer(palette = "Set2", name="Canopy", labels = c("Upper Canopy","Lower Canopy")) +
    theme(legend.position = "bottom")
  plot(x)  
  #ggsave("ts_p300_height.pdf",plot = x, path = OUTPUT_DIR_1)

  # Height - understory plot
  x <- ggplot() +
    geom_line(data=dplyr::filter(p300_patch_height_sum, canopy_layer==2),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "gray80") +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "olivedrab3") +
    labs(title = "Height - Understory", x = "Wateryear", y = "Height (meters)")
  plot(x)  
  #ggsave("ts_p300_height.pdf",plot = x, path = OUTPUT_DIR_1)
  
  if (num_canopies == 2){
    # Shrub maximum height (by run)
    max_height = p300_patch_height_sum %>%
      dplyr::filter(canopy_layer == 2) %>%
      group_by(run) %>%
      summarize(height = max(avg_value))
    plot(max_height$height)
  }
  # Tree maximum height (by run)
  max_height = p300_patch_height_sum %>%
    dplyr::filter(canopy_layer == 1) %>%
    group_by(run) %>%
    summarize(height = max(avg_value))
  plot(max_height$height)
  
  # ----
  
  # Litter  plot
  x <- ggplot() +
    geom_line(data=p300_patch_ground_sum,
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "gray80") +
    geom_line(data=dplyr::filter(p300_patch_ground_sum, run==this_one),
              aes(x=wy,y=avg_value, group=1), color="black",size = 1.2) +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    labs(title = "Litter Store", x = "Wateryear", y = "Carbon (g/m2)")
  plot(x)
  # ggsave("ts_p300_ground.pdf",plot = x, path = OUTPUT_DIR_1)
  
  # ---
  # Extra Data checks
  
  # Maximum litter variability
  max_litter = p300_patch_ground_sum %>%
    group_by(run) %>%
    summarize(litter = max(avg_value))
  plot(max_litter$litter)

  # ---
  beep()
}

# ---------------------------------------------------------------------
# Call patch simulation evaluation function


# RS
patch_simulation_eval(num_canopies = 1,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_RS,
                      initial_date = "1988-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_RS,
                      stand_age_vect = c(1994,2001,2009,2019,2029,2049,2069),
                      top_par_output = OUTPUT_DIR_1_RS_TOP_PS)

# P300
patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_P300,
                      initial_date = "1941-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      top_par_output = OUTPUT_DIR_1_P300_TOP_PS)

# HJA
patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_HJA,
                      initial_date = "1957-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_HJA,
                      stand_age_vect = c(1967,1977,1997,2027,2057,2097,2147),
                      top_par_output = OUTPUT_DIR_1_HJA_TOP_PS)

# SF
patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_SF,
                      initial_date = "",
                      parameter_file = RHESSYS_PAR_FILE_1.1_SF,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      top_par_output = OUTPUT_DIR_1_SF_TOP_PS)


