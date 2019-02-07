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
                                  top_par_output,
                                  watershed,
                                  watershed_label,
                                  output_path){
  
  # Import parameter set file
  ps <- read_csv(parameter_file)
  
  # Set Stand Ages
  stand_age_vect <- stand_age_vect
  
  # ---------------------------------------------------------------------
  # Patch Simulation data processing
  
  patch_ground <- readin_rhessys_output_cal(var_names = c("litrc"),
                                            path = allsim_path,
                                            initial_date = ymd(initial_date),
                                            parameter_file = parameter_file,
                                            num_canopies = 1)
  patch_ground$value <- as.numeric(patch_ground$value)
  patch_ground$wy <- y_to_wy(lubridate::year(patch_ground$dates),lubridate::month(patch_ground$dates))
  patch_ground_sum <- patch_ground %>%
    group_by(wy, run, var_type) %>%
    summarize(avg_value = mean(value)*1000)     # Ground stores are originally in Kg/m2
  rm(patch_ground)
  
  
  patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                            path = allsim_path,
                                            initial_date = ymd(initial_date),
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
  patch_height$wy <- y_to_wy(lubridate::year(patch_height$dates),lubridate::month(patch_height$dates))
  patch_height_sum <- patch_height %>%
    group_by(wy, canopy_layer, run, var_type) %>%
    summarize(avg_value = mean(as.numeric(value)))
  rm(patch_height)
  
  # ---------------------------------------------------------------------
  # Identify parameter set for simulation with 1.3_patch_fire
  
  # Select the parameter set that most consistently produces the rank median
  # value of overstory height and litter across the stand ages.
  
  # Process upper canopy height ranks (Produces for each parameter set, the mean and sd of rank values across stand ages)
  rank_height <- patch_height_sum %>% 
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
    rank_height <- patch_height_sum %>% 
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
  rank_litter <- patch_ground_sum %>% 
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
  print(paste("this_one", this_one))
  
  # ----
  
  # Height plot
  height_plot <- ggplot() +
    geom_line(data=dplyr::filter(patch_height_sum, canopy_layer==1),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "navajowhite2") +
    geom_line(data=dplyr::filter(patch_height_sum, canopy_layer==2),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "navajowhite1") +
    #geom_line(data=dplyr::filter(patch_height_sum, run==this_one),
    #          aes(x=wy,y=avg_value, group=as.factor(canopy_layer)),size = 1.2) +
    geom_line(data=dplyr::filter(patch_height_sum, run==this_one),
              aes(x=wy,y=avg_value, color=as.factor(canopy_layer), 
                  linetype=as.factor(canopy_layer),
                  group=as.factor(canopy_layer)),size = 1.2) +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    #geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "olivedrab3") +
    labs(title = paste(watershed_label, ": Canopy Height", sep=""), x = "Wateryear", y = "Height (m)") +
    scale_color_manual(values = c("navajowhite4","navajowhite4"), name="Canopy", labels = c("Upper","Lower")) +
    scale_linetype(name="Canopy", labels = c("Upper","Lower")) +
    theme_bw(base_size = 18) +
    theme(legend.position = "right") +
    NULL
  plot(height_plot)  
  ggsave(paste("ts_height_",watershed,".pdf",sep=""),
         plot = height_plot,
         path = output_path,
         width = 6,
         height = 4)
  
  # Height - understory plot
  x <- ggplot() +
    geom_line(data=dplyr::filter(patch_height_sum, canopy_layer==2),
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "gray80") +
    geom_line(data=dplyr::filter(patch_height_sum, run==this_one, canopy_layer==2),
              aes(x=wy,y=avg_value, group=as.factor(canopy_layer)), color="black",size = 1.2) +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "olivedrab3") +
    labs(title = paste("Height - Understory - ", watershed, sep=""), x = "Wateryear", y = "Height (meters)") +
    theme(legend.position = "bottom") +
    NULL
  plot(x)  
  ggsave(paste("ts_height_understory_",watershed,".pdf",sep=""),
         plot = x,
         path = output_path,
         width = 6,
         height = 4)
  
  # ----
  
  # Litter  plot
  litter_plot <- ggplot() +
    geom_line(data=patch_ground_sum,
              aes(x=wy,y=avg_value, group=as.factor(run)), size = 1.2, color = "navajowhite2") +
    geom_line(data=dplyr::filter(patch_ground_sum, run==this_one),
              aes(x=wy,y=avg_value, group=1), color="navajowhite4",size = 1.2) +
    geom_vline(xintercept = stand_age_vect, linetype=2, size=.4) +
    labs(title = paste(watershed_label, ": Litter", sep=""), x = "Wateryear", y = "Carbon (g/m2)") +
    theme_bw(base_size = 18) +
    NULL
  plot(litter_plot)
  ggsave(paste("ts_litter_",watershed,".pdf",sep=""),
         plot = litter_plot,
         path = output_path,
         width = 6,
         height = 4)
  
  # ---
  beep()
  
  out_figures <- list(height_plot,litter_plot)
  return(out_figures)
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch simulation evaluation function


# HJA
out_hja <- patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_HJA,
                      initial_date = "1957-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_HJA,
                      stand_age_vect = c(1963,1970,1978,1998,2028,2058,2098),
                      top_par_output = OUTPUT_DIR_1_HJA_TOP_PS,
                      watershed = "HJA",
                      watershed_label = "H.J. Andrews",
                      output_path = OUTPUT_DIR_1)

# P300
out_p300 <- patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_P300,
                      initial_date = "1941-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      top_par_output = OUTPUT_DIR_1_P300_TOP_PS,
                      watershed = "P300",
                      watershed_label = "P301",
                      output_path = OUTPUT_DIR_1)

# RS
out_rs <- patch_simulation_eval(num_canopies = 1,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_RS,
                      initial_date = "1988-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_RS,
                      stand_age_vect = c(1994,2001,2009,2019,2029,2049,2069),
                      top_par_output = OUTPUT_DIR_1_RS_TOP_PS,
                      watershed = "RS",
                      watershed_label = "Rattlesnake",
                      output_path = OUTPUT_DIR_1)

# SF
out_sf <- patch_simulation_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_1.1_SF,
                      initial_date = "1955-10-01",
                      parameter_file = RHESSYS_PAR_FILE_1.1_SF,
                      stand_age_vect = c(1961,1968,1976,1986,1996,2016,2036),
                      top_par_output = OUTPUT_DIR_1_SF_TOP_PS,
                      watershed = "SF", 
                      watershed_label = "Santa Fe",
                      output_path = OUTPUT_DIR_1)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Cowplot tuning figures


# Combine figures for Cowplot
tuning_4by2 <- cowplot::plot_grid(out_rs[[1]],out_rs[[2]],
                                  out_p300[[1]],out_p300[[2]],
                                  out_sf[[1]],out_sf[[2]],
                                  out_hja[[1]],out_hja[[2]],
                             labels=c("a","b","c","d","e","f","g","h"),
                             nrow=4,
                             align="h",
                             label_size = 16)


#plot(tuning_4by2)
cowplot::save_plot(file.path(OUTPUT_DIR_1, paste("4by2_tuning.pdf",sep="")),
                   tuning_4by2,
                   ncol=2,
                   nrow=4,
                   base_aspect_ratio=2)

beep()

