# Patch Fire Sensitivity Analysis
#
# Contains scripts for evaluating parameter sensitivity of fire effects model

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Patch simulation evaluation function

patch_sens_sobol_eval <- function(num_canopies,
                                  allsim_path,
                                  initial_date,
                                  parameter_file,
                                  stand_age_vect,
                                  watershed,
                                  stand_age,
                                  sobol_model_input,
                                  output_path,
                                  output_1st,
                                  output_total){
  
  # ---------------------------------------------------------------------
  # Patch Fire data processing
  # Computes differences in variables for several days before to several days after a fire.
  
  patch_fire <- readin_rhessys_output_cal(var_names = c("canopy_target_prop_mort_patched",
                                                        "canopy_target_prop_mort_consumed_patched",
                                                        "canopy_target_prop_c_consumed_patched",
                                                        "canopy_target_prop_c_remain_patched"),
                                          path = allsim_path,
                                          initial_date = initial_date,
                                          parameter_file = parameter_file,
                                          num_canopies = num_canopies)
  
  if (watershed == "P300"){
    patch_fire_diff <- patch_fire %>%
      dplyr::filter(dates == ('1941-10-11')) %>%
      spread(dates, value) %>%
      mutate(relative_change = as.double(`1941-10-11`)*100)
  }
  if (watershed == "SF"){
    patch_fire_diff <- patch_fire %>%
      dplyr::filter(dates == ('1955-10-11')) %>%
      spread(dates, value) %>%
      mutate(relative_change = as.double(`1955-10-11`)*100)
  }
  if (watershed == "HJA"){
    patch_fire_diff <- patch_fire %>%
      dplyr::filter(dates == ('1957-10-11')) %>%
      spread(dates, value) %>%
      mutate(relative_change = as.double(`1957-10-11`)*100)
  }
  if (watershed == "RS"){
    patch_fire_diff <- patch_fire %>%
      dplyr::filter(dates == ('1988-10-11')) %>%
      spread(dates, value) %>%
      mutate(relative_change = as.double(`1988-10-11`)*100)
  }
  rm(patch_fire)
  
  # Check data output
  # Note: monotonic relation non necessary for variance based measures (Song 2015)
  # For monotonic relation, plot relation between parameter and response variable
  # ggplot(data=patch_fire_diff) +
  #   geom_histogram(aes(x=relative_change)) +
  #   facet_grid(var_type~canopy_layer)
  
  
  # ---------------------------------------------------------------------
  # Sensitivity analysis
  
  # Import the parameter sets used in the sobol model.
  sobol_par <- read.csv(parameter_file)
  sobol_model <- readRDS(sobol_model_input)

  # ----
  # Sobol models of relative loss
  # Upper canopy for all locations
  sobol_upper_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_patched", canopy_layer==1)$relative_change)
  sobol_upper_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed_patched", canopy_layer==1)$relative_change)
  sobol_upper_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed_patched", canopy_layer==1)$relative_change)
  sobol_upper_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain_patched", canopy_layer==1)$relative_change)

  if (watershed != "RS"){
    # Lower canopy for HJA, P300 and SF
    sobol_lower_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_patched", canopy_layer==2)$relative_change)
    sobol_lower_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed_patched", canopy_layer==2)$relative_change)
    sobol_lower_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed_patched", canopy_layer==2)$relative_change)
    sobol_lower_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain_patched", canopy_layer==2)$relative_change)
  }
  
  # ----
  # Establish parameter and response variables (plus names) for figures  
  
  if (watershed != "RS"){
    # HJA, P300 and SF

    parameter_initial <- c("h_overstory", "h_understory", 
                           "k_mort_u", "k_consumption", "k1_mort_o", "k2_mort_o",
                           "k_mort_u", "k_consumption", "k1_mort_o", "k2_mort_o",
                           "I'")
    
    parameter <- c("h_overstory", "h_understory",
                   "k_mort_u", "k_consumption", "k1_mort_o", "k2_mort_o",
                   "I'")
    
    parameter_names <- c(expression('h'[overstory]), expression('h'[understory]), 
                         expression('k'[mort_u]), expression('k'[consumption]), 
                         expression('k'['1_mort_o']), expression('k'['2_mort_o']),   
                         "I'")
    
    response_variable_names <- c("prop_c_mort", "prop_mort_consumed",
                                 "prop_c_consumed", "prop_c_remain")
    
    parameter_canopy_group <- c("none", "none",
                                "low","low","low","low",
                                "up","up","up","up",
                                "none")
    
  } else {
    
    # RS
    parameter <- c("h_overstory", "h_understory", "k_mort_u", 
                   "k_consumption", "k1_mort_o",
                   "k2_mort_o", "I'")
    
    parameter_names <- c(expression('h'[overstory]), expression('h'[understory]),
                         expression('k'[mort_u]), expression('k'[consumption]), 
                         expression('k'['1_mort_o']), expression('k'['2_mort_o']), "I'")
    
    response_variable_names <- c("prop_c_mort", "prop_mort_consumed",
                                 "prop_c_consumed", "prop_c_remain")
  }
  
  # ----
  # Make a tibble for analyzing sensitivity
  
  
  if (watershed != "RS"){  
    # First-order indices
    sobol_fire_1st <- tibble(prop_c_mort_up = sobol_upper_canopy_mort_rel$S$original,
                             prop_mort_consumed_up = sobol_upper_canopy_mort_consumed_rel$S$original,
                             prop_c_consumed_up = sobol_upper_canopy_c_consumed_rel$S$original,
                             prop_c_remain_up = sobol_upper_canopy_c_remain_rel$S$original,
                             prop_c_mort_low = sobol_lower_canopy_mort_rel$S$original,
                             prop_mort_consumed_low = sobol_lower_canopy_mort_consumed_rel$S$original,
                             prop_c_consumed_low = sobol_lower_canopy_c_consumed_rel$S$original,
                             prop_c_remain_low = sobol_lower_canopy_c_remain_rel$S$original,
                             
                             prop_c_mort_up_se = sobol_upper_canopy_mort_rel$S$`std. error`,
                             prop_mort_consumed_up_se = sobol_upper_canopy_mort_consumed_rel$S$`std. error`,
                             prop_c_consumed_up_se = sobol_upper_canopy_c_consumed_rel$S$`std. error`,
                             prop_c_remain_up_se = sobol_upper_canopy_c_remain_rel$S$`std. error`,
                             prop_c_mort_low_se = sobol_lower_canopy_mort_rel$S$`std. error`,
                             prop_mort_consumed_low_se = sobol_lower_canopy_mort_consumed_rel$S$`std. error`,
                             prop_c_consumed_low_se = sobol_lower_canopy_c_consumed_rel$S$`std. error`,
                             prop_c_remain_low_se = sobol_lower_canopy_c_remain_rel$S$`std. error`,
                             
                             parameter=parameter_initial,
                             parameter_canopy_group=parameter_canopy_group)
    
    # Code to gather both sensitivity and se values
    a <- sobol_fire_1st %>% 
      tidyr::gather(response_variable, sensitivity_value, 1:8) %>% 
      dplyr::select(-(1:8))
    b <-sobol_fire_1st %>% 
      tidyr::gather(response_variable, se_value, 9:16) %>% 
      dplyr::select(-(1:8))
    b_mod <- dplyr::mutate(b, response_variable = stringr::str_sub(b$response_variable, start = 1L, end = -4L))
    sobol_fire_1st <- dplyr::full_join(a,b_mod, by=c("parameter", "parameter_canopy_group", "response_variable"))
    
    sobol_fire_1st <- sobol_fire_1st %>% 
      # Add canopy group variable
      mutate(response_canopy_group = if_else(response_variable %in% c("prop_c_mort_up",
                                                                      "prop_mort_consumed_up",
                                                                      "prop_c_consumed_up",
                                                                      "prop_c_remain_up"),
                                             "Upper Canopy",
                                             "Lower Canopy")) %>% 
      # Remove values where lower canopy parameter values are associated with
      # upper canopy response variables, and vice versa 
      dplyr::filter((parameter_canopy_group == "low" & response_canopy_group == "Lower Canopy") |
                      (parameter_canopy_group == "up" & response_canopy_group == "Upper Canopy") |
                      (parameter_canopy_group == "none")) %>% 
      # Change response variable names so they do not indicate upper or lower canopy
      mutate(response_variable = case_when(
        response_variable %in% c("prop_c_mort_up","prop_c_mort_low")~"prop_c_mort",
        response_variable %in% c("prop_mort_consumed_up","prop_mort_consumed_low")~"prop_mort_consumed",
        response_variable %in% c("prop_c_consumed_up","prop_c_consumed_low")~"prop_c_consumed",
        response_variable %in% c("prop_c_remain_up","prop_c_remain_low")~"prop_c_remain"
      ))
    sobol_fire_1st$response_canopy_group <- factor(sobol_fire_1st$response_canopy_group,
                                                   levels = c("Upper Canopy","Lower Canopy"))
    
    # ---
    
    # Total indices
    sobol_fire_total <- tibble(prop_c_mort_up = sobol_upper_canopy_mort_rel$T$original,
                               prop_mort_consumed_up = sobol_upper_canopy_mort_consumed_rel$T$original,
                               prop_c_consumed_up = sobol_upper_canopy_c_consumed_rel$T$original,
                               prop_c_remain_up = sobol_upper_canopy_c_remain_rel$T$original,
                               prop_c_mort_low = sobol_lower_canopy_mort_rel$T$original,
                               prop_mort_consumed_low = sobol_lower_canopy_mort_consumed_rel$T$original,
                               prop_c_consumed_low = sobol_lower_canopy_c_consumed_rel$T$original,
                               prop_c_remain_low = sobol_lower_canopy_c_remain_rel$T$original,
                               
                               prop_c_mort_up_se = sobol_upper_canopy_mort_rel$T$`std. error`,
                               prop_mort_consumed_up_se = sobol_upper_canopy_mort_consumed_rel$T$`std. error`,
                               prop_c_consumed_up_se = sobol_upper_canopy_c_consumed_rel$T$`std. error`,
                               prop_c_remain_up_se = sobol_upper_canopy_c_remain_rel$T$`std. error`,
                               prop_c_mort_low_se = sobol_lower_canopy_mort_rel$T$`std. error`,
                               prop_mort_consumed_low_se = sobol_lower_canopy_mort_consumed_rel$T$`std. error`,
                               prop_c_consumed_low_se = sobol_lower_canopy_c_consumed_rel$T$`std. error`,
                               prop_c_remain_low_se = sobol_lower_canopy_c_remain_rel$T$`std. error`,
                               
                               parameter=parameter_initial,
                               parameter_canopy_group=parameter_canopy_group)
    
    # Code to gather both sensitivity and se values
    a <- sobol_fire_total %>% 
      tidyr::gather(response_variable, sensitivity_value, 1:8) %>% 
      dplyr::select(-(1:8))
    b <-sobol_fire_total %>% 
      tidyr::gather(response_variable, se_value, 9:16) %>% 
      dplyr::select(-(1:8))
    b_mod <- dplyr::mutate(b, response_variable = stringr::str_sub(b$response_variable, start = 1L, end = -4L))
    sobol_fire_total <- dplyr::full_join(a,b_mod, by=c("parameter", "parameter_canopy_group", "response_variable"))
    
    sobol_fire_total <- sobol_fire_total %>% 
      # Add canopy group variable
      mutate(response_canopy_group = if_else(response_variable %in% c("prop_c_mort_up",
                                                                      "prop_mort_consumed_up",
                                                                      "prop_c_consumed_up",
                                                                      "prop_c_remain_up"),
                                             "Upper Canopy",
                                             "Lower Canopy")) %>% 
      # Remove values where lower canopy parameter values are associated with
      # upper canopy response variables, and vice versa 
      dplyr::filter((parameter_canopy_group == "low" & response_canopy_group == "Lower Canopy") |
                      (parameter_canopy_group == "up" & response_canopy_group == "Upper Canopy") |
                      (parameter_canopy_group == "none")) %>% 
      # Change response variable names so they do not indicate upper or lower canopy
      mutate(response_variable = case_when(
        response_variable %in% c("prop_c_mort_up","prop_c_mort_low")~"prop_c_mort",
        response_variable %in% c("prop_mort_consumed_up","prop_mort_consumed_low")~"prop_mort_consumed",
        response_variable %in% c("prop_c_consumed_up","prop_c_consumed_low")~"prop_c_consumed",
        response_variable %in% c("prop_c_remain_up","prop_c_remain_low")~"prop_c_remain"
      ))
    sobol_fire_total$response_canopy_group <- factor(sobol_fire_total$response_canopy_group,
                                                     levels = c("Upper Canopy","Lower Canopy"))
    
  } else {
    # RS
    
    # First-order indices
      sobol_fire_1st <- tibble(prop_c_mort = sobol_upper_canopy_mort_rel$S$original,
                               prop_mort_consumed = sobol_upper_canopy_mort_consumed_rel$S$original,
                               prop_c_consumed = sobol_upper_canopy_c_consumed_rel$S$original,
                               prop_c_remain = sobol_upper_canopy_c_remain_rel$S$original,
                               
                               prop_c_mort_se = sobol_upper_canopy_mort_rel$S$`std. error`,
                               prop_mort_consumed_se = sobol_upper_canopy_mort_consumed_rel$S$`std. error`,
                               prop_c_consumed_se = sobol_upper_canopy_c_consumed_rel$S$`std. error`,
                               prop_c_remain_se = sobol_upper_canopy_c_remain_rel$S$`std. error`,
                               
                               parameter=parameter)
      response_variable_limits_1st <- names(sobol_fire_1st[1:4])
      a <- sobol_fire_1st %>% 
        tidyr::gather(response_variable, sensitivity_value, 1:4) %>% 
        dplyr::select(-(1:4))
      b <- sobol_fire_1st %>% 
        tidyr::gather(response_variable, se_value, 5:8) %>% 
        dplyr::select(-(1:4))
      b_mod <- dplyr::mutate(b, response_variable = stringr::str_sub(b$response_variable, start = 1L, end = -4L))
      sobol_fire_1st <- dplyr::full_join(a,b_mod, by=c("parameter", "response_variable"))
      
      
      # Total indices
      sobol_fire_total <- tibble(prop_c_mort = sobol_upper_canopy_mort_rel$T$original,
                                 prop_mort_consumed = sobol_upper_canopy_mort_consumed_rel$T$original,
                                 prop_c_consumed = sobol_upper_canopy_c_consumed_rel$T$original,
                                 prop_c_remain = sobol_upper_canopy_c_remain_rel$T$original,
                                 
                                 prop_c_mort_se = sobol_upper_canopy_mort_rel$T$`std. error`,
                                 prop_mort_consumed_se = sobol_upper_canopy_mort_consumed_rel$T$`std. error`,
                                 prop_c_consumed_se = sobol_upper_canopy_c_consumed_rel$T$`std. error`,
                                 prop_c_remain_se = sobol_upper_canopy_c_remain_rel$T$`std. error`,
                                 
                                 parameter=parameter)
      response_variable_limits_total <- names(sobol_fire_total[1:4])
      a <- sobol_fire_total %>% 
        tidyr::gather(response_variable, sensitivity_value, 1:4) %>% 
        dplyr::select(-(1:4))
      b <- sobol_fire_total %>% 
        tidyr::gather(response_variable, se_value, 5:8) %>% 
        dplyr::select(-(1:4))
      b_mod <- dplyr::mutate(b, response_variable = stringr::str_sub(b$response_variable, start = 1L, end = -4L))
      sobol_fire_total <- dplyr::full_join(a,b_mod, by=c("parameter", "response_variable"))
  }
  
  # Output sobol final results
  write_csv(sobol_fire_1st, file.path(output_path, paste("2_sobol_1st_",stand_age,"_",watershed,".csv",sep="")))
  write_csv(sobol_fire_total, file.path(output_path, paste("2_sobol_total_",stand_age,"_",watershed,".csv",sep="")))
  
  # ----
  # Make a tibble for analyzing convergence
  # Quick and dirty version (prints results instead)
  print(paste("************** Watershed", watershed, "- Stand age", stand_age, "**************"))
  print("sobol_upper_canopy_mort_rel - Canopy 1")
  print(sobol_upper_canopy_mort_rel)
  print("sobol_upper_canopy_mort_consumed_rel - Canopy 1")
  print(sobol_upper_canopy_mort_consumed_rel)
  print("sobol_upper_canopy_c_consumed_rel - Canopy 1")
  print(sobol_upper_canopy_c_consumed_rel)
  print("sobol_upper_canopy_c_remain_rel - Canopy 1")
  print(sobol_upper_canopy_c_remain_rel)
  
  if (watershed != "RS"){  
    print("sobol_lower_canopy_mort_rel - Canopy 2")
    print(sobol_lower_canopy_mort_rel)
    print("sobol_lower_canopy_mort_consumed_rel - Canopy 2")
    print(sobol_lower_canopy_mort_consumed_rel)
    print("sobol_lower_canopy_c_consumed_rel - Canopy 2")
    print(sobol_lower_canopy_c_consumed_rel)
    print("sobol_lower_canopy_c_remain_rel - Canopy 2")
    print(sobol_lower_canopy_c_remain_rel)
  }
  
  # ---------------------------------------------------------------------
  # Figures 
  
  # ******** An secondary grouping was added to the response variable (y-axis).
  # See https://stackoverflow.com/questions/18165863/ for details. Possibly
  # replace upper and lower canopy terminology with primary and secondary
  # *********
  
  if (watershed != "RS"){
    # Figures for HJA, P300 and SF
    
    theme_set(theme_bw(base_size = 12))
    
    # First-order indices
    x <- ggplot(sobol_fire_1st) +
      geom_tile(aes(x=parameter, y=response_variable, fill=sensitivity_value)) +
      scale_fill_continuous(name="First-order\nIndices    ") +
      scale_x_discrete(labels = c(parameter_names),
                       limits=c(parameter),
                       expand=c(0,0)) +
      scale_y_discrete(labels = c(rev(response_variable_names)),
                       limits=c(rev(response_variable_names)),
                       expand=c(0,0)) +
      theme(axis.text.x = element_text(angle = 330, hjust=0)) +
      labs(title = paste("Sobol: ", watershed, " at stand age ", stand_age, sep=""),
           x = "Parameter", y = "Response Variable") +
      facet_grid(response_canopy_group~., switch = "y") +
      theme(panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(),
            strip.placement = "outside") + 
      # geom_hline(yintercept=4.5, color='red', size=2) + # Problem: line is also made at top of graph due to faceting
      NULL
    #plot(x)
    ggsave(paste("sobal_1st_", watershed, "_", stand_age, ".pdf",sep=""), plot = x,
           path = output_path, width = 8, height=6)
    
    # Total indices
    x <- ggplot(sobol_fire_total) +
      geom_tile(aes(x=parameter, y=response_variable, fill=sensitivity_value)) +
      scale_fill_continuous(name="Total\nIndices    ") +
      scale_x_discrete(labels = c(parameter_names),
                       limits=c(parameter),
                       expand=c(0,0)) +
      scale_y_discrete(labels = c(rev(response_variable_names)),
                       limits=c(rev(response_variable_names)),
                       expand=c(0,0)) +
      theme(axis.text.x = element_text(angle = 330, hjust=0)) +
      labs(title = paste("Sobol: ", watershed, " at stand age ", stand_age, sep=""),
           x = "Parameter", y = "Response Variable") +
      facet_grid(response_canopy_group~., switch = "y") +
      theme(panel.spacing = unit(0, "lines"), 
            strip.background = element_blank(),
            strip.placement = "outside") + 
      # geom_hline(yintercept=4.5, color='red', size=2) + # Problem: line is also made at top of graph due to faceting
      NULL    
    #plot(x)
    ggsave(paste("sobal_total_", watershed, "_", stand_age, ".pdf",sep=""), plot = x,
           path = output_path, width = 8, height=6)
    
  } else {
    # Figures for RS
    
    theme_set(theme_bw(base_size = 12))
    
    # First-order indices
    x <- ggplot(sobol_fire_1st) +
      geom_tile(aes(x=parameter, y=response_variable, fill=sensitivity_value)) +
      scale_fill_continuous(name="First-order\nIndices    ") +
      scale_x_discrete(labels = c(parameter_names),
                       limits=c(parameter)) +
      scale_y_discrete(labels = c(rev(response_variable_names)),
                       limits=c(rev(response_variable_limits_1st))) +
      theme(axis.text.x = element_text(angle = 330, hjust=0)) +
      labs(title = paste("Sobol: ", watershed, " at stand age ", stand_age, sep=""), x = "Parameter", y = "Response Variable")
    #plot(x)
    ggsave(paste("sobal_1st_", watershed, "_", stand_age, ".pdf",sep=""), plot = x,
           path = output_path, width = 8, height=6)
    
    # Total indices
    x <- ggplot(sobol_fire_total) +
      geom_tile(aes(x=parameter, y=response_variable, fill=sensitivity_value)) +
      scale_fill_continuous(name="Total\nIndices    ") +
      scale_x_discrete(labels = c(parameter_names),
                       limits=c(parameter)) +
      scale_y_discrete(labels = c(rev(response_variable_names)),
                       limits=c(rev(response_variable_limits_total))) +
      theme(axis.text.x = element_text(angle = 330, hjust=0)) +
      labs(title = paste("Sobol: ", watershed, " at stand age ", stand_age, sep=""), x = "Parameter", y = "Response Variable")
    #plot(x)
    ggsave(paste("sobal_total_", watershed, "_", stand_age, ".pdf",sep=""), plot = x,
           path = output_path, width = 8, height=6)
    
  }
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch sensitivity evaluation function


# HJA

allsim_2.5_hja <- c(RHESSYS_ALLSIM_DIR_2.5_HJA_STAND1,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND2,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND3,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND4,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND5,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND6,
                    RHESSYS_ALLSIM_DIR_2.5_HJA_STAND7)

stand_age_hja = c("5","12","20","40","70","100","140")

# Step through stands for HJA
for (aa in seq_along(allsim_2.5_hja)){
  
  patch_sens_sobol_eval(num_canopies = 2,
                        allsim_path = allsim_2.5_hja[aa],
                        initial_date = ymd("1957-10-01"),
                        parameter_file = RHESSYS_PAR_SOBOL_2.1_HJA,
                        stand_age_vect = c(1963,1970,1978,1998,2028,2058,2098),
                        watershed = "HJA",
                        stand_age = stand_age_hja[aa],
                        sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2007_2.1_HJA,
                        output_path = OUTPUT_DIR_2)
}



# ---------------------------------------------------------------------
# P300

allsim_2.5_p300 <- c(RHESSYS_ALLSIM_DIR_2.5_P300_STAND1,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND2,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND3,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND4,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND5,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND6,
                     RHESSYS_ALLSIM_DIR_2.5_P300_STAND7)

stand_age_p300 = c("5","12","20","30","40","60","80")

# Step through stands for P300
for (aa in seq_along(allsim_2.5_p300)){
  
  patch_sens_sobol_eval(num_canopies = 2,
                        allsim_path = allsim_2.5_p300[aa],
                        initial_date = ymd("1941-10-01"),
                        parameter_file = RHESSYS_PAR_SOBOL_2.1_P300,
                        stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                        watershed = "P300",
                        stand_age = stand_age_p300[aa],
                        sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2007_2.1_P300,
                        output_path = OUTPUT_DIR_2)
}



# ---------------------------------------------------------------------
# RS

allsim_2.5_rs <- c(RHESSYS_ALLSIM_DIR_2.5_RS_STAND1,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND2,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND3,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND4,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND5,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND6,
                   RHESSYS_ALLSIM_DIR_2.5_RS_STAND7)

stand_age_rs = c("5","12","20","30","40","60","80")

# Step through stands for RS
for (aa in seq_along(allsim_2.5_rs)){
  
  patch_sens_sobol_eval(num_canopies = 1,
                        allsim_path = allsim_2.5_rs[aa],
                        initial_date = ymd("1988-10-01"),
                        parameter_file = RHESSYS_PAR_SOBOL_2.1_RS,
                        stand_age_vect = c(1994,2001,2009,2019,2029,2049,2069),
                        watershed = "RS",
                        stand_age = stand_age_rs[aa],
                        sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2007_2.1_RS,
                        output_path = OUTPUT_DIR_2)
}



# ---------------------------------------------------------------------
# SF

allsim_2.5_sf <- c(RHESSYS_ALLSIM_DIR_2.5_SF_STAND1,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND2,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND3,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND4,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND5,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND6,
                   RHESSYS_ALLSIM_DIR_2.5_SF_STAND7)

stand_age_sf = c("5","12","20","30","40","60","80")

# Step through stands for SF
for (aa in seq_along(allsim_2.5_sf)){
  
  patch_sens_sobol_eval(num_canopies = 2,
                        allsim_path = allsim_2.5_sf[aa],
                        initial_date = ymd("1955-10-01"),
                        parameter_file = RHESSYS_PAR_SOBOL_2.1_SF,
                        stand_age_vect = c(1961,1968,1976,1986,1996,2016,2036),
                        watershed = "SF",
                        stand_age = stand_age_sf[aa],
                        sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2007_2.1_SF,
                        output_path = OUTPUT_DIR_2)
}


