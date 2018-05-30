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
                                  fig_title,
                                  watershed,
                                  stand_age,
                                  sobol_model_input,
                                  output_path){
  
  # ---------------------------------------------------------------------
  # Patch Fire data processing
  # Computes differences in variables for several days before to several days after a fire.
  
  patch_fire <- readin_rhessys_output_cal(var_names = c("canopy_target_prop_mort",
                                                        "canopy_target_prop_mort_consumed",
                                                        "canopy_target_prop_c_consumed",
                                                        "canopy_target_prop_c_remain"),
                                          path = allsim_path,
                                          initial_date = initial_date,
                                          parameter_file = parameter_file,
                                          num_canopies = num_canopies)
  patch_fire_diff <- patch_fire %>%
    dplyr::filter(dates == ymd("1941-10-11")) %>%
    spread(dates, value) %>%
    mutate(relative_change = as.double(`1941-10-11`)*100)
  rm(patch_fire)
  
  
  # ---------------------------------------------------------------------
  # Sensitivity analysis
  
  # Import the parameter sets used in the sobol model.
  sobol_par <- read.csv(parameter_file)
  sobol_model <- readRDS(sobol_model_input)

  # ----
  # Sobol models of relative loss
  sobol_upper_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort", canopy_layer==1)$relative_change)
  sobol_lower_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort", canopy_layer==2)$relative_change)
  sobol_upper_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed", canopy_layer==1)$relative_change)
  sobol_lower_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed", canopy_layer==2)$relative_change)
  sobol_upper_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed", canopy_layer==1)$relative_change)
  sobol_lower_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed", canopy_layer==2)$relative_change)
  sobol_upper_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain", canopy_layer==1)$relative_change)
  sobol_lower_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain", canopy_layer==2)$relative_change)
  
  # ----
  # Establish parameter and response variables (plus names) for figures  
  
  if (watershed != "RS"){
    # HJA, P300 and SF
    parameter <- c("h_overstory", "h_understory", "LowerCan: k_mort_u",
                    "LowerCan: k_consumption", "LowerCan: k1_mort_o", "LowerCan: k2_mort_o",
                    "UpperCan: k_mort_u", "UpperCan: k_consumption", "UpperCan: k1_mort_o",
                    "UpperCan: k2_mort_o", "I'")
    
    parameter_names <- c(expression('h'[overstory]), expression('h'[understory]), 
                         expression('LowerCan: k'[mort_u]), expression('LowerCan: k'[consumption]), 
                         expression('LowerCan: k'['1_mort_o']), expression('LowerCan: k'['2_mort_o']),   
                         expression('UpperCan: k'[mort_u]), expression('UpperCan: k'[consumption]), 
                         expression('UpperCan: k'['1_mort_o']), expression('UpperCan: k'['2_mort_o']), "I'")
    
    response_variable_names <- c("UpperCan: prop_c_mort", "UpperCan: prop_mort_consumed",
                                      "UpperCan: prop_c_consumed", "UpperCan: prop_c_remain",
                                      "LowerCan: prop_c_mort", "LowerCan: prop_mort_consumed",
                                      "LowerCan: prop_c_consumed","LowerCan: prop_c_remain")
  } else {
    
    # RS
    parameter <- c("overstory_height_thresh", "understory_height_thresh", "LowerCan: understory_mort",     
                    "LowerCan: consumption", "LowerCan: overstory_mort_k1", "LowerCan: overstory_mort_k2",   
                    "I'")
      
    # Response variable names
    response_variable_names <- c("LowerCan: prop_c_mort", "LowerCan: prop_mort_consumed",
                                       "LowerCan: prop_c_consumed", "LowerCan: prop_c_remain")
  }
  
  # ----
  # Make a tibble for analyzing sensitivity
  
  # First-order indices
  sobol_fire_1st <- tibble(prop_c_mort_up = sobol_upper_canopy_mort_rel$S$original,
                           prop_mort_consumed_up = sobol_upper_canopy_mort_consumed_rel$S$original,
                           prop_c_consumed_up = sobol_upper_canopy_c_consumed_rel$S$original,
                           prop_c_remain_up = sobol_upper_canopy_c_remain_rel$S$original,
                           prop_c_mort_low = sobol_lower_canopy_mort_rel$S$original,
                           prop_mort_consumed_low = sobol_lower_canopy_mort_consumed_rel$S$original,
                           prop_c_consumed_low = sobol_lower_canopy_c_consumed_rel$S$original,
                           prop_c_remain_low = sobol_lower_canopy_c_remain_rel$S$original,
                           parameter=parameter)
  response_variable_limits_1st <- names(sobol_fire_1st[1:8])
  sobol_fire_1st <- tidyr::gather(sobol_fire_1st, response_variable, sensitivity_value, 1:8)
  
  # Total indices
  sobol_fire_total <- tibble(prop_c_mort_up = sobol_upper_canopy_mort_rel$T$original,
                             prop_mort_consumed_up = sobol_upper_canopy_mort_consumed_rel$T$original,
                             prop_c_consumed_up = sobol_upper_canopy_c_consumed_rel$T$original,
                             prop_c_remain_up = sobol_upper_canopy_c_remain_rel$T$original,
                             prop_c_mort_low = sobol_lower_canopy_mort_rel$T$original,
                             prop_mort_consumed_low = sobol_lower_canopy_mort_consumed_rel$T$original,
                             prop_c_consumed_low = sobol_lower_canopy_c_consumed_rel$T$original,
                             prop_c_remain_low = sobol_lower_canopy_c_remain_rel$T$original,
                             parameter=parameter)
  response_variable_limits_total <- names(sobol_fire_total[1:8])
  sobol_fire_total <- tidyr::gather(sobol_fire_total, response_variable, sensitivity_value, 1:8)
  
  
  # ****** Need to save output tibbles so that final figures can be made *****
  
  
  # ---------------------------------------------------------------------
  # Figures 
  
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
    labs(title = fig_title, x = "Parameter", y = "Response Variable")
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
    labs(title = fig_title, x = "Parameter", y = "Response Variable")
  #plot(x)
  ggsave(paste("sobal_total_", watershed, "_", stand_age, ".pdf",sep=""), plot = x,
         path = output_path, width = 8, height=6)
  
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch sensitivity evaluation function


# # ----
# HJA
#
# patch_sens_sobol_eval(num_canopies = 2,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_HJA,
#                       initial_date = ymd("1957-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_HJA,
#                       stand_age_vect = c(1968,1978,1998,2028,2058,2098,2148),
#                       top_par_output = OUTPUT_DIR_1_HJA_TOP_PS,
#                       fig_title = "HJA")

# ----
# P301

# Stand age 5 years
patch_sens_sobol_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_2.1_P300_STAND1,
                      initial_date = ymd("1941-10-01"),
                      parameter_file = RHESSYS_PAR_SOBOL_2.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      fig_title = "Sobol: P300 at stand age 5",
                      watershed = "P300",
                      stand_age = "5",
                      sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_MARTINEZ_2.1_P300,
                      output_path = OUTPUT_DIR_2)

# Stand age 20 years 
patch_sens_sobol_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_2.1_P300_STAND2,
                      initial_date = ymd("1941-10-01"),
                      parameter_file = RHESSYS_PAR_SOBOL_2.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      fig_title = "Sobol: P300 at stand age 20",
                      watershed = "P300",
                      stand_age = "20",
                      sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2.1_P300,
                      output_path = OUTPUT_DIR_2)

# Stand age 80 years
patch_sens_sobol_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_2.1_P300_STAND3,
                      initial_date = ymd("1941-10-01"),
                      parameter_file = RHESSYS_PAR_SOBOL_2.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      fig_title = "Sobol: P300 at stand age 80",
                      watershed = "P300",
                      stand_age = "80",
                      sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2.1_P300,
                      output_path = OUTPUT_DIR_2)



# # ----
# RS
#
# patch_sens_sobol_eval(num_canopies = 1,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_RS,
#                       initial_date = ymd("1988-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_RS,
#                       stand_age_vect = c(1994,2001,2009,2019,2029,2049,2069),
#                       top_par_output = OUTPUT_DIR_1_RS_TOP_PS,
#                       fig_title = "RS")
# 
# # ----
# SF
#
# patch_sens_sobol_eval(num_canopies = 2,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_SF,
#                       initial_date = ymd("1941-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_SF,
#                       stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
#                       top_par_output = OUTPUT_DIR_1_SF_TOP_PS,
#                       fig_title = "SF")





