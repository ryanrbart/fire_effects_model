# Patch Fire Sensitivity Analysis
#
# Contains scripts for evaluating parameter sensitivity of fire effects model

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))

# ---------------------------------------------------------------------
# Patch simulation evaluation function

patch_sens_sobol_eval <- function(num_canopies,
                                  allsim_path,
                                  initial_date,
                                  parameter_file,
                                  stand_age_vect,
                                  fig_title,
                                  sobol_par_input,
                                  sobol_model_input){
  
  # ---------------------------------------------------------------------
  # Patch Fire data processing
  # Computes differences in variables for several days before to several days after a fire.
  
  patch_canopy <- readin_rhessys_output_cal(var_names = c("leafc", "stemc"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
  patch_canopy_diff <- patch_canopy %>%
    dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
    spread(dates, value) %>%
    mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
  rm(patch_canopy)
  
  
  patch_ground <- readin_rhessys_output_cal(var_names = c("litrc", "soil1c"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = 1)
  patch_ground$value <- as.double(patch_ground$value)
  patch_ground_diff <- patch_ground %>%
    dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
    spread(dates, value) %>%
    mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
  rm(patch_ground)
  
  
  patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                          path = allsim_path,
                                          initial_date = initial_date,
                                          parameter_file = parameter_file,
                                          num_canopies = num_canopies)
  patch_cwdc_diff <- patch_cwdc %>%
    group_by(run, dates, var_type) %>%
    summarize(value = sum(value)) %>%
    dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
    spread(dates, value) %>%
    mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
  rm(patch_cwdc)
  
  
  patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
  patch_height_diff <- patch_height %>%
    dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
    spread(dates, value) %>%
    mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
  rm(patch_height)
  
  
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
    mutate(relative_change = `1941-10-11`*100)
  rm(patch_fire)
  
  
  # ---------------------------------------------------------------------
  # Sensitivity analysis
  # Generate y from above, and possibly substitute values for stand_age
  
  # Import the parameter sets used in the sobol model.
  sobol_par <- read.csv(sobol_par_input)
  sobol_model <- readRDS(sobol_model_input)


  # Sobol models of relative loss

  # Height, Leaf, stem and ground store variables
  sobol_upper_canopy_height_rel <- tell(sobol_model, dplyr::filter(patch_height_diff, canopy_layer==1)$relative_change) 
  sobol_lower_canopy_height_rel <- tell(sobol_model, dplyr::filter(patch_height_diff, canopy_layer==2)$relative_change) 
  sobol_upper_canopy_leafc_rel <- tell(sobol_model, dplyr::filter(patch_canopy_diff, canopy_layer==1, var_type=="leafc")$relative_change) 
  sobol_lower_canopy_leafc_rel <- tell(sobol_model, dplyr::filter(patch_canopy_diff, canopy_layer==2, var_type=="leafc")$relative_change) 
  sobol_upper_canopy_stemc_rel <- tell(sobol_model, dplyr::filter(patch_canopy_diff, canopy_layer==1, var_type=="stemc")$relative_change) 
  sobol_lower_canopy_stemc_rel <- tell(sobol_model, dplyr::filter(patch_canopy_diff, canopy_layer==2, var_type=="stemc")$relative_change) 
  sobol_cwd_rel <- tell(sobol_model, patch_cwdc_diff$relative_change) 
  sobol_litrc_rel <- tell(sobol_model, dplyr::filter(patch_ground_diff, var_type=="litrc")$relative_change) 
  sobol_soil1c_rel <- tell(sobol_model, dplyr::filter(patch_ground_diff, var_type=="soil1c")$relative_change) 

  # Fire variables
  sobol_upper_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort", canopy_layer==1)$relative_change) 
  sobol_lower_canopy_mort_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort", canopy_layer==2)$relative_change) 
  sobol_upper_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed", canopy_layer==1)$relative_change) 
  sobol_lower_canopy_mort_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_mort_consumed", canopy_layer==2)$relative_change) 
  sobol_upper_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed", canopy_layer==1)$relative_change) 
  sobol_lower_canopy_c_consumed_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_consumed", canopy_layer==2)$relative_change) 
  sobol_upper_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain", canopy_layer==1)$relative_change) 
  sobol_lower_canopy_c_remain_rel <- tell(sobol_model, dplyr::filter(patch_fire_diff, var_type=="canopy_target_prop_c_remain", canopy_layer==2)$relative_change) 

  
  # Make a tibble for analyzing  
  
  # Parameter names for HJA, P300 and SF
  parameter_long <- c("overstory_height_thresh", "understory_height_thresh", "shrub_understory_mort",     
                      "shrub_consumption", "shrub_overstory_mort_k1", "shrub_overstory_mort_k2",   
                      "conifer_understory_mort", "conifer_consumption", "conifer_overstory_mort_k1", 
                      "conifer_overstory_mort_k2", "pspread")
  
  # Parameter names for RS
  parameter_short <- c("overstory_height_thresh", "understory_height_thresh", "shrub_understory_mort",     
                       "shrub_consumption", "shrub_overstory_mort_k1", "shrub_overstory_mort_k2",   
                       "pspread")
  
  sobol_veg_ground <- tibble(a=sobol_upper_canopy_height_rel$S$original,
                             b=sobol_lower_canopy_height_rel$S$original,
                             c=sobol_upper_canopy_leafc_rel$S$original,
                             d=sobol_lower_canopy_leafc_rel$S$original,
                             e=sobol_upper_canopy_stemc_rel$S$original,
                             f=sobol_lower_canopy_stemc_rel$S$original,
                             g=sobol_cwd_rel$S$original,
                             h=sobol_litrc_rel$S$original,
                             i=sobol_soil1c_rel$S$original,
                             parameter=parameter_long)
  response_variable_veg_ground <- names(sobol_veg_ground[1:9])
  sobol_veg_ground <- tidyr::gather(sobol_veg_ground, response_variable, sensitivity_value, 1:9)
  
  sobol_fire <- tibble(prop_c_mort_o = sobol_upper_canopy_mort_rel$S$original,
                       prop_c_mort_u = sobol_lower_canopy_mort_rel$S$original,
                       prop_mort_consumed_o = sobol_upper_canopy_mort_consumed_rel$S$original,
                       prop_mort_consumed_u = sobol_lower_canopy_mort_consumed_rel$S$original,
                       prop_c_consumed_o = sobol_upper_canopy_c_consumed_rel$S$original,
                       prop_c_consumed_u = sobol_lower_canopy_c_consumed_rel$S$original,
                       prop_c_remain_o = sobol_upper_canopy_c_remain_rel$S$original,
                       prop_c_remain_u = sobol_lower_canopy_c_remain_rel$S$original,
                       parameter=parameter_long)
  response_variable_fire <- names(sobol_fire[1:8])
  sobol_fire <- tidyr::gather(sobol_fire, response_variable, sensitivity_value, 1:8)
  
  #row.names(sobol_model$S)
  
  # ---------------------------------------------------------------------
  # Figures 
  
  theme_set(theme_bw(base_size = 12))
  ggplot(sobol_fire) +
    geom_tile(aes(x=parameter, y=response_variable, fill=sensitivity_value)) +
    scale_fill_continuous(name="Sensitivity\nValue") +
    scale_x_discrete(limits=c(parameter_long)) +
    scale_y_discrete(limits=c(response_variable_fire)) +
    theme(axis.text.x = element_text(angle = 330, hjust=0)) +
    labs(title = fig_title, x = "Parameter", y = "Response Variable")
    #ggsave(paste("***_",watershed,".pdf",sep=""), plot = x, path = OUTPUT_DIR_1)
    
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch sensitivity evaluation function


# # HJA
# patch_sens_sobol_eval(num_canopies = 2,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_HJA,
#                       initial_date = ymd("1957-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_HJA,
#                       stand_age_vect = c(1968,1978,1998,2028,2058,2098,2148),
#                       top_par_output = OUTPUT_DIR_1_HJA_TOP_PS,
#                       fig_title = "HJA")

# P300
patch_sens_sobol_eval(num_canopies = 2,
                      allsim_path = RHESSYS_ALLSIM_DIR_2.1_P300_STAND1,
                      initial_date = ymd("1941-10-01"),
                      parameter_file = RHESSYS_PAR_SOBOL_2.1_P300,
                      stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
                      fig_title = "Sensitivity Sobol: P300 at stand age 5",
                      sobol_par_input = RHESSYS_PAR_SOBOL_2.1_P300,
                      sobol_model_input = RHESSYS_PAR_SOBOL_MODEL_2.1_P300)



# RS
# patch_sens_sobol_eval(num_canopies = 1,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_RS,
#                       initial_date = ymd("1988-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_RS,
#                       stand_age_vect = c(1994,2001,2009,2019,2029,2049,2069),
#                       top_par_output = OUTPUT_DIR_1_RS_TOP_PS,
#                       fig_title = "RS")
# 
# # SF
# patch_sens_sobol_eval(num_canopies = 2,
#                       allsim_path = RHESSYS_ALLSIM_DIR_1.1_SF,
#                       initial_date = ymd("1941-10-01"),
#                       parameter_file = RHESSYS_PAR_FILE_1.1_SF,
#                       stand_age_vect = c(1947,1954,1962,1972,1982,2002,2022),
#                       top_par_output = OUTPUT_DIR_1_SF_TOP_PS,
#                       fig_title = "SF")





