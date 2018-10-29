# Figure of parameter sensitivity through time
#
# These figures are analogous to the rectangle sensitivity plots in 2.6, but
# with stand age on x-axis instead of parameter

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Function for plotting sensitivity 'timeseries'

process_sens_ts <- function(stand_age,
                            output_path,
                            watershed,
                            response_var,
                            response_can_group,
                            response_can_group_name,
                            sobol_indices,
                            y_axis,
                            plot_title){
  
  # Import csv output from 2.6 and stack for each watershed
  sens_results_list <- list()
  for (aa in seq_along(stand_age)){
    sens_results_list[[aa]] <- read_csv(file.path(output_path, paste("2_sobol_1st_",stand_age[aa],"_",watershed,".csv",sep="")))
  }
  sens_results_by_stand_age <- bind_rows(sens_results_list, .id="stand_num")
  
  # Change columns to factor
  sens_results_by_stand_age$response_canopy_group <- factor(sens_results_by_stand_age$response_canopy_group,
                                                            levels = c("Upper Canopy","Lower Canopy"))
  
  sens_results_by_stand_age$response_variable <- factor(sens_results_by_stand_age$response_variable,
                                                            levels = c("prop_c_mort",
                                                                       "prop_mort_consumed",
                                                                       "prop_c_consumed",
                                                                       "prop_c_remain"))
  
  sens_results_by_stand_age$parameter <- factor(sens_results_by_stand_age$parameter,
                                                        levels = c("h_overstory", "h_understory",
                                                                   "k_mort_u", "k_consumption",
                                                                   "k1_mort_o", "k2_mort_o",
                                                                   "I'"))
  
  # ------
  # Figures
  
  x <- sens_results_by_stand_age %>% 
    # dplyr::filter(response_variable == response_var,
    #               response_canopy_group == response_can_group) %>% 
    dplyr::filter(response_canopy_group == response_can_group) %>% 
  ggplot(.) +
    geom_line(aes(x=stand_num, y=sensitivity_value, group = parameter)) +
    labs(title = plot_title, x = "Stand Age (years)", y = y_axis) +
    scale_color_discrete(name="Parameter") +
    scale_linetype_discrete(name="Parameter") +
    scale_x_discrete(labels = c(stand_age)) +
    facet_grid(response_variable~parameter) +
    theme_bw() +
    NULL
  plot(x)
  ggsave(paste("sobal_ts_", sobol_indices, "_", watershed, "_", response_var, "_", response_can_group_name,".pdf",sep=""), plot = x,
         path = output_path, width = 8, height=6)
  
}


stand_age_hja = c("5","12","20","40","70","100","140")
stand_age_p300 = c("5","12","20","30","40","60","80")
stand_age_rs = c("5","12","20","30","40","60","80")
stand_age_sf = c("5","12","20","30","40","60","80")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call function function


  
process_sens_ts(stand_age = stand_age_hja,
                output_path = OUTPUT_DIR_2,
                watershed = "HJA",
                response_var = "prop_c_mort",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "Sensitivity of post-fire mortality to modeled parameters"
                )



process_sens_ts(stand_age = stand_age_p300,
                output_path = OUTPUT_DIR_2,
                watershed = "P300",
                response_var = "prop_c_mort",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "Sensitivity of post-fire mortality to modeled parameters"
)


# Should I be plotting response variables or parameters???

# Variants
# 4 Watersheds
# 2 sobol parameters (first and total)
# 8 response variables (4 for RS) (4 variables/2 canopies)
# 7 facet panes (parameters)

# 64 (7 pane figures)


