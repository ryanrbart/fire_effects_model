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
                            response_can_group,
                            response_can_group_name,
                            sobol_indices,
                            y_axis,
                            plot_title){
  
  # Import csv output from 2.6 and stack for each watershed
  sens_results_list <- list()
  for (aa in seq_along(stand_age)){
    sens_results_list[[aa]] <- read_csv(file.path(output_path, paste("2_sobol_", sobol_indices,"_",stand_age[aa],"_",watershed,".csv",sep="")))
  }
  sens_results_by_stand_age <- bind_rows(sens_results_list, .id="stand_num")
  
  # If Rattlesnake, add dummy response_canopy_group column to match sites with 2 canopies
  if (watershed == "RS"){
    sens_results_by_stand_age$response_canopy_group <- response_can_group
  }
  
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
  
  if (watershed == "RS"){
    # Prints a consolodated figure for RS
    x <- sens_results_by_stand_age %>% 
      dplyr::filter(response_canopy_group == response_can_group) %>%
      dplyr::group_by(parameter,response_variable,response_canopy_group) %>% 
      dplyr::summarise(sensitivity_value=mean(sensitivity_value)) %>% 
      ggplot(.) +
      geom_col(aes(x=parameter, y=sensitivity_value, group = parameter), 
               color="black", width = .8) +
      labs(title = plot_title, x = "Parameter", y = y_axis) +
      scale_color_discrete(name="Parameter") +
      scale_linetype_discrete(name="Parameter") +
      facet_grid(response_variable~.) +
      theme_bw(base_size = 10) +
      theme(axis.text.x = element_text(angle = 300, hjust=0, vjust=0.6)) +
      NULL
    #plot(x)
    ggsave(paste("sobal_ts_", sobol_indices, "_", watershed, "_", response_can_group_name,"2.pdf",sep=""), plot = x,
           path = output_path, width = 5, height=6)
  }
  
  x <- sens_results_by_stand_age %>% 
    dplyr::filter(response_canopy_group == response_can_group) %>% 
    ggplot(.) +
    geom_col(aes(x=stand_num, y=sensitivity_value, group = parameter), 
             color="black", width = .8) +
    labs(title = plot_title, x = "Stand Age (years)", y = y_axis) +
    scale_color_discrete(name="Parameter") +
    scale_linetype_discrete(name="Parameter") +
    scale_x_discrete(labels = c(stand_age)) +
    facet_grid(response_variable~parameter) +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 300, hjust=0, vjust=0.6)) +
    NULL
  
  #plot(x)
  ggsave(paste("sobal_ts_", sobol_indices, "_", watershed, "_", response_can_group_name,".pdf",sep=""), plot = x,
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


# HJA

# 1st order, upper canopy
process_sens_ts(stand_age = stand_age_hja,
                output_path = OUTPUT_DIR_2,
                watershed = "HJA",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "H.J. Andrews: First-order Indices for Upper Canopy"
)

# 1st order, lower canopy
process_sens_ts(stand_age = stand_age_hja,
                output_path = OUTPUT_DIR_2,
                watershed = "HJA",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "H.J. Andrews: First-order Indices for Lower Canopy"
)


# total, upper canopy
process_sens_ts(stand_age = stand_age_hja,
                output_path = OUTPUT_DIR_2,
                watershed = "HJA",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "H.J. Andrews: Total-order Indices for Upper Canopy"
)

# total, lower canopy
process_sens_ts(stand_age = stand_age_hja,
                output_path = OUTPUT_DIR_2,
                watershed = "HJA",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "H.J. Andrews: Total-order Indices for Lower Canopy"
)


# -----
# P300

# 1st order, upper canopy
process_sens_ts(stand_age = stand_age_p300,
                output_path = OUTPUT_DIR_2,
                watershed = "P300",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "P301: First-order Indices for Upper Canopy"
)

# 1st order, lower canopy
process_sens_ts(stand_age = stand_age_p300,
                output_path = OUTPUT_DIR_2,
                watershed = "P300",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "P301: First-order Indices for Lower Canopy"
)


# total, upper canopy
process_sens_ts(stand_age = stand_age_p300,
                output_path = OUTPUT_DIR_2,
                watershed = "P300",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "P301: Total-order Indices for Upper Canopy"
)

# total, lower canopy
process_sens_ts(stand_age = stand_age_p300,
                output_path = OUTPUT_DIR_2,
                watershed = "P300",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "P301: Total-order Indices for Lower Canopy"
)


# -----
# RS

# 1st order, upper canopy
process_sens_ts(stand_age = stand_age_rs,
                output_path = OUTPUT_DIR_2,
                watershed = "RS",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "Rattlesnake: First-order Indices"
)

# total, upper canopy
process_sens_ts(stand_age = stand_age_rs,
                output_path = OUTPUT_DIR_2,
                watershed = "RS",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "Rattlesnake: Total-order Indices"
)


# -----
# SF

# 1st order, upper canopy
process_sens_ts(stand_age = stand_age_sf,
                output_path = OUTPUT_DIR_2,
                watershed = "SF",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "Santa Fe: First-order Indices for Upper Canopy"
)

# 1st order, lower canopy
process_sens_ts(stand_age = stand_age_sf,
                output_path = OUTPUT_DIR_2,
                watershed = "SF",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "1st",
                y_axis = "First-order Indices",
                plot_title = "Santa Fe: First-order Indices for Lower Canopy"
)


# total, upper canopy
process_sens_ts(stand_age = stand_age_sf,
                output_path = OUTPUT_DIR_2,
                watershed = "SF",
                response_can_group = "Upper Canopy",
                response_can_group_name = "upper",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "Santa Fe: Total-order Indices for Upper Canopy"
)

# total, lower canopy
process_sens_ts(stand_age = stand_age_sf,
                output_path = OUTPUT_DIR_2,
                watershed = "SF",
                response_can_group = "Lower Canopy",
                response_can_group_name = "lower",
                sobol_indices = "total",
                y_axis = "Total-order Indices",
                plot_title = "Santa Fe: Total-order Indices for Lower Canopy"
)






# --------------------------------------------------------------


# Should I be plotting response variables or parameters???

# Variants
# 4 Watersheds
# 2 sobol parameters (first and total)
# 8 response variables (4 for RS) (4 variables/2 canopies)
# 7 facet panes (parameters)

# 64 (7 pane figures)


