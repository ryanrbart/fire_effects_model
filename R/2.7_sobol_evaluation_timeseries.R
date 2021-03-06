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
  
  # Change response variable prop_c_remain to prop_c_residual
  sens_results_by_stand_age$response_variable <- dplyr::recode(sens_results_by_stand_age$response_variable, "prop_c_remain"="prop_c_residual")
  
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
                                                                       "prop_c_residual"))
  
  sens_results_by_stand_age$parameter <- factor(sens_results_by_stand_age$parameter,
                                                        levels = c("h_overstory", "h_understory",
                                                                   "k_mort_u", "k_consumption",
                                                                   "k1_mort_o", "k2_mort_o",
                                                                   "I'"))
  
  parameter_var <- c("h_overstory", "h_understory", "k_mort_u", 
                 "k_consumption", "k1_mort_o",
                 "k2_mort_o", "I'")
  
  parameter_names <- c(expression('h'[o]), expression('h'[u]), 
                       expression('k'[u_mort]), expression('k'[cons]), 
                       expression('k'['o_mort_1']), expression('k'['o_mort_2']),   
                       "FII")
  
  # Not used anymore.
  # response_variable_lab <- c("prop_c_mort" = expression('V'[pMort]),
  #                            "prop_mort_consumed" = expression('V'[pCons]),
  #                            "prop_c_consumed" = expression('V'[Cons]),
  #                            "prop_c_residual" = expression('V'[Resid]))

  # # ------
  # Figures
  
  if (watershed == "RS" || response_can_group == "Lower Canopy"){
    # Prints a consolidated figure for RS
    
    levels(sens_results_by_stand_age$response_variable) <- c(expression('V'[pMort]),
                                                             expression('V'[pCons]),
                                                             expression('V'[Cons]),
                                                             expression('V'[Resid]))
    
    x <- sens_results_by_stand_age %>% 
      dplyr::filter(response_canopy_group == response_can_group) %>%
      dplyr::group_by(parameter,response_variable,response_canopy_group) %>% 
      dplyr::summarise(sensitivity_value=mean(sensitivity_value), se_value=mean(se_value)) %>% 
      ggplot(.) +
      geom_col(aes(x=parameter, y=sensitivity_value, group = parameter), 
               color="black", fill = "Navajowhite2", width = .8) +
      geom_errorbar(aes(x=parameter, ymax=sensitivity_value+se_value,ymin =sensitivity_value-se_value),
                    color="black", width=0.4) +
      labs(title = plot_title, x = "Parameters/ Input Variable", y = y_axis) +
      scale_color_discrete(name="Parameter") +
      scale_linetype_discrete(name="Parameter") +
      scale_x_discrete(labels = c(parameter_names),
                       limits=c(parameter_var)) +
      facet_grid(response_variable~., labeller = labeller(response_variable = label_parsed)) +
      theme_bw(base_size = 16) +
      theme(axis.text.x = element_text(angle = 300, hjust=0, vjust=0.6)) +
      NULL
    #plot(x)
    ggsave(paste("sobal_ts_", sobol_indices, "_", watershed, "_", response_can_group_name,"2.pdf",sep=""), plot = x,
           path = output_path, width = 5, height=6)
  }
  
  x <- sens_results_by_stand_age %>% 
    dplyr::filter(response_canopy_group == response_can_group)
  
  # Needed to add levels to get expressions to properly work https://stackoverflow.com/questions/37089052/
  levels(x$parameter) <- c(expression('h'[o]), expression('h'[u]), 
                           expression('k'[u_mort]), expression('k'[cons]), 
                           expression('k'['o_mort_1']), expression('k'['o_mort_2']),   
                           "FII")     # Needed to add backticks to get ' to work. https://stackoverflow.com/questions/17639325/
  
  levels(x$response_variable) <- c(expression('V'[pMort]),
                                   expression('V'[pCons]),
                                   expression('V'[Cons]),
                                   expression('V'[Resid]))
  
  x <- ggplot(x) +
    geom_col(aes(x=stand_num, y=sensitivity_value, group = parameter), 
             color="black", fill = "navajowhite2", width = .8) +
    geom_errorbar(aes(x=stand_num, ymax=sensitivity_value+se_value,ymin =sensitivity_value-se_value), 
                  color="black", width=0.4) +
    labs(title = plot_title, x = "Stand Age (years)", y = y_axis) +
    scale_color_discrete(name="Parameter") +
    scale_linetype_discrete(name="Parameter") +
    scale_x_discrete(labels = c(stand_age)) +
    facet_grid(response_variable~parameter, labeller = labeller(.cols = label_parsed, .rows = label_parsed)) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6)) +
    NULL
  
  #plot(x)
  ggsave(paste("sobal_ts_", sobol_indices, "_", watershed, "_", response_can_group_name,".pdf",sep=""), plot = x,
         path = output_path, width = 8, height=6)
  
  return(x)
}


stand_age_hja = c("5","12","20","40","70","100","140")
stand_age_p300 = c("5","12","20","30","40","60","80")
stand_age_rs = c("5","12","20","30","40","60","80")
stand_age_sf = c("5","12","20","30","40","60","80")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call function 


# HJA

# 1st order, upper canopy
x_hja_upper_1 <- process_sens_ts(stand_age = stand_age_hja,
                                 output_path = OUTPUT_DIR_2,
                                 watershed = "HJA",
                                 response_can_group = "Upper Canopy",
                                 response_can_group_name = "upper",
                                 sobol_indices = "1st",
                                 y_axis = "First-Order Indices",
                                 plot_title = "H.J. Andrews: First-Order Indices for Primary Canopy"
)

# 1st order, lower canopy
x_hja_lower_1 <- process_sens_ts(stand_age = stand_age_hja,
                                 output_path = OUTPUT_DIR_2,
                                 watershed = "HJA",
                                 response_can_group = "Lower Canopy",
                                 response_can_group_name = "lower",
                                 sobol_indices = "1st",
                                 y_axis = "First-Order Indices",
                                 plot_title = "H.J. Andrews: First-Order Indices\nfor Secondary Canopy"
)


# total, upper canopy
x_hja_upper_total <- process_sens_ts(stand_age = stand_age_hja,
                                     output_path = OUTPUT_DIR_2,
                                     watershed = "HJA",
                                     response_can_group = "Upper Canopy",
                                     response_can_group_name = "upper",
                                     sobol_indices = "total",
                                     y_axis = "Total-Order Indices",
                                     plot_title = "H.J. Andrews: Total-Order Indices for Primary Canopy"
)

# total, lower canopy
x_hja_lower_total <- process_sens_ts(stand_age = stand_age_hja,
                                     output_path = OUTPUT_DIR_2,
                                     watershed = "HJA",
                                     response_can_group = "Lower Canopy",
                                     response_can_group_name = "lower",
                                     sobol_indices = "total",
                                     y_axis = "Total-Order Indices",
                                     plot_title = "H.J. Andrews: Total-Order Indices for Secondary Canopy"
)


# -----
# P300

# 1st order, upper canopy
x_p300_upper_1 <- process_sens_ts(stand_age = stand_age_p300,
                                  output_path = OUTPUT_DIR_2,
                                  watershed = "P300",
                                  response_can_group = "Upper Canopy",
                                  response_can_group_name = "upper",
                                  sobol_indices = "1st",
                                  y_axis = "First-Order Indices",
                                  plot_title = "P301: First-Order Indices for Primary Canopy"
)

# 1st order, lower canopy
x_p300_lower_1 <- process_sens_ts(stand_age = stand_age_p300,
                                  output_path = OUTPUT_DIR_2,
                                  watershed = "P300",
                                  response_can_group = "Lower Canopy",
                                  response_can_group_name = "lower",
                                  sobol_indices = "1st",
                                  y_axis = "First-Order Indices",
                                  plot_title = "P301: First-Order Indices\nfor Secondary Canopy"
)


# total, upper canopy
x_p300_upper_total <- process_sens_ts(stand_age = stand_age_p300,
                                      output_path = OUTPUT_DIR_2,
                                      watershed = "P300",
                                      response_can_group = "Upper Canopy",
                                      response_can_group_name = "upper",
                                      sobol_indices = "total",
                                      y_axis = "Total-Order Indices",
                                      plot_title = "P301: Total-Order Indices for Primary Canopy"
)

# total, lower canopy
x_p300_lower_total <- process_sens_ts(stand_age = stand_age_p300,
                                      output_path = OUTPUT_DIR_2,
                                      watershed = "P300",
                                      response_can_group = "Lower Canopy",
                                      response_can_group_name = "lower",
                                      sobol_indices = "total",
                                      y_axis = "Total-Order Indices",
                                      plot_title = "P301: Total-Order Indices for Secondary Canopy"
)


# -----
# RS

# 1st order, upper canopy
x_rs_1 <- process_sens_ts(stand_age = stand_age_rs,
                          output_path = OUTPUT_DIR_2,
                          watershed = "RS",
                          response_can_group = "Upper Canopy",
                          response_can_group_name = "upper",
                          sobol_indices = "1st",
                          y_axis = "First-Order Indices",
                          plot_title = "Rattlesnake: First-Order Indices"
)

# total, upper canopy
x_rs_total <- process_sens_ts(stand_age = stand_age_rs,
                              output_path = OUTPUT_DIR_2,
                              watershed = "RS",
                              response_can_group = "Upper Canopy",
                              response_can_group_name = "upper",
                              sobol_indices = "total",
                              y_axis = "Total-Order Indices",
                              plot_title = "Rattlesnake: Total-Order Indices"
)


# -----
# SF

# 1st order, upper canopy
x_sf_upper_1 <- process_sens_ts(stand_age = stand_age_sf,
                                output_path = OUTPUT_DIR_2,
                                watershed = "SF",
                                response_can_group = "Upper Canopy",
                                response_can_group_name = "upper",
                                sobol_indices = "1st",
                                y_axis = "First-Order Indices",
                                plot_title = "Santa Fe: First-Order Indices for Primary Canopy"
)

# 1st order, lower canopy
x_sf_lower_1 <- process_sens_ts(stand_age = stand_age_sf,
                                output_path = OUTPUT_DIR_2,
                                watershed = "SF",
                                response_can_group = "Lower Canopy",
                                response_can_group_name = "lower",
                                sobol_indices = "1st",
                                y_axis = "First-Order Indices",
                                plot_title = "Santa Fe: First-Order Indices\nfor Secondary Canopy"
)


# total, upper canopy
x_sf_upper_total <- process_sens_ts(stand_age = stand_age_sf,
                                    output_path = OUTPUT_DIR_2,
                                    watershed = "SF",
                                    response_can_group = "Upper Canopy",
                                    response_can_group_name = "upper",
                                    sobol_indices = "total",
                                    y_axis = "Total-Order Indices",
                                    plot_title = "Santa Fe: Total-Order Indices for Primary Canopy"
)

# total, lower canopy
x_sf_lower_total <- process_sens_ts(stand_age = stand_age_sf,
                                    output_path = OUTPUT_DIR_2,
                                    watershed = "SF",
                                    response_can_group = "Lower Canopy",
                                    response_can_group_name = "lower",
                                    sobol_indices = "total",
                                    y_axis = "Total-Order Indices",
                                    plot_title = "Santa Fe: Total-Order Indices for Secondary Canopy"
)



# --------------------------------------------------------------
# Cow Plot P300 and HJA

# Combine figures for Cowplot
sens_3by1 <- cowplot::plot_grid(x_rs_1,
                                x_p300_upper_1,
                                x_hja_upper_1,
                                labels=c("a","b","c"),
                                nrow=3,
                                label_size = 16)

#plot(sens_3by1)
cowplot::save_plot(file.path(OUTPUT_DIR_2, "sens_3by1.pdf"),
                   sens_3by1,
                   ncol=1,
                   nrow=3,
                   base_height=4.5,
                   base_width=8)





