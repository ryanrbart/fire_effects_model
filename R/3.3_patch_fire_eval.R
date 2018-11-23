# Patch Fire Evaluation
#
# Contains scripts for evaluating patch-level fire effects

source("R/0.1_utilities.R")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Patch fire data processing function

patch_fire_eval <- function(num_canopies,
                            allsim_path,
                            initial_date,
                            parameter_file,
                            watershed,
                            output_path,
                            cwdc_yes){
  
  # ----
  # Canopies
  patch_canopy <- readin_rhessys_output_cal(var_names = c("leafc",
                                                          "stemc",
                                                          "height"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
  
  if (watershed == "P300" | watershed == "SF"){
    patch_canopy_diff <- patch_canopy %>%
      dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
             absolute_change = `1941-10-11` - `1941-10-03`)
  }
  if (watershed == "HJA"){
    patch_canopy_diff <- patch_canopy %>%
      dplyr::filter(dates == ymd("1957-10-03") | dates == ymd("1957-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1957-10-11` - `1957-10-03`)/`1957-10-03`)*100, 
             absolute_change = `1957-10-11` - `1957-10-03`)
  }
  if (watershed == "RS"){
    patch_canopy_diff <- patch_canopy %>%
      dplyr::filter(dates == ymd("1988-10-03") | dates == ymd("1988-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1988-10-11` - `1988-10-03`)/`1988-10-03`)*100, 
             absolute_change = `1988-10-11` - `1988-10-03`)
  }
  rm(patch_canopy)
  
  
  # ----
  # Litter and soil
  patch_ground <- readin_rhessys_output_cal(var_names = c("litrc",
                                                          "soil1c"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = 1)
  
  if (watershed == "P300" | watershed == "SF"){
    patch_ground_diff <- patch_ground %>%
      dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
             absolute_change = `1941-10-11` - `1941-10-03`)
  }
  if (watershed == "HJA"){
    patch_ground_diff <- patch_ground %>%
      dplyr::filter(dates == ymd("1957-10-03") | dates == ymd("1957-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1957-10-11` - `1957-10-03`)/`1957-10-03`)*100, 
             absolute_change = `1957-10-11` - `1957-10-03`)
  }
  if (watershed == "RS"){
    patch_ground_diff <- patch_ground %>%
      dplyr::filter(dates == ymd("1988-10-03") | dates == ymd("1988-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1988-10-11` - `1988-10-03`)/`1988-10-03`)*100, 
             absolute_change = `1988-10-11` - `1988-10-03`)
  }
  rm(patch_ground)
  
  
  # ----
  # Fire
  patch_fire <- readin_rhessys_output_cal(var_names = c("canopy_target_prop_mort",
                                                        "canopy_target_prop_mort_consumed",
                                                        "canopy_target_prop_c_consumed",
                                                        "canopy_target_prop_c_remain"),
                                          path = allsim_path,
                                          initial_date = initial_date,
                                          parameter_file = parameter_file,
                                          num_canopies = num_canopies)
  
  if (watershed == "P300" | watershed == "SF"){
    patch_fire_diff <- patch_fire %>%
      dplyr::filter(dates == ('1941-10-11')) %>%
      spread(dates, value) %>%
      mutate(relative_change = as.double(`1941-10-11`)*100)
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
  
  
  # ----
  # CWD
  if (cwdc_yes == TRUE){
    patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
    
    if (watershed == "P300" | watershed == "SF"){
      patch_cwdc_diff <- patch_cwdc %>%
        group_by(run, dates, var_type, world, pspread_levels) %>%
        summarize(value = sum(value)) %>%
        dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
        spread(dates, value) %>%
        mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100,
               absolute_change = `1941-10-11` - `1941-10-03`)
    }
    if (watershed == "HJA"){
      patch_cwdc_diff <- patch_cwdc %>%
        group_by(run, dates, var_type, world, pspread_levels) %>%
        summarize(value = sum(value)) %>%
        dplyr::filter(dates == ymd("1957-10-03") | dates == ymd("1957-10-11")) %>%
        spread(dates, value) %>%
        mutate(relative_change = ((`1957-10-11` - `1957-10-03`)/`1957-10-03`)*100,
               absolute_change = `1957-10-11` - `1957-10-03`)
    }
    if (watershed == "RS"){
      patch_cwdc_diff <- patch_cwdc %>%
        group_by(run, dates, var_type, world, pspread_levels) %>%
        summarize(value = sum(value)) %>%
        dplyr::filter(dates == ymd("1988-10-03") | dates == ymd("1988-10-11")) %>%
        spread(dates, value) %>%
        mutate(relative_change = ((`1988-10-11` - `1988-10-03`)/`1988-10-03`)*100,
               absolute_change = `1988-10-11` - `1988-10-03`)
    }
    rm(patch_cwdc)
  }
  

  
  # ---------------------------------------------------------------------
  # Initialize other variables
  
  if (watershed == "HJA"){
    world_file_yr <- c(
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1963M10D1H1.state" = "Stand\nage:\n5 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1970M10D1H1.state" = "Stand\nage:\n12 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1978M10D1H1.state" = "Stand\nage:\n20 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y1998M10D1H1.state" = "Stand\nage:\n40 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2028M10D1H1.state" = "Stand\nage:\n70 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2058M10D1H1.state" = "Stand\nage:\n100 yrs",
      "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2098M10D1H1.state" = "Stand\nage:\n140 yrs")
    
    stand_age = c("5","12","20","40","70","100","140")
  }
  
  if (watershed == "P300"){
    world_file_yr <- c(
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1947M10D1H1.state" = "Stand\nage:\n5 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1954M10D1H1.state" = "Stand\nage:\n12 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1962M10D1H1.state" = "Stand\nage:\n20 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1972M10D1H1.state" = "Stand\nage:\n30 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y1982M10D1H1.state" = "Stand\nage:\n40 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2002M10D1H1.state" = "Stand\nage:\n60 yrs",
      "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state" = "Stand\nage:\n80 yrs")
    
    stand_age = c("5","12","20","30","40","60","80")
  }
  
  if (watershed == "RS"){
    world_file_yr <- c(
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y1994M10D1H1.state" = "Stand\nage:\n5 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2001M10D1H1.state" = "Stand\nage:\n12 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2009M10D1H1.state" = "Stand\nage:\n20 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2019M10D1H1.state" = "Stand\nage:\n30 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2029M10D1H1.state" = "Stand\nage:\n40 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2049M10D1H1.state" = "Stand\nage:\n60 yrs",
      "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2069M10D1H1.state" = "Stand\nage:\n80 yrs")
    
    stand_age = c("5","12","20","30","40","60","80")
  }
  
  if (watershed == "SF"){
    world_file_yr <- c(
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1947M10D1H1.state" = "Stand\nage:\n5 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1954M10D1H1.state" = "Stand\nage:\n12 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1962M10D1H1.state" = "Stand\nage:\n20 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1972M10D1H1.state" = "Stand\nage:\n30 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1982M10D1H1.state" = "Stand\nage:\n40 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2002M10D1H1.state" = "Stand\nage:\n60 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2022M10D1H1.state" = "Stand\nage:\n80 yrs")
    
    stand_age = c("5","12","20","30","40","60","80")
  }
  
  canopy <- c("1" = "Upper Canopy","2" = "Lower Canopy")
  
  
  
  
  
  
  # ---------------------------------------------------------------------
  # Figures
  
  
  
  variable_types <- c("leafc","stemc","height","litrc","soil1c",
                      "canopy_target_prop_mort","canopy_target_prop_mort_consumed",
                      "canopy_target_prop_c_consumed","canopy_target_prop_c_remain")
  
  single_canopy <- c("litrc","soil1c","cwdc")
  
  fig_title <- c("Leaf Carbon", "Stem Carbon", "Height", "Litter Carbon", "Soil Carbon", 
                 "Mortality", "Consumption", "Canopy Consumption", "Canopy Remaining as Litter")
  
  y_label <- c("Change in Leaf Carbon (%)", "Change in Stem Carbon (%)", "Change in Height (%)",
               "Change in Litter Carbon (%)", "Change in Soil Carbon (%)", "Mortality (%)", 
               "Proportion of Mortality Consumed (%)", "Canopy Carbon Consumed (%)",
               "Canopy Carbon Remaining as Litter (%)")
  
  # Processing cwd is super slow. Option to ignore it when processing
  if (cwdc_yes == TRUE){
    variable_types[10] <- "cwdc"
    fig_title[10] <- "Coarse Woody Debris Carbon"
    y_label[10] <- "Change in Coarse Woody Debris Carbon (%)"
  }
  
  
  # -----
  
  
  for (aa in seq_along(variable_types)){
    
    # Select which 'patch_***_diff' to use 
    if (variable_types[aa] %in% c("leafc","stemc","height")){happy = patch_canopy_diff}
    if (variable_types[aa] %in% c("litrc","soil1c")){happy = patch_ground_diff}
    if (variable_types[aa] %in% c("canopy_target_prop_mort",
                                  "canopy_target_prop_mort_consumed",
                                  "canopy_target_prop_c_consumed",
                                  "canopy_target_prop_c_remain")){happy = patch_fire_diff}
    if (variable_types[aa] %in% c("cwdc")){happy = patch_cwdc_diff}
    
    # ----
    
    if (variable_types[aa] %in% single_canopy){       # Facet with one canopy
      tmp <- dplyr::filter(happy,var_type == variable_types[aa])
      x <- ggplot(data = tmp) +
        geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                     fill = "olivedrab3",
                     color="black", 
                     outlier.shape = NA) +
        facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
        theme(legend.position = "none") +
        labs(title = fig_title[aa], x = "Intensity (I')", y = y_label[aa]) +
        scale_x_continuous(breaks = c(0.2, 0.8)) +
        theme_set(theme_bw(base_size = 11)) +
        NULL
      #plot(x)
      ggsave(paste(watershed,"_", variable_types[aa], ".pdf", sep=""), plot = x, 
             device = "pdf", path = output_path, width = 7, height = 5)
      
    } else {       # Facet with two canopies
      
      tmp <- dplyr::filter(happy,var_type == variable_types[aa])
      x <- ggplot(data = tmp) +
        geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                     fill = "olivedrab3",
                     color="black", 
                     outlier.shape = NA) +
        facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
        theme(legend.position = "none") +
        labs(title = fig_title[aa], x = "Intensity (I')", y = y_label[aa]) +
        scale_x_continuous(breaks = c(0.2, 0.8)) +
        theme_set(theme_bw(base_size = 11)) +
        NULL
      #plot(x)
      ggsave(paste(watershed,"_", variable_types[aa], ".pdf", sep=""), plot = x, 
             device = "pdf", path = output_path, width = 7, height = 5)
    }
  }
}
  
  # ----
  # 
  # tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "leafc")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Leaf Carbon", x = "Intensity", y = "Change in Leaf Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_leafc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 
  # tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "stemc")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Stem Carbon", x = "Intensity", y = "Change in Stem Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_stemc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 
  # tmp <- dplyr::filter(p300_patch_height_diff,var_type == "height")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Height", x = "Intensity", y = "Change in Height (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_height.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # # ----
  # 
  # tmp <- dplyr::filter(p300_patch_ground_diff, var_type == "litrc")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Litter Carbon", x = "Intensity", y = "Change in Litter Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_litrc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # tmp <- dplyr::filter(p300_patch_ground_diff, var_type == "soil1c")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   #geom_bar(stat="identity",aes(x=pspread_levels,y=relative_change), color="olivedrab3") +
  #   facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Soil Carbon", x = "Intensity", y = "Change in Soil Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_soil1c.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # # tmp <- dplyr::filter(p300_patch_cwdc_diff, var_type == "cwdc")
  # # x <- ggplot(data = tmp) +
  # #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  # #                fill = "olivedrab3",
  # #                color="black", 
  # #                outlier.shape = NA) +
  # #   facet_grid(.~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  # #   theme(legend.position = "none") +
  # #   labs(title = "Coarse Woody Debris Carbon", x = "Intensity", y = "Change in CWD Carbon (%)") +
  # #   scale_x_continuous(breaks = c(0.2, 0.8))
  # # plot(x)
  # #ggsave("p300_patch_sim_change_cwdc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 
  # # ----
  # 
  # tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "leafc")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Leaf Carbon", x = "Intensity", y = "Change in Leaf Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_leafc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 
  # tmp <- dplyr::filter(p300_patch_canopy_diff,var_type == "stemc")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Stem Carbon", x = "Intensity", y = "Change in Stem Carbon (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_stemc.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 
  # tmp <- dplyr::filter(p300_patch_height_diff,var_type == "height")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Height", x = "Intensity", y = "Change in Height (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_change_height.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # # ---
  # tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_mort")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Mortality", x = "Intensity", y = "Mortality (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_mortality.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_mort_consumed")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Consumption", x = "Intensity", y = "Proportion of Mortality Consumed (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_mortality_consumed.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_c_consumed")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Canopy Consumption", x = "Intensity", y = "Canopy Carbon Consumed (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_c_consumed.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # tmp <- dplyr::filter(p300_patch_fire_diff,var_type == "canopy_target_prop_c_remain")
  # x <- ggplot(data = tmp) +
  #   geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
  #                fill = "olivedrab3",
  #                color="black", 
  #                outlier.shape = NA) +
  #   facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
  #   theme(legend.position = "none") +
  #   labs(title = "Canopy Remaining as Litter", x = "Intensity", y = "Canopy Carbon Remaining as Litter (%)") +
  #   scale_x_continuous(breaks = c(0.2, 0.8))
  # plot(x)
  # #ggsave("p300_patch_sim_c_remain.pdf",plot = x, path = OUTPUT_DIR_2)
  # 
  # 





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch sensitivity evaluation function

# ---------------------------------------------------------------------
# HJA





# ---------------------------------------------------------------------
# P300

patch_fire_eval(num_canopies = 2,
                allsim_path = RHESSYS_ALLSIM_DIR_3.2_P300,
                initial_date = ymd("1941-10-01"),
                parameter_file = RHESSYS_PAR_SIM_3.1_P300,
                watershed = "P300",
                output_path = OUTPUT_DIR_3,
                cwdc_yes = FALSE
                )


# ---------------------------------------------------------------------
# RS



# ---------------------------------------------------------------------
# SF






# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


# Find parameter to compare
ls(p300_patch_canopy_diff)
unique(p300_patch_canopy_diff$world)

x <- p300_patch_canopy_diff %>% 
  dplyr::filter(var_type == "stemc", 
                world == "ws_p300/worldfiles/p300_30m_2can_patch_9445.world.Y2022M10D1H1.state",
                canopy_layer == 1) %>% 
  ggplot(.) +
  geom_point(aes(
    #x=ws_p300.defs.veg_p300_conifer.def.understory_mort,
    #x=ws_p300.defs.veg_p300_conifer.def.consumption,
    #x=ws_p300.defs.veg_p300_conifer.def.overstory_mort_k1,
    x=ws_p300.defs.veg_p300_conifer.def.overstory_mort_k2,
    y=relative_change)) +
  facet_wrap(.~pspread_levels)
plot(x)



