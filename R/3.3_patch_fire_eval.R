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
  
  if (watershed == "P300"){
    patch_canopy_diff <- patch_canopy %>%
      dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
             absolute_change = `1941-10-11` - `1941-10-03`)
  }
  if (watershed == "SF"){
    patch_canopy_diff <- patch_canopy %>%
      dplyr::filter(dates == ymd("1955-10-03") | dates == ymd("1955-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1955-10-11` - `1955-10-03`)/`1955-10-03`)*100, 
             absolute_change = `1955-10-11` - `1955-10-03`)
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
  
  if (watershed == "P300"){
    patch_ground_diff <- patch_ground %>%
      dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, 
             absolute_change = `1941-10-11` - `1941-10-03`)
  }
  if (watershed == "SF"){
    patch_ground_diff <- patch_ground %>%
      dplyr::filter(dates == ymd("1955-10-03") | dates == ymd("1955-10-11")) %>%
      spread(dates, value) %>%
      mutate(relative_change = ((`1955-10-11` - `1955-10-03`)/`1955-10-03`)*100, 
             absolute_change = `1955-10-11` - `1955-10-03`)
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
  
  
  # ----
  # CWD
  if (cwdc_yes == TRUE){
    patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                            path = allsim_path,
                                            initial_date = initial_date,
                                            parameter_file = parameter_file,
                                            num_canopies = num_canopies)
    
    if (watershed == "P300"){
      patch_cwdc_diff <- patch_cwdc %>%
        group_by(run, dates, var_type, world, pspread_levels) %>%
        summarize(value = sum(value)) %>%
        dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
        spread(dates, value) %>%
        mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100,
               absolute_change = `1941-10-11` - `1941-10-03`)
    }
    if (watershed == "SF"){
      patch_cwdc_diff <- patch_cwdc %>%
        group_by(run, dates, var_type, world, pspread_levels) %>%
        summarize(value = sum(value)) %>%
        dplyr::filter(dates == ymd("1955-10-03") | dates == ymd("1955-10-11")) %>%
        spread(dates, value) %>%
        mutate(relative_change = ((`1955-10-11` - `1955-10-03`)/`1955-10-03`)*100,
               absolute_change = `1955-10-11` - `1955-10-03`)
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
    
    stand_age = c(5,12,20,40,70,100,140)
    
    # For timeseries in cowplot
    stand_age_years <- c(1963,1970,1978,1998,2028,2058,2098)
    stores_ts <- readin_rhessys_output("ws_hja/out/1.3_hja_patch_simulation/patch_sim", g=1, c=1, p=0)
    shed_name <- "H.J. Andrews"
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
    
    stand_age = c(5,12,20,30,40,60,80)
    
    # For timeseries in cowplot
    stand_age_years <- c(1947,1954,1962,1972,1982,2002,2022)
    stores_ts <- readin_rhessys_output("ws_p300/out/1.3_p300_patch_simulation/patch_sim", g=1, c=1, p=0)
    shed_name <- "P301"
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
    
    stand_age = c(5,12,20,30,40,60,80)
    
    # For timeseries in cowplot
    stand_age_years <- c(1994,2001,2009,2019,2029,2049,2069)
    stores_ts <- readin_rhessys_output("ws_rs/out/1.3_rs_patch_simulation/patch_sim", g=1, c=1, p=0)
    shed_name <- "Rattlesnake"
  }
  
  if (watershed == "SF"){
    world_file_yr <- c(
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1961M10D1H1.state" = "Stand\nage:\n5 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1968M10D1H1.state" = "Stand\nage:\n12 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1976M10D1H1.state" = "Stand\nage:\n20 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1986M10D1H1.state" = "Stand\nage:\n30 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y1996M10D1H1.state" = "Stand\nage:\n40 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2016M10D1H1.state" = "Stand\nage:\n60 yrs",
      "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2036M10D1H1.state" = "Stand\nage:\n80 yrs")
    
    stand_age = c(5,12,20,30,40,60,80)
    
    # For timeseries in cowplot
    stand_age_years <- c(1961,1968,1976,1986,1996,2016,2036)
    stores_ts <- readin_rhessys_output("ws_sf/out/1.3_sf_patch_simulation/patch_sim", g=1, c=1, p=0)
    shed_name <- "Santa Fe"
  }
  
  canopy <- c("1" = "Upper Canopy","2" = "Lower Canopy")
  
  
  
  
  
  
  # ---------------------------------------------------------------------
  # Figures
  
  
  
  variable_types <- c("leafc","stemc","height","litrc","soil1c",
                      "canopy_target_prop_mort","canopy_target_prop_mort_consumed",
                      "canopy_target_prop_c_consumed","canopy_target_prop_c_remain")
  
  single_canopy <- c("litrc","soil1c","cwdc")
  
  fig_title_1 <- c("Leaf Carbon", "Stem Carbon", "Height", "Litter Carbon", "Soil Carbon", 
                   "Canopy Mortality", "Consumption", "Canopy Consumption", "Canopy Remaining as Litter")
  
  fig_title_2 <- c("Upper Canopy Leaf Carbon", "Upper Canopy Stem Carbon", "Upper Canopy Height", "Litter Carbon", "Soil Carbon", 
                   "Upper Canopy Mortality", "Consumption", "Upper Canopy Consumption", "Upper Canopy Remaining as Litter")
  
  y_label <- c("Change in Leaf Carbon (%)", "Change in Stem Carbon (%)", "Change in Height (%)",
               "Change in Litter Carbon (%)", "Change in Soil Carbon (%)", "Mortality (%)", 
               "Proportion of Mortality Consumed (%)", "Carbon Consumed (%)",
               "Carbon Remaining as Litter (%)")
  
  # Processing cwd is super slow. Option to ignore it when processing
  if (cwdc_yes == TRUE){
    variable_types[10] <- "cwdc"
    fig_title[10] <- "Coarse Woody Debris Carbon"
    y_label[10] <- "Change in Coarse Woody Debris Carbon (%)"
  }
  
  
  # -----
  
  figure_list <- list()
  
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
    
    if (variable_types[aa] %in% single_canopy){       # litrc, soil1c, and cwdc
      tmp <- dplyr::filter(happy,var_type == variable_types[aa])
      x <- ggplot(data = tmp) +
        geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                     fill = "Navajowhite2",
                     color="black", 
                     outlier.shape = NA) +
        facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
        labs(title = fig_title_1[aa], x = "Intensity (I')", y = y_label[aa]) +
        scale_x_continuous(breaks = c(0.2, 0.8)) +
        NULL
    
      x1 <- x + 
        theme_bw(base_size = 11) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      #plot(x1)
      ggsave(paste(watershed,"_", variable_types[aa], ".pdf", sep=""), plot = x1, 
             device = "pdf", path = output_path, width = 8, height = 3)
      
      x2 <- x +
        theme_bw(base_size = 16) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      figure_list[[aa]] <- x2
      
    } else {       # leafc, stemc, height, canopy_target_prop_mort, canopy_target_prop_mort_consumed, canopy_target_prop_c_consumed, canopy_target_prop_c_remain
      
      if (watershed == "RS"){
        tmp <- dplyr::filter(happy,var_type == variable_types[aa])
        x <- ggplot(data = tmp) +
          geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                       fill = "Navajowhite2",
                       color="black", 
                       outlier.shape = NA) +
          facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
          labs(title = fig_title_1[aa], x = "Intensity (I')", y = y_label[aa]) +
          scale_x_continuous(breaks = c(0.2, 0.8)) +
          NULL
        #plot(x)
        
        x1 <- x + 
          theme_bw(base_size = 11) +
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        #plot(x1)
        ggsave(paste(watershed,"_", variable_types[aa], ".pdf", sep=""), plot = x1, 
               device = "pdf", path = output_path, width = 8, height = 3)
        
        x2 <- x + 
          theme_bw(base_size = 16) +
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        figure_list[[aa]] <- x2
        
      } else {
        # Export two canopies for possible display
        tmp <- dplyr::filter(happy,var_type == variable_types[aa])
        x <- ggplot(data = tmp) +
          geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                       fill = "Navajowhite2",
                       color="black", 
                       outlier.shape = NA) +
          facet_grid(canopy_layer~world, labeller = labeller(world = world_file_yr, canopy_layer = canopy)) +
          labs(title = fig_title_1[aa], x = "Intensity (I')", y = y_label[aa]) +
          scale_x_continuous(breaks = c(0.2, 0.8)) +
          theme_bw(base_size = 11) +
          theme(legend.position = "none") +
          NULL
        #plot(x)
        ggsave(paste(watershed,"_", variable_types[aa], "_two_canopies.pdf", sep=""), plot = x, 
               device = "pdf", path = output_path, width = 8, height = 5)
        
        # Export upper canopy for possible display and for cowplot
        tmp <- dplyr::filter(happy,var_type == variable_types[aa], canopy_layer==1)
        x <- ggplot(data = tmp) +
          geom_boxplot(aes(x=pspread_levels,y=relative_change, group=pspread_levels),
                       fill = "Navajowhite2",
                       color="black", 
                       outlier.shape = NA) +
          facet_grid(.~world, labeller = labeller(world = world_file_yr)) +
          labs(title = fig_title_2[aa], x = "Intensity (I')", y = y_label[aa]) +
          scale_x_continuous(breaks = c(0.2, 0.8)) +
          theme_bw(base_size = 16) +
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
          NULL
        #plot(x)
        ggsave(paste(watershed,"_", variable_types[aa], ".pdf", sep=""), plot = x, 
               device = "pdf", path = output_path, width = 8, height = 3)
        figure_list[[aa]] <- x
      }
    }
  }
  
  # ---------------------------------------------------------------------
  # Cowplot time
  
  # Canopy IDs
  # 6018, 100000  HJA
  # 9445, 100000  P301
  # 40537         RS
  # 2777, 10000   SF
  
  # ----
  # Change stem carbon from daily to wy 
  stores_canopy <- stores_ts$cdg %>% 
    dplyr::group_by(wy, stratumID) %>% 
    dplyr::summarise(dead_stemc = mean(dead_stemc),
                     live_stemc =  mean(live_stemc),
                     leafc = mean(leafc),
                     leafc_store = mean(leafc_store)) %>% 
    dplyr::mutate(c_aboveground = dead_stemc + live_stemc + leafc + leafc_store) %>% 
    dplyr::mutate(store = if_else(stratumID==6018 | stratumID==9445 | stratumID==40537 | stratumID==2777,
                                  "upper",
                                  "lower")) %>% 
    dplyr::select(wy, store, c_aboveground) 
  
  # Change litter carbon from daily to wy 
  stores_litter <- stores_ts$bd %>% 
    dplyr::group_by(wy) %>% 
    dplyr::summarise(c_aboveground = mean(litrc)*1000) %>% 
    tibble::add_column(store = "litter")
  
  # Combine dataframes
  stores_final <- dplyr::bind_rows(stores_canopy, stores_litter)
  stores_final$wy <- stores_final$wy - min(stores_final$wy) + 1   # This is plotting stand age as x-axis
  
  # Make timeseries plot
  x_stores <- ggplot() +
    geom_line(data=stores_final, aes(x=wy,y=c_aboveground, group=as.factor(store), color=store, linetype=store), size = 1.2) +
    geom_vline(xintercept = stand_age, linetype=2, size=.4) +
    #geom_hline(yintercept = c(4,7), linetype=1, size=.4, color = "Navajowhite2") +
    labs(title = "", x = "Stand Age - Wateryear", y = expression('Carbon (g/m'^2*')')) +
    theme_bw(base_size = 16) + 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
    NULL
  
  if (watershed == "RS"){
    x_stores <- x_stores + scale_linetype_discrete(name="Abovegraound Carbon Stores", labels = c("Litter","Canopy")) +
      scale_color_brewer(palette = "Set2", name="Abovegraound Carbon Stores", labels = c("Litter","Canopy"))
  } else {
    x_stores <- x_stores + scale_linetype_discrete(name="Abovegraound Carbon Stores", labels = c("Litter","Lower\nCanopy","Upper\nCanopy")) +
      scale_color_brewer(palette = "Set2", name="Abovegraound Carbon Stores", labels = c("Litter","Lower\nCanopy","Upper\nCanopy"))
  }
      
  #plot(x_stores)
  ggsave(paste(watershed,"_timeseries.pdf", sep=""), plot = x_stores, 
         device = "pdf", path = output_path, width = 8, height = 3)

  # ----
  # Combine figures for Cowplot
  
  if (watershed == "RS" | watershed == "SF"){cow_label = c("a","b","c","d")}
  if (watershed == "P300" | watershed == "HJA"){cow_label = c("e","f","g","h")}
  
  x_4by1 <- cowplot::plot_grid(x_stores,
                               figure_list[[6]],
                               figure_list[[8]],
                               figure_list[[9]],
                               labels=cow_label,
                               nrow=4,
                               label_size = 14,
                               label_x = 0.95,
                               label_y = 1)
  
  cowplot::save_plot(file.path(output_path, paste("4by1_cowplot_",watershed,".pdf",sep="")),
                     x_4by1,
                     ncol=1,
                     nrow=4,
                     base_height=4,
                     base_width=8)
  
  return(x_4by1)
  # return(patch_canopy_diff)     # For use when searching for parameter set ranges 
}
  




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Call patch sensitivity evaluation function

# ---------------------------------------------------------------------
# HJA

out_hja <- patch_fire_eval(num_canopies = 2,
                           allsim_path = RHESSYS_ALLSIM_DIR_3.2_HJA,
                           initial_date = ymd("1957-10-01"),
                           parameter_file = RHESSYS_PAR_SIM_3.1_HJA,
                           watershed = "HJA",
                           output_path = OUTPUT_DIR_3,
                           cwdc_yes = FALSE
)


# ---------------------------------------------------------------------
# P301

out_p300 <- patch_fire_eval(num_canopies = 2,
                            allsim_path = RHESSYS_ALLSIM_DIR_3.2_P300,
                            initial_date = ymd("1941-10-01"),
                            parameter_file = RHESSYS_PAR_SIM_3.1_P300,
                            watershed = "P300",
                            output_path = OUTPUT_DIR_3,
                            cwdc_yes = FALSE
)


# ---------------------------------------------------------------------
# RS

out_rs <- patch_fire_eval(num_canopies = 1,
                          allsim_path = RHESSYS_ALLSIM_DIR_3.2_RS,
                          initial_date = ymd("1988-10-01"),
                          parameter_file = RHESSYS_PAR_SIM_3.1_RS,
                          watershed = "RS",
                          output_path = OUTPUT_DIR_3,
                          cwdc_yes = FALSE
)


# ---------------------------------------------------------------------
# SF

out_sf <- patch_fire_eval(num_canopies = 2,
                          allsim_path = RHESSYS_ALLSIM_DIR_3.2_SF,
                          initial_date = ymd("1955-10-01"),
                          parameter_file = RHESSYS_PAR_SIM_3.1_SF,
                          watershed = "SF",
                          output_path = OUTPUT_DIR_3,
                          cwdc_yes = FALSE
)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Cowplot multiple simulations together


# Combine figures for RS and P301
sim_sf_hja <- cowplot::plot_grid(out_rs,
                                 out_p300,
                                 labels= c("Rattlesnake", "P301"),
                                 ncol=2,
                                 label_size = 22,
                                 label_x = 0,
                                 label_y = 1,
                                 label_fontface = "plain"
)

cowplot::save_plot(file.path(OUTPUT_DIR_3, "sim_rs_p301.pdf"),
                   sim_sf_hja,
                   ncol=2,
                   nrow=1,
                   base_height=16,
                   base_width=8)


# Combine figures for SF and HJA
sim_sf_hja <- cowplot::plot_grid(out_sf,
                                 out_hja,
                                 labels= c("Santa Fe", "H.J. Andrews"),
                                 ncol=2,
                                 label_size = 22,
                                 label_x = 0,
                                 label_y = 1,
                                 label_fontface = "plain"
                                 )

cowplot::save_plot(file.path(OUTPUT_DIR_3, "sim_sf_hja.pdf"),
                   sim_sf_hja,
                   ncol=2,
                   nrow=1,
                   base_height=16,
                   base_width=8)





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


# Code to find ranges for some of the parameters in 3.1 
# Separates out relative change by intensity
# Change return value in patch_fire_eval to patch_canopy_diff

#HJA
ls(out)
unique(out$world)

x <- out %>% 
  dplyr::filter(var_type == "stemc", 
                world == "ws_hja/worldfiles/hja_2can_patch_6018.world.Y2098M10D1H1.state",
                canopy_layer == 1) %>% 
  ggplot(.) +
  geom_point(aes(
    #x=ws_hja.defs.veg_hja_7dougfir.def.understory_mort,
    #x=ws_hja.defs.veg_hja_7dougfir.def.consumption,
    #x=ws_hja.defs.veg_hja_7dougfir.def.overstory_mort_k1,
    x=ws_hja.defs.veg_hja_7dougfir.def.overstory_mort_k2,
    y=relative_change)) +
  facet_wrap(.~pspread_levels)
plot(x)


# ----
#P301
ls(out)
unique(out$world)

x <- out %>% 
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


# ----
#RS
ls(out)
unique(out$world)

x <- out %>% 
  dplyr::filter(var_type == "stemc", 
                world == "ws_rs/worldfiles/rs_30m_1can_patch_40537.world.Y2069M10D1H1.state",
                canopy_layer == 1) %>% 
  ggplot(.) +
  geom_point(aes(
    #x=ws_rs.defs.veg_rs_shrub.def.understory_mort,
    #x=ws_rs.defs.veg_rs_shrub.def.consumption,
    #x=ws_rs.defs.veg_rs_shrub.def.overstory_mort_k1,
    x=ws_rs.defs.veg_rs_shrub.def.overstory_mort_k2,
    y=relative_change)) +
  facet_wrap(.~pspread_levels)
plot(x)


# ----
#SF
ls(out)
unique(out$world)

x <- out %>% 
  dplyr::filter(var_type == "stemc", 
                world == "ws_sf/worldfiles/sf_2can_patch_2777.world.Y2036M10D1H1.state",
                canopy_layer == 1) %>% 
  ggplot(.) +
  geom_point(aes(
    #x=ws_sf.defs.veg_sf_1ponderosapine.def.understory_mort,
    #x=ws_sf.defs.veg_sf_1ponderosapine.def.consumption,
    #x=ws_sf.defs.veg_sf_1ponderosapine.def.overstory_mort_k1,
    x=ws_sf.defs.veg_sf_1ponderosapine.def.overstory_mort_k2,
    y=relative_change)) +
  facet_wrap(.~pspread_levels)
plot(x)




