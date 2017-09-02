# Patch Fire Sensitivity Analysis
#
# Contains scripts for evaluating parameter sensitivity of fire effects model

source("R/0.1_utilities.R")

theme_set(theme_bw(base_size = 11))


# ---------------------------------------------------------------------
# P300 Patch Fire data processing
# Computes differences in variables for several days before to several days after a fire.
# This will be updated when explicit fire effects output is implemented.


p300_patch_canopy <- readin_rhessys_output_cal(var_names = c("leafc", "stemc", "rootc"),
                                               path = ALLSIM_DIR_p300_2.1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_2.1.1_SOBOL,
                                               num_canopies = 2)
p300_patch_canopy_diff <- p300_patch_canopy %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_canopy)


p300_patch_ground <- readin_rhessys_output_cal(var_names = c("litrc", "soil1c"),
                                               path = ALLSIM_DIR_p300_2.1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_2.1.1_SOBOL,
                                               num_canopies = 1)
p300_patch_ground$value <- as.double(p300_patch_ground$value)
p300_patch_ground_diff <- p300_patch_ground %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_ground)


p300_patch_cwdc <- readin_rhessys_output_cal(var_names = c("cwdc"),
                                             path = ALLSIM_DIR_p300_2.1.1,
                                             initial_date = ymd("1941-10-01"),
                                             parameter_file = PARAMETER_FILE_p300_2.1.1_SOBOL,
                                             num_canopies = 2)
p300_patch_cwdc_diff <- p300_patch_cwdc %>%
  group_by(run, dates, var_type) %>%
  summarize(value = sum(value)) %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_cwdc)


p300_patch_height <- readin_rhessys_output_cal(var_names = c("height"),
                                               path = ALLSIM_DIR_p300_2.1.1,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = PARAMETER_FILE_p300_2.1.1_SOBOL,
                                               num_canopies = 2)
p300_patch_height_diff <- p300_patch_height %>%
  dplyr::filter(dates == ymd("1941-10-03") | dates == ymd("1941-10-11")) %>%
  spread(dates, value) %>%
  mutate(relative_change = ((`1941-10-11` - `1941-10-03`)/`1941-10-03`)*100, absolute_change = `1941-10-11` - `1941-10-03`)
rm(p300_patch_height)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Sensitivity analysis
# Generate y from above, and possibly substitute values for stand_age

# Overstory height relative loss
p300_patch_height_diff_1 <- dplyr::filter(p300_patch_height_diff, canopy_layer==1)
tell(sobol_model, p300_patch_height_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 

# Understory height relative loss
p300_patch_height_diff_2 <- dplyr::filter(p300_patch_height_diff, canopy_layer==2)
tell(sobol_model, p300_patch_height_diff_2$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# Overstory leaf relative loss
p300_patch_canopy_diff_1 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==1, var_type=="leafc")
tell(sobol_model, p300_patch_canopy_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 

# Understory leaf relative loss
p300_patch_canopy_diff_2 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==2, var_type=="leafc")
tell(sobol_model, p300_patch_canopy_diff_2$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# Overstory stem relative loss
p300_patch_canopy_diff_1 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==1, var_type=="stemc")
tell(sobol_model, p300_patch_canopy_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 

# Understory stem relative loss
p300_patch_canopy_diff_2 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==2, var_type=="stemc")
tell(sobol_model, p300_patch_canopy_diff_2$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# Overstory root relative loss
p300_patch_canopy_diff_1 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==1, var_type=="rootc")
tell(sobol_model, p300_patch_canopy_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 

# Understory stem relative loss
p300_patch_canopy_diff_2 <- dplyr::filter(p300_patch_canopy_diff, canopy_layer==2, var_type=="rootc")
tell(sobol_model, p300_patch_canopy_diff_2$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# CWD relative loss
tell(sobol_model, p300_patch_cwdc_diff$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# Litter relative loss
p300_patch_ground_diff_1 <- dplyr::filter(p300_patch_ground_diff, var_type=="litrc")
tell(sobol_model, p300_patch_ground_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 


# ----
# Soil relative loss
p300_patch_ground_diff_1 <- dplyr::filter(p300_patch_ground_diff, var_type=="soil1c")
tell(sobol_model, p300_patch_ground_diff_1$relative_change) 
print(sobol_model) 
plot(sobol_model) 



