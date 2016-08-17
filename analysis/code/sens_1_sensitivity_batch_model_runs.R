# Fire effects model sensitivity test - run model
# 
# Ryan Bart July 2016

source("code/sensitivity_batch_functions.R")


library("dplyr")





# ---------------------------------------------------------------------


# Set Directory - Yes, I know this is bad code. 
# Possibly reconfigure RHESSys code so that directory for C and R code is root directory
# Question is whether it makes sense to do calibrations from R - probably not
setwd("../../scripts")


# Parameters to search
thresh_over = seq(3.5,3.5,length.out = 1)           # default: 5
thresh_under = seq(3,3,length.out = 1)          # default: 3
pspread_loss_rel = seq(1,1,length.out = 1)      # default: 1
vapor_loss_rel = seq(1,1,length.out = 1)        # default: 1
biomass_loss_rel_k1 = seq(-10,-10,length.out = 1)       # default: ?
biomass_loss_rel_k2 = seq(.1,1,length.out = 10)           # default: ?
pspread =  seq(1,1,length.out = 1)
c_under = seq(1,1,length.out = 1)

fire_sensitivity = sensitivity_model_runs(thresh_over=thresh_over,thresh_under=thresh_under,vapor_loss_rel=vapor_loss_rel,pspread=pspread,biomass_loss_rel_k1=biomass_loss_rel_k1,biomass_loss_rel_k2=biomass_loss_rel_k2)
fire_sensitivity

# save output
write.csv(fire_sensitivity, file = "../post/fire_model_analysis/data_output/run_4.csv")

# Reset directory
setwd("../post/fire_model_analysis")



