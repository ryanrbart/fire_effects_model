# Fire effects model sensitivity test - run model
# Ryan Bart July 2016

source("analysis/code/sens_0_functions.R")

# ---------------------------------------------------------------------
# Run 1

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
write.csv(fire_sensitivity, file = "results/sens/run_1.csv")

# ---------------------------------------------------------------------
# Run 2

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
write.csv(fire_sensitivity, file = "results/sens/run_2.csv")

# ---------------------------------------------------------------------
# Run 3

# Parameters to search
thresh_over = seq(3.5,3.5,length.out = 1)           # default: 5
thresh_under = seq(3,3,length.out = 1)          # default: 3
pspread_loss_rel = seq(1,1,length.out = 1)      # default: 1
vapor_loss_rel = seq(1,1,length.out = 1)        # default: 1
biomass_loss_rel_k1 = seq(-5,-15,length.out = 3)       # default: ?
biomass_loss_rel_k2 = seq(.1,1,length.out = 5)           # default: ?
pspread =  seq(0,1,length.out = 6)
c_under = seq(1,1,length.out = 1)

fire_sensitivity = sensitivity_model_runs(thresh_over=thresh_over,thresh_under=thresh_under,vapor_loss_rel=vapor_loss_rel,pspread=pspread,biomass_loss_rel_k1=biomass_loss_rel_k1,biomass_loss_rel_k2=biomass_loss_rel_k2)
fire_sensitivity

# save output
write.csv(fire_sensitivity, file = "results/sens/run_3.csv")



# ---------------------------------------------------------------------
# Two canopy - Run 1

# Parameters to search
cover_fraction = seq(0,1,length.out = 11)           # default: 1
gap_fraction = seq(0,0,length.out = 1)          # default: 1

two_canopy = two_canopy_cf_gf(cover_fraction=cover_fraction,gap_fraction=gap_fraction)
two_canopy = as.data.frame(two_canopy)
colnames(two_canopy)[1] = "V1"


# save output
write.csv(two_canopy, file = "results/two_canopy/run_1.csv")







