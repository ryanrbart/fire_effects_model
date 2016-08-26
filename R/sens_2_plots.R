# Fire effects model sensitivity test - plot results
# Ryan Bart July 2016

library("dplyr")
library("tidyr")
library("ggplot2")

# ---------------------------------------------------------------------


# Investigate relation between pspread and canopy height thresholds

fire_sen = read.csv(file = "results/sens/run_1.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=pspread,y=delta_height_u), color="blue") +
  geom_line(aes(x=pspread,y=delta_height_o), color="black") +
  facet_grid(thresh_o ~ thresh_u, labeller = label_both)

ggplot(data = fire_sen) +
  geom_line(aes(x=pspread,y=delta_lai_u), color="blue") +
  geom_line(aes(x=pspread,y=delta_lai_o), color="black") +
  facet_grid(thresh_o ~ thresh_u, labeller = label_both)




# Investigate pspread_loss_rel parmeter

fire_sen = read.csv(file = "results/sens/run_2.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=pspread_loss_rel,y=delta_lai_u), color="blue") +
  facet_grid(. ~ pspread)




# Investigate vapor_loss_rel parmeter

fire_sen = read.csv(file = "results/sens/run_3.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=biomass_loss_rel_k2,y=delta_lai_o), color="blue") +
  facet_grid(pspread~biomass_loss_rel_k1)









# ---------------------------------------------------------------------

# Investigate two canopies

two_canopy = read.csv(file = "results/two_canopy/run_1.csv", header = T)

ls(two_canopy)

upper = two_canopy[seq(1, length(two_canopy$V1), 2),]
lower = two_canopy[seq(2, length(two_canopy$V1), 2),]

fake_date = seq(1,length(upper$V1))
upper = cbind(upper, fake_date)
lower = cbind(lower, fake_date)

upper = gather(upper, "param", "value", 1:11)
lower = gather(lower, "param", "value", 1:11)



name = rep("upper",length(upper$param))
upper = cbind(upper, name)

name = rep("lower",length(lower$param))
lower = cbind(lower, name)

runs = rbind(upper, lower)

ggplot(data = runs) +
  geom_line(aes(x=fake_date,y=value, color=name)) +
  facet_wrap(~param)

