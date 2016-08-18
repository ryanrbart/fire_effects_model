# Fire effects model sensitivity test - plot results
# Ryan Bart July 2016

library("dplyr")
library("ggplot2")

# ---------------------------------------------------------------------


# Investigate relation between pspread and canopy height thresholds

fire_sen = read.csv(file = "analysis/output/sens/run_1.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=pspread,y=delta_height_u), color="blue") +
  geom_line(aes(x=pspread,y=delta_height_o), color="black") +
  facet_grid(thresh_o ~ thresh_u, labeller = label_both)




# Investigate pspread_loss_rel parmeter

fire_sen = read.csv(file = "analysis/output/sens/run_2.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=pspread_loss_rel,y=delta_lai_u), color="blue") +
  facet_grid(. ~ pspread)




# Investigate vapor_loss_rel parmeter

fire_sen = read.csv(file = "analysis/output/sens/run_3.csv", header = T)

ls(fire_sen)

ggplot(data = fire_sen) +
  geom_line(aes(x=vapor_loss_rel,y=delta_litrc), color="blue") +
  facet_grid(. ~ pspread)


