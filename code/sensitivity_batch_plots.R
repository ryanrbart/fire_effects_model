# Fire effects model - Analysis of batch sensitvity runs

library("dplyr")
library("ggplot2")


# Import RHESSys output

fire_sen = read.csv(file = "data_output/fire_sensitivity.csv", header = T)

ls(fire_sen)


ggplot(data = fire_sen) +
  geom_line(aes(x=pspread,y=delta_height_u), color="blue") +
  geom_line(aes(x=pspread,y=delta_height_o), color="black") +
  facet_grid(thresh_o ~ thresh_u, labeller = label_both)




