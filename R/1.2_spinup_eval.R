# Evaluation of spinup


# ---------------------------------------------------------------------
# Functions





# ---------------------------------------------------------------------
# P300



p300_spinup = readin_rhessys_output("ws_p300/out/spinup/spinup", b=1, g=1, c=1)

ls(p300_spinup$bd)

ggplot(data=p300_spinup$bd) + 
  geom_line(aes(x=date,y=height), color="blue") 




ls(p300_spinup$cd)

ls(p300_spinup$cdg)

variable = p300_spinup$cd$lai
dates = p300_spinup$cd$date

two_can = separate_canopies(variable, dates, 2)


ggplot(data = two_can) +
  geom_line(aes(x=dates,y=var, linetype=as.character(names)))


# ---------------------------------------------------------------------
# Rattlesnake








