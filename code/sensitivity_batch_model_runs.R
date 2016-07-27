# Fire effects model - Calls RHESSys for sensitivity runs 

# Libraries
library("dplyr")


# Set Directory - Yes, I know this is bad code. 
# Possibly reconfigure RHESSys code so that directory for C and R code is root directory
# Question is whether it makes sense to do calibrations from R - probably not
setwd("../../scripts")
getwd()




# Make awk scripts
# 

# Parameters to search
height_under = seq(1,5,length.out = 11)
height_over = seq(7,7,length.out = 1)
pspread =  seq(0,1,length.out = 11)
c_under = seq(1,5,length.out = 11)

for (aa in seq_along(height_under)){
for (bb in seq_along(height_over)){
for (cc in seq_along(pspread)){
  
  # Modify def files
  awk1 = sprintf("awk -f ../awks/change.def.understory_height_thresh.awk par=%f < ../defs/veg_p301_conifer_cec.def > ../defs/veg.tmp1",height_under[aa])
  system(awk1)
  awk2 = sprintf("awk -f ../awks/change.def.overstory_height_thresh.awk par=%f < ../defs/veg.tmp1 > ../defs/veg.tmp2",height_over[bb])
  system(awk2)
  
  # Modify dated sequence
  dseq1 = sprintf("awk -f ../awks/change.datedseq.pspread.awk par=%f < ../clim/lowProv.pspread_template > ../clim/lowProv.pspread",pspread[cc])
  system(dseq1)
  
  # Modify worldfile
  
  
  # Run RHESSys
  happy = sprintf("/Users/ryanrbart/bin/rhessys5.20.fire_off -t ../tecfiles/tec.p301_sim -w ../worldfiles/world.p301_patch_2canopy_dated_seq -whdr ../worldfiles/world.p301_patch_2canopy_dated_seq.hdr   -r ../flowtables/flow.patch  -st 1945 10 1 1 -ed 1945 10 7 1 -pre ../out/cal5/cal5 -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -g -p")
  system(happy)
  a = readin_rhessys_output("../out/cal5/cal5", c=1,g=1,p=1)

  plot(a$cd$height~a$cd$day, main = paste("thresh_u",height_under[aa],", pspread",pspread[cc]), ylim=c(0,6))
  
}  
} 
}









#system("source cal5.txt")
setwd("../post/fire_model_analysis")
getwd()





