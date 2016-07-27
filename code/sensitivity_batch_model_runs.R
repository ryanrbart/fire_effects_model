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
thresh_under = seq(1,5,length.out = 5)
thresh_over = seq(3,7,length.out = 5)
pspread =  seq(0,1,length.out = 11)
c_under = seq(1,1,length.out = 1)
scen = length(thresh_under) * length(thresh_over) * length(pspread) * length(c_under)


fire_sensitivity = as.data.frame(matrix(nrow=scen, ncol=12))
colnames(fire_sensitivity) = c("thresh_u","thresh_o","pspread","c_under","delta_lai_u", "delta_lai_o","delta_height_u","delta_height_o","delta_cwdc_u","delta_cwdc_o","delta_litrc","delta_soil1c")
count=1

for (aa in seq_along(thresh_under)){
for (bb in seq_along(thresh_over)){
for (cc in seq_along(pspread)){
for (dd in seq_along(c_under)){
  
  # Modify def files
  awk1 = sprintf("awk -f ../awks/change.def.understory_height_thresh.awk par=%f < ../defs/veg_p301_conifer_cec.def > ../defs/veg.tmp1",thresh_under[aa])
  system(awk1)
  awk2 = sprintf("awk -f ../awks/change.def.overstory_height_thresh.awk par=%f < ../defs/veg.tmp1 > ../defs/veg.tmp2",thresh_over[bb])
  system(awk2)
  
  # Modify dated sequence
  dseq1 = sprintf("awk -f ../awks/change.datedseq.pspread.awk par=%f < ../clim/lowProv.pspread_template > ../clim/lowProv.pspread",pspread[cc])
  system(dseq1)
  
  # Modify worldfile
  
  
  # Run RHESSys
  happy = sprintf("/Users/ryanrbart/bin/rhessys5.20.fire_off -t ../tecfiles/tec.p301_sim -w ../worldfiles/world.p301_patch_2canopy_dated_seq -whdr ../worldfiles/world.p301_patch_2canopy_dated_seq.hdr   -r ../flowtables/flow.patch  -st 1945 10 1 1 -ed 1945 10 7 1 -pre ../out/cal5/cal5 -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -g -p")
  system(happy)
  out = readin_rhessys_output("../out/cal5/cal5", c=1,g=1,p=1)

  
  # Derive metrics
  lai_o_dif = out$cd$lai[7]/out$cd$lai[5]
  lai_u_dif = out$cd$lai[8]/out$cd$lai[6]
  height_o_dif = out$cd$height[7]/out$cd$height[5]
  height_u_dif = out$cd$height[8]/out$cd$height[6]
  cwdc_o_dif = 0 #out$cd$cwdc[5]/out$cd$cwdc[3]
  cwdc_u_dif = 0 #out$cd$cwdc[6]/out$cd$cwdc[4]
  litrc_dif = out$bd$litrc[3]/out$bd$litrc[2]
  soil1c_dif = out$pdg$soil1c[3]/out$pdg$soil1c[2]
  
  fire_sensitivity$thresh_u[count] = thresh_under[aa]
  fire_sensitivity$thresh_o[count] = thresh_over[bb]
  fire_sensitivity$pspread[count] = pspread[cc]
  fire_sensitivity$c_under[count] = c_under[dd]
  fire_sensitivity$delta_lai_u[count] = lai_u_dif
  fire_sensitivity$delta_lai_o[count] = lai_o_dif
  fire_sensitivity$delta_height_u[count] = height_u_dif
  fire_sensitivity$delta_height_o[count] = height_o_dif
  fire_sensitivity$delta_cwdc_u[count] = cwdc_u_dif
  fire_sensitivity$delta_cwdc_o[count] = cwdc_o_dif
  fire_sensitivity$delta_litrc[count] = litrc_dif
  fire_sensitivity$delta_soil1c[count] = soil1c_dif
  
  
  #plot(out$cd$height~out$cd$day, main = paste("thresh_under =",height_under[aa],", pspread =",pspread[cc]), ylim=c(0,6))
  count = count + 1
}  
} 
}
}

#print(fire_sensitivity)

# save output
write.csv(fire_sensitivity, file = "../post/fire_model_analysis/data_output/fire_sensitivity.csv")


# Reset directory
setwd("../post/fire_model_analysis")
getwd()





