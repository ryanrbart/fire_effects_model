# Fire effects model sensitivity test - functions
# Called by sens_1_runs.R
# Ryan Bart July 2016


# ---------------------------------------------------------------------


sensitivity_model_runs = function(thresh_over=5,thresh_under=3,pspread_loss_rel=1,vapor_loss_rel=1,biomass_loss_rel_k1=-0.02,biomass_loss_rel_k2=200,pspread=.5,c_under=.1){
    
  scen = length(thresh_over) * length(thresh_under) * length(pspread_loss_rel) * length(vapor_loss_rel) *
    length(biomass_loss_rel_k1) * length(biomass_loss_rel_k2) * length(pspread) * length(c_under)
  
  fire_sensitivity = as.data.frame(matrix(nrow=scen, ncol=16))
  colnames(fire_sensitivity) = c("thresh_over","thresh_under", "pspread_loss_rel","vapor_loss_rel","biomass_loss_rel_k1","biomass_loss_rel_k2","pspread","c_under","delta_lai_u", "delta_lai_o","delta_height_u","delta_height_o","delta_cwdc_u","delta_cwdc_o","delta_litrc","delta_soil1c")
  count=1
  
  # Step though def file changes
  for (aa in seq_along(thresh_over)){
    for (bb in seq_along(thresh_under)){
      for (cc in seq_along(pspread_loss_rel)){
        for (dd in seq_along(vapor_loss_rel)){
          for (ee in seq_along(biomass_loss_rel_k1)){
            for (ff in seq_along(biomass_loss_rel_k2)){
              
              # Step though dated-sequence changes
              for (mm in seq_along(pspread)){
                
                # Step though worldfile changes
                for (rr in seq_along(c_under)){
                  
                  # Modify def files
                  def.awk = sprintf("awk -f awks/change.def.overstory_height_thresh.awk par=%f < defs/veg_p301_conifer_cec.def > defs/veg1.tmp",thresh_over[aa])
                  system(def.awk)
                  def.awk = sprintf("awk -f awks/change.def.understory_height_thresh.awk par=%f < defs/veg1.tmp > defs/veg2.tmp",thresh_under[bb])
                  system(def.awk)
                  def.awk = sprintf("awk -f awks/change.def.pspread_loss_rel.awk par=%f < defs/veg2.tmp > defs/veg3.tmp",pspread_loss_rel[cc])
                  system(def.awk)
                  def.awk = sprintf("awk -f awks/change.def.vapor_loss_rel.awk par=%f < defs/veg3.tmp > defs/veg4.tmp",vapor_loss_rel[dd])
                  system(def.awk)
                  def.awk = sprintf("awk -f awks/change.def.biomass_loss_rel_k1.awk par=%f < defs/veg4.tmp > defs/veg5.tmp",biomass_loss_rel_k1[ee])
                  system(def.awk)
                  def.awk = sprintf("awk -f awks/change.def.biomass_loss_rel_k2.awk par=%f < defs/veg5.tmp > defs/veg.tmp",biomass_loss_rel_k2[ff])
                  system(def.awk)
                  
                  # Modify dated sequence
                  dseq.awk = sprintf("awk -f awks/change.datedseq.pspread.awk par=%f < clim/lowProv.pspread_template > clim/lowProv.pspread",pspread[mm])
                  system(dseq.awk)
                  
                  # Modify worldfile
                  #  world.awk = sprintf("awk -f awks/change.world.c_under.awk par=%f < defs/veg_p301_conifer_cec.def > defs/veg.tmp",thresh_under[aa])
                  #  system(world.awk)
                  
                  # -----------
                  # Run RHESSys
                  happy = sprintf("/Users/ryanrbart/bin/rhessys5.20.fire_off -t tecfiles/tec.p301_sim -w worldfiles/world.p301_patch_2canopy_dated_seq -whdr worldfiles/world.p301_patch_2canopy_dated_seq.hdr   -r flowtables/flow.patch  -st 1945 10 1 1 -ed 1945 10 7 1 -pre out/cal5/cal5 -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -g -p")
                  system(happy)
                  out = readin_rhessys_output("out/cal5/cal5", c=1,g=1,p=1)
                  
                  
                  # Derive metrics
                  lai_o_dif = (out$cd$lai[7]-out$cd$lai[5])/out$cd$lai[5]
                  lai_u_dif = (out$cd$lai[8]-out$cd$lai[6])/out$cd$lai[6]
                  height_o_dif = (out$cd$height[7]-out$cd$height[5])/out$cd$height[5]
                  height_u_dif = (out$cd$height[8]-out$cd$height[6])/out$cd$height[6]
                  cwdc_o_dif = 0 #(out$cd$cwdc[5]-out$cd$cwdc[3])/out$cd$cwdc[3]
                  cwdc_u_dif = 0 #(out$cd$cwdc[6]-out$cd$cwdc[4])/out$cd$cwdc[4]
                  litrc_dif = (out$bd$litrc[3]-out$bd$litrc[2])/out$bd$litrc[2]
                  soil1c_dif = (out$pdg$soil1c[3]-out$pdg$soil1c[2])/out$pdg$soil1c[2]
                  
                  fire_sensitivity$thresh_over[count] = thresh_over[aa]
                  fire_sensitivity$thresh_under[count] = thresh_under[bb]
                  fire_sensitivity$pspread_loss_rel[count] = pspread_loss_rel[cc]
                  fire_sensitivity$vapor_loss_rel[count] = vapor_loss_rel[dd]
                  fire_sensitivity$biomass_loss_rel_k1[count] = biomass_loss_rel_k1[ee]
                  fire_sensitivity$biomass_loss_rel_k2[count] = biomass_loss_rel_k2[ff]
                  fire_sensitivity$pspread[count] = pspread[mm]
                  fire_sensitivity$c_under[count] = c_under[rr]
                  fire_sensitivity$delta_lai_u[count] = lai_u_dif
                  fire_sensitivity$delta_lai_o[count] = lai_o_dif
                  fire_sensitivity$delta_height_u[count] = height_u_dif
                  fire_sensitivity$delta_height_o[count] = height_o_dif
                  fire_sensitivity$delta_cwdc_u[count] = cwdc_u_dif
                  fire_sensitivity$delta_cwdc_o[count] = cwdc_o_dif
                  fire_sensitivity$delta_litrc[count] = litrc_dif
                  fire_sensitivity$delta_soil1c[count] = soil1c_dif
                  
                  count = count + 1
                }  
              } 
            }
          }
        }
      }
    }
  }
  
  return(fire_sensitivity)
} # End function




# ---------------------------------------------------------------------


two_canopy_cf_gf = function(cover_fraction=1,gap_fraction=1){
  
  scen = length(cover_fraction) * length(gap_fraction)

  two_canopy_param = as.data.frame(matrix(nrow=scen, ncol=2))
  colnames(two_canopy_param) = c("cover_fraction","gap_fraction")
  two_canopy = vector()

  count=1
  
  # Step though def file changes
  for (aa in seq_along(cover_fraction)){
    for (bb in seq_along(gap_fraction)){
#      for (cc in seq_along(pspread_loss_rel)){
#        for (dd in seq_along(vapor_loss_rel)){

          # Modify def files
          
          # Modify dated sequence

          # Modify worldfile
          world.awk = sprintf("awk -f awks/change.world.cf.awk par=%f < worldfiles/world.p301_patch_2canopy > worldfiles/world1.tmp",cover_fraction[aa])
          system(world.awk)
          world.awk = sprintf("awk -f awks/change.world.gf.awk par=%f < worldfiles/world1.tmp > worldfiles/world2.tmp",gap_fraction[bb])
          system(world.awk)
          
          # -----------
          # Run RHESSys
          happy = sprintf("/Users/ryanrbart/bin/rhessys5.20.fire_off -t tecfiles/tec.p301_sim -w worldfiles/world2.tmp -whdr worldfiles/world.p301_patch_2canopy.hdr   -r flowtables/flow.patch  -st 1941 10 1 1 -ed 2000 10 1 1 -pre out/cal3/cal3 -s 1.792761 1.566492 -sv 1.792761 1.566492 -svalt 7.896941 1.179359 -gw 0.168035 0.178753 -b -c -g -p")
          system(happy)
          out = readin_rhessys_output("out/cal3/cal3", c=1,g=1,p=1)
          
          
          # Derive metrics
          
          two_canopy_param$cover_fraction[count] = cover_fraction[aa]
          two_canopy_param$gap_fraction[count] = gap_fraction[bb]
          if (count==1){
            two_canopy = out$cd$lai
          } else {
            two_canopy = cbind(two_canopy, out$cd$lai)
          }
          count = count + 1

#        }
#      }
    }
  }
  
  return(two_canopy)
} # End function



