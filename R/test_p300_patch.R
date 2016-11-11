# Interactive session for P300-patch

library(rhessysR)

# ---------------------------------------------------------------------
# Various p300 patches

# -c 1 181 9445 9445
# -c 1 36 6973 6973
# -c 1 232 16064 16064

# ---------------------------------------------------------------------

# RHESSys Inputs
rhessys_version <- "bin/rhessys5.20.fire_off"
tec_file <- "ws_p300/tecfiles/tec.p300_sim"
world_file <- "ws_p300/worldfiles/world.p300_30m_2can_patch_9445"
world_hdr_file <- "ws_p300/worldfiles/world.p300_30m_2can_spinup.hdr"
flow_file <- "ws_p300/flowtables/flow.p300_30m_patch_9445"
start_date <- "1941 10 1 1"
end_date <- "2341 10 1 1"
output_folder <- "ws_p300/out/test_p300_patch/"     # Must end with '/'
output_filename <- "patch"
command_options <- "-b -g -c -p"
parameter_type <- "all_combinations"
m <- c(1.792761)
k <- c(1.566492)
m_v <- c(1.792761)
k_v <- c(1.566492)
pa <- c(7.896941)
po <- c(1.179359)
gw1 <- c(0.168035)
gw2 <- c(0.178753)


# List of lists containing parameters, awk_file, input_file, output_file
#parameter_change_list <- NULL
parameter_change_list <- list()
parameter_change_list[[1]] <- list(c(0.1),"awks/change.def.epc.livewood_turnover.awk",
                                "ws_p300/defs/veg_p300_shrub.def", "ws_p300/defs/veg_p300_shrub.tmp1")
parameter_change_list[[2]] <- list(c(0.9),"awks/change.def.epc.alloc_livewoodc_woodc.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp1", "ws_p300/defs/veg_p300_shrub.tmp2")
parameter_change_list[[3]] <- list(c(0.05),"awks/change.def.epc.branch_turnover.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp2", "ws_p300/defs/veg_p300_shrub.tmp3")
parameter_change_list[[4]] <- list(c(6),"awks/change.def.overstory_height_thresh.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp3", "ws_p300/defs/veg_p300_shrub.tmp4")
parameter_change_list[[5]] <- list(c(4),"awks/change.def.understory_height_thresh.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp4", "ws_p300/defs/veg_p300_shrub.tmp5")
parameter_change_list[[6]] <- list(c(.2),"awks/change.def.pspread_loss_rel.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp5", "ws_p300/defs/veg_p300_shrub.tmp6")
parameter_change_list[[7]] <- list(c(.2),"awks/change.def.vapor_loss_rel.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp6", "ws_p300/defs/veg_p300_shrub.tmp7")
parameter_change_list[[8]] <- list(c(-0.05),"awks/change.def.biomass_loss_rel_k1.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp7", "ws_p300/defs/veg_p300_shrub.tmp8")
parameter_change_list[[9]] <- list(c(0.25),"awks/change.def.biomass_loss_rel_k2.awk",
                                "ws_p300/defs/veg_p300_shrub.tmp8", "ws_p300/defs/veg_p300_shrub.tmp_final")


# Make tec-file
#tec_data <- NULL
tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
#tec_data[3,] <- data.frame(1943, 03, 31, 1, "output_current_state", stringsAsFactors=FALSE)
#tec_data[4,] <- data.frame(1949, 03, 31, 1, "output_current_state", stringsAsFactors=FALSE)


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
#output_variables <- NULL
output_variables <- list()
output_variables[[1]] <- list("lai", "awks/extlai.awk","patch_basin.daily")
output_variables[[2]] <- list("et", "awks/extet.awk","patch_basin.daily")

# ---------------------------------------------------------------------

run_rhessys(rhessys_version, tec_file = tec_file, world_file = world_file, 
            world_hdr_file = world_hdr_file, flow_file = flow_file, 
            start_date = start_date, end_date = end_date, 
            output_folder = output_folder, output_filename = output_filename, 
            command_options = command_options, parameter_type = parameter_type, 
            m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, 
            gw1 = gw1, gw2 = gw2, parameter_change_list = parameter_change_list,  
            tec_data = tec_data, output_variables = output_variables)





# ---------------------------------------------------------------------


p300_patch = readin_rhessys_output("ws_p300/out/test_p300_patch/patch", b=1, g=1, c=1, p=1)
beep(4)

bd = p300_patch$bd
bdg = p300_patch$bdg
pd = p300_patch$pd
pdg = p300_patch$pdg
cd = separate_canopy_output(p300_patch$cd, 2)
cdg = separate_canopy_output(p300_patch$cdg, 2)

#ls(bd)
#ls(bdg)
#ls(pd)
#ls(pdg)
#ls(cd)
#ls(cdg)


#cd_2 = dplyr::filter(cd, names==2)
p300_spinup_height <- ggplot(data = cd) +
  geom_line(aes(x=date,y=height, linetype=as.character(names)))
plot(p300_spinup_height)
#ggsave("p300_spinup_height",p300_spinup_height, path = SPINUP_DIR)

p300_spinup_lai <- ggplot(data = cd) +
  geom_line(aes(x=date,y=lai, linetype=as.character(names)))
plot(p300_spinup_lai)
#ggsave("p300_spinup_lai",p300_spinup_lai, path = SPINUP_DIR)

p300_spinup_npool <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=npool, linetype=as.character(names)))
plot(p300_spinup_npool)
#ggsave("p300_spinup_npool",p300_spinup_npool, path = SPINUP_DIR)

p300_spinup_leafc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=leafc, linetype=as.character(names)))
plot(p300_spinup_leafc)
#ggsave("p300_spinup_leafc",p300_spinup_leafc, path = SPINUP_DIR)

p300_spinup_livestemc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_stemc, linetype=as.character(names)))
plot(p300_spinup_livestemc)
#ggsave("p300_spinup_livestemc",p300_spinup_livestemc, path = SPINUP_DIR)

p300_spinup_deadstemc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=dead_stemc, linetype=as.character(names)))
plot(p300_spinup_deadstemc)
#ggsave("p300_spinup_deadstemc",p300_spinup_deadstemc, path = SPINUP_DIR)

p300_spinup_frootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=frootc, linetype=as.character(names)))
plot(p300_spinup_frootc)
#ggsave("p300_spinup_frootc",p300_spinup_frootc, path = SPINUP_DIR)

p300_spinup_livecrootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_crootc, linetype=as.character(names)))
plot(p300_spinup_livecrootc)
#ggsave("p300_spinup_livecrootc",p300_spinup_livecrootc, path = SPINUP_DIR)

p300_spinup_deadcrootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_crootc, linetype=as.character(names)))
plot(p300_spinup_deadcrootc)
#ggsave("p300_spinup_deadcrootc",p300_spinup_deadcrootc, path = SPINUP_DIR)

p300_spinup_cpool <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=cpool, linetype=as.character(names)))
plot(p300_spinup_cpool)
#ggsave("p300_spinup_cpool",p300_spinup_cpool, path = SPINUP_DIR)

# -------

p300_spinup_soil1c <- ggplot(data = pdg) +
  geom_line(aes(x=date,y=soil1c))
plot(p300_spinup_soil1c)
#ggsave("p300_spinup_soil1c",p300_spinup_soil1c, path = SPINUP_DIR)

p300_spinup_litr1c <- ggplot(data = pdg) +
  geom_line(aes(x=date,y=litr1c))
plot(p300_spinup_litr1c)
#ggsave("p300_spinup_litr1c",p300_spinup_litr1c, path = SPINUP_DIR)


# -------

p300_spinup_npool <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=npool, linetype=as.character(names)))
plot(p300_spinup_npool)
#ggsave("p300_spinup_npool",p300_spinup_npool, path = SPINUP_DIR)


# -------

cdg_2 = dplyr::filter(cdg, names==2)
p300_spinup_mresp <- ggplot(data = cdg_2) +
  geom_line(aes(x=date,y=npool, linetype=as.character(names)))
plot(p300_spinup_mresp)
#ggsave("p300_spinup_mresp",p300_spinup_mresp, path = SPINUP_DIR)




# ---------------------------------------------------------------------
# Fire Analysis


fire_pd = dplyr::filter(pd, date > "2000-09-29" & date < "2000-10-2")
fire_pdg = dplyr::filter(pdg, date > "2000-09-29" & date < "2000-10-2")

fire_cd = dplyr::filter(cd, date > "2000-09-29" & date < "2000-10-2")
fire_cdg = dplyr::filter(cdg, date > "2000-09-29" & date < "2000-10-2")


# Carbon balance

fire_diff_u = function(x) x[2]-x[1]
fire_diff_l = function(x) x[4]-x[3]

# Initial litter removed by fire, then more created by vegetation mortality


sum(
fire_diff_u(fire_pdg$litr1c),
fire_diff_u(fire_pdg$litr2c),
fire_diff_u(fire_pdg$litr3c),
fire_diff_u(fire_pdg$litr4c))*1000

fire_diff_u(fire_cdg$cwdc)
fire_diff_l(fire_cdg$cwdc)



sum(
fire_diff_u(fire_cdg$leafc),
fire_diff_u(fire_cdg$live_stemc),
fire_diff_u(fire_cdg$dead_stemc),
fire_diff_u(fire_cdg$frootc),
fire_diff_u(fire_cdg$live_crootc),
fire_diff_u(fire_cdg$dead_crootc))


sum(
fire_diff_l(fire_cdg$leafc),
fire_diff_l(fire_cdg$live_stemc),
fire_diff_l(fire_cdg$dead_stemc),
fire_diff_l(fire_cdg$frootc),
fire_diff_l(fire_cdg$live_crootc),
fire_diff_l(fire_cdg$dead_crootc))

fire_diff_u(fire_cdg$cpool)*1000
fire_diff_l(fire_cdg$leafc_store)
fire_diff_l(fire_cdg$dead_leafc)




