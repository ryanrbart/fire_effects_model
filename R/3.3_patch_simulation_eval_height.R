# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation


theme_set(theme_bw(base_size = 12))


# ---------------------------------------------------------------------
# Functions



# ---------------------------------------------------------------------
# Input and output paths 

#P300_PATCH_SIM <- "ws_p300/out/3.1_p300_patch_simulation/patch_sim"
P300_PATCH_SIM_SIMILAR <- "ws_p300/out/3.1_p300_patch_simulation/patch_sim_s"  #delete
P300_PATCH_SIM_DIFFERENT <- "ws_p300/out/3.1_p300_patch_simulation/patch_sim_d"  #delete

OUTPUT_DIR <- "outputs"
PATCH_SIM_DIR <- file.path(OUTPUT_DIR, "3_patch_sim")



# ---------------------------------------------------------------------
# P300 Patch Simulation Output

#p300_patch_sim <- readin_rhessys_output(P300_PATCH_SIM, b=1, g=1, c=1, p=1)
p300_patch_sim_similar <- readin_rhessys_output(P300_PATCH_SIM_SIMILAR, b=1, g=1, c=1, p=1) #delete
p300_patch_sim_different <- readin_rhessys_output(P300_PATCH_SIM_DIFFERENT, b=1, g=1, c=1, p=1) #delete


# ---------------------------------------------------------------------
# P300 Patch Simulation Output

#p300_patch_sim_similar
#p300_patch_sim_different


tmp1 <- cbind(p300_patch_sim_similar$bd, run_type = rep("similar", length(p300_patch_sim_similar$bd[,1])))
tmp2 <- cbind(p300_patch_sim_different$bd, run_type = rep("different", length(p300_patch_sim_different$bd[,1])))
bd_total <- rbind(tmp1, tmp2)

tmp1 <- cbind(p300_patch_sim_similar$bdg, run_type = rep("similar", length(p300_patch_sim_similar$bdg[,1])))
tmp2 <- cbind(p300_patch_sim_different$bdg, run_type = rep("different", length(p300_patch_sim_different$bdg[,1])))
bdg_total <- rbind(tmp1, tmp2)


tmp1 <- cbind(p300_patch_sim_similar$pd, run_type = rep("similar", length(p300_patch_sim_similar$pd[,1])))
tmp2 <- cbind(p300_patch_sim_different$pd, run_type = rep("different", length(p300_patch_sim_different$pd[,1])))
pd_total <- rbind(tmp1, tmp2)

tmp1 <- cbind(p300_patch_sim_similar$pdg, run_type = rep("similar", length(p300_patch_sim_similar$pdg[,1])))
tmp2 <- cbind(p300_patch_sim_different$pdg, run_type = rep("different", length(p300_patch_sim_different$pdg[,1])))
pdg_total <- rbind(tmp1, tmp2)


tmp1 <- separate_canopy_output(p300_patch_sim_similar$cd, 2)
tmp2 <- separate_canopy_output(p300_patch_sim_different$cd, 2)
tmp3 <- cbind(tmp1, run_type = as.character(rep("similar", length(tmp1[,1]))))
tmp4 <- cbind(tmp2, run_type = as.character(rep("different", length(tmp2[,1]))))
cd_total <- rbind(tmp3, tmp4)
cd_total$names <- as.character(cd_total$names)
cd_total$wy <- as.integer(cd_total$wy)

tmp1 <- separate_canopy_output(p300_patch_sim_similar$cdg, 2)
tmp2 <- separate_canopy_output(p300_patch_sim_different$cdg, 2)
tmp3 <- cbind(tmp1, run_type = as.character(rep("similar", length(tmp1[,1]))))
tmp4 <- cbind(tmp2, run_type = as.character(rep("different", length(tmp2[,1]))))
cdg_total <- rbind(tmp3, tmp4)
cdg_total$names <- as.character(cdg_total$names)
cdg_total$wy <- as.integer(cdg_total$wy)

#ls(bd_total)
#ls(bdg_total)
#ls(pd_total)
#ls(pdg_total)
#ls(cd_total)
#ls(cdg_total)




# Annual plots

x <- cdg_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_leafc = mean(leafc)) %>% 
  ggplot() +
    geom_line(aes(x = wy, y = avg_leafc, color=names, linetype=run_type)) +
#    xlim(1941,1952) +
#    ylim(0,5) +
    labs(title = "Leaf Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)
#ggsave("p300_c_frac_layer_1_3_3.pdf",plot = x, path = PATCH_SIM_DIR)

x <- cdg_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_live_stemc = mean(live_stemc)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_live_stemc, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "Live Stem Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)

x <- cdg_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_dead_stemc = mean(dead_stemc)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_dead_stemc, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "Dead Stem Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_height = mean(height)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_height, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "Height", x = "Wateryear", y = "Height (m)")
plot(x)


x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_psn_to_cpool = mean(psn_to_cpool)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_psn_to_cpool, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "psn_to_cpool", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)



x <- cdg_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_frootc = mean(frootc)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_frootc, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "Fine Root Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)


x <- cdg_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_dead_crootc = mean(dead_crootc)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_dead_crootc, color=names, linetype=run_type)) +
  xlim(1941,1952) +
  ylim(0,5) +
  labs(title = "Dead Coarse Root Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)


x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_rootzone.S = mean(rootzone.S)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_rootzone.S, color=names, linetype=run_type)) +
#  xlim(1941,1982) +
#  ylim(0,5) +
  labs(title = "rootzone.S", x = "Wateryear", y = "")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_psi = mean(psi)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_psi, color=names, linetype=run_type)) +
#  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "psi", x = "Wateryear", y = "")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_trans = mean(trans)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_trans, color=names, linetype=run_type)) +
 #   xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "trans", x = "Wateryear", y = "")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_evap = mean(evap)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_evap, color=names, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "evap", x = "Wateryear", y = "")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_ga = mean(ga)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_ga, color=names, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "ga", x = "Wateryear", y = "")
plot(x)

x <- cd_total %>%
  group_by(wy, names, run_type) %>%
  summarize(avg_gs = mean(gs)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_gs, color=names, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "gs", x = "Wateryear", y = "")
plot(x)


# Patch Level

x <- pd_total %>%
  group_by(wy, run_type) %>%
  summarize(avg_root_zone.S = mean(root_zone.S)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_root_zone.S, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "root_zone.S", x = "Wateryear", y = "")
plot(x)

x <- pd_total %>%
  group_by(wy, run_type) %>%
  summarize(avg_gasnow = mean(gasnow)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_gasnow, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "gasnow", x = "Wateryear", y = "")
plot(x)

x <- pd_total %>%
  group_by(wy, run_type) %>%
  summarize(avg_ga = mean(ga)) %>% 
  ggplot() +
  geom_line(aes(x = wy, y = avg_ga, linetype=run_type)) +
  #  xlim(1941,1982) +
  #  ylim(0,5) +
  labs(title = "ga", x = "Wateryear", y = "")
plot(x)



# Daily plots


x <-  ggplot(cdg_total) +
  geom_line(aes(x = date, y = leafc, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,3) +
  labs(title = "Leaf Carbon", x = "Wateryear", y = "Carbon (g/m2)")
plot(x)
#ggsave("p300_c_frac_layer_1_3_3.pdf",plot = x, path = PATCH_SIM_DIR)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = height, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,.1) +
  labs(title = "height", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = trans, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,0.025) +
  labs(title = "trans", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = psi, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
#  ylim(0,10) +
  labs(title = "PSI", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = m_LWP, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  #  ylim(0,10) +
  labs(title = "m_LWP", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = gs, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
    ylim(0,1) +
  labs(title = "gs", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = psn_to_cpool, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,0.06) +
  labs(title = "PSN", x = "Wateryear", y = "")
plot(x)


x <-  ggplot(cdg_total) +
  geom_line(aes(x = date, y = root_depth, color=names, linetype=run_type)) +
#  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,10) +
  labs(title = "root_depth", x = "Wateryear", y = "mm")
plot(x)


x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = rootzone.S, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,1) +
  labs(title = "rootzone.S", x = "Wateryear", y = "")
plot(x)


x <-  ggplot(cdg_total) +
  geom_line(aes(x = date, y = mresp, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,0.025) +
  labs(title = "mresp", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = ga, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
#  ylim(0,.5) +
  labs(title = "ga", x = "Wateryear", y = "")
plot(x)


x <-  ggplot(pd_total) +
  geom_line(aes(x = date, y = root_zone.S, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,1) +
  labs(title = "root_zone.S", x = "Wateryear", y = "")
plot(x)


x <-  ggplot(pd_total) +
  geom_line(aes(x = date, y = gasnow, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
  ylim(0,10) +
  labs(title = "gasnow", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(pd_total) +
  geom_line(aes(x = date, y = ga, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '5/1/1944'), format="%m/%d/%Y")) +
 # ylim(0,10) +
  labs(title = "ga", x = "Wateryear", y = "")
plot(x)




# -----
# for playing around


x <-  ggplot(pd_total) +
  geom_line(aes(x = date, y = soil_evap, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '8/1/1945'), format="%m/%d/%Y")) +
  #ylim(0,.010) +
  labs(title = "", x = "Wateryear", y = "")
plot(x)

x <-  ggplot(cd_total) +
  geom_line(aes(x = date, y = ga, color=names, linetype=run_type)) +
  xlim(as.Date(c('4/1/1944', '8/1/1945'), format="%m/%d/%Y")) +
  ylim(0,10) +
  labs(title = "", x = "Wateryear", y = "")
plot(x)

x <- cdg_total %>% 
  dplyr::filter(run_type == "different", names == 1) %>% 
  ggplot() +
  geom_line(aes(x = date, y = live_stemc, color = names)) +
  xlim(as.Date(c('4/1/1944', '5/1/1945'), format="%m/%d/%Y")) +
  ylim(0,1) +
  labs(title = "ga", x = "Wateryear", y = "")
plot(x)


cd_total %>% 
  dplyr::filter(names == 1) %>% 
  tail()


# Produce tables

# 
# x <- select(cd, names, lai, date)
# lai <- tidyr::spread(x, names, lai)
# View(lai)
# 
# x <- select(cd, names, height, date)
# height <- tidyr::spread(x, names, height)
# View(height)
# 
# x <- select(cd, names, psi, date)
# psi <- tidyr::spread(x, names, psi)
# View(psi)
# 
# x <- select(cd, names, evap, date)
# evap <- tidyr::spread(x, names, evap)
# View(evap)
# 
# x <- select(cd, names, trans, date)
# trans <- tidyr::spread(x, names, trans)
# View(trans)
# 
# x <- select(cd, names, ga, date)
# ga <- tidyr::spread(x, names, ga)
# View(ga)
# 
# x <- select(cd, names, psn_to_cpool, date)
# psn_to_cpool <- tidyr::spread(x, names, psn_to_cpool)
# View(psn_to_cpool)
# 
# x <- select(cdg, names, dead_stemc, date)
# dead_stemc <- tidyr::spread(x, names, dead_stemc)
# View(dead_stemc)
# 
# x <- select(cdg, names, live_stemc, date)
# live_stemc <- tidyr::spread(x, names, live_stemc)
# View(live_stemc)
# 
# x <- select(cdg, names, leafc, date)
# leafc <- tidyr::spread(x, names, leafc)
# View(leafc)
# 
# x <- select(cdg, names, frootc, date)
# frootc <- tidyr::spread(x, names, frootc)
# View(frootc)
# 
# x <- select(cdg, names, live_crootc, date)
# live_crootc <- tidyr::spread(x, names, live_crootc)
# View(live_crootc)
# 
# x <- select(cdg, names, mresp, date)
# mresp <- tidyr::spread(x, names, mresp)
# View(mresp)

# 