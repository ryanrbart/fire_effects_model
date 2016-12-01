# Spinup Evaluation
#
# Contains scripts for evaluating spinup.

library(rhessysR)
library(ggplot2)


# ---------------------------------------------------------------------
# Input and output paths

P300_MODEL_SPINUP <- "ws_p300/out/p300_spinup/spinup"

OUTPUT_DIR <- "outputs"
SPINUP_DIR <- file.path(OUTPUT_DIR, "spinup")

# ---------------------------------------------------------------------
# P300 Spinup Output

p300_spinup <- readin_rhessys_output(P300_MODEL_SPINUP, b=1, g=1, c=1, p=1)

bd <- p300_spinup$bd
bdg <- p300_spinup$bdg
pd <- p300_spinup$pd
pdg <- p300_spinup$pdg
cd <- separate_canopy_output(p300_spinup$cd, 2)
cdg <- separate_canopy_output(p300_spinup$cdg, 2)

# ---------------------------------------------------------------------
# P300 Evaluation


p300_spinup_height <- ggplot(data = cd) +
  geom_line(aes(x=date,y=height, linetype=as.character(names)))
plot(p300_spinup_height)
ggsave("p300_spinup_height",p300_spinup_height, path = SPINUP_DIR)

p300_spinup_lai <- ggplot(data = cd) +
  geom_line(aes(x=date,y=lai, linetype=as.character(names)))
plot(p300_spinup_lai)
ggsave("p300_spinup_lai",p300_spinup_lai, path = SPINUP_DIR)

p300_spinup_npool <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=npool, linetype=as.character(names)))
plot(p300_spinup_npool)
ggsave("p300_spinup_npool",p300_spinup_npool, path = SPINUP_DIR)

p300_spinup_leafc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=leafc, linetype=as.character(names)))
plot(p300_spinup_leafc)
ggsave("p300_spinup_leafc",p300_spinup_leafc, path = SPINUP_DIR)

p300_spinup_livestemc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_stemc, linetype=as.character(names)))
plot(p300_spinup_livestemc)
ggsave("p300_spinup_livestemc",p300_spinup_livestemc, path = SPINUP_DIR)

p300_spinup_deadstemc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=dead_stemc, linetype=as.character(names)))
plot(p300_spinup_deadstemc)
ggsave("p300_spinup_deadstemc",p300_spinup_deadstemc, path = SPINUP_DIR)

p300_spinup_frootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=frootc, linetype=as.character(names)))
plot(p300_spinup_frootc)
ggsave("p300_spinup_frootc",p300_spinup_frootc, path = SPINUP_DIR)

p300_spinup_livecrootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_crootc, linetype=as.character(names)))
plot(p300_spinup_livecrootc)
ggsave("p300_spinup_livecrootc",p300_spinup_livecrootc, path = SPINUP_DIR)

p300_spinup_deadcrootc <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=live_crootc, linetype=as.character(names)))
plot(p300_spinup_deadcrootc)
ggsave("p300_spinup_deadcrootc",p300_spinup_deadcrootc, path = SPINUP_DIR)

p300_spinup_cpool <- ggplot(data = cdg) +
  geom_line(aes(x=date,y=cpool, linetype=as.character(names)))
plot(p300_spinup_cpool)
ggsave("p300_spinup_cpool",p300_spinup_cpool, path = SPINUP_DIR)

# -------
# Patch

p300_spinup_soil1c <- ggplot(data = pdg) +
  geom_line(aes(x=date,y=soil1c))
plot(p300_spinup_soil1c)
ggsave("p300_spinup_soil1c",p300_spinup_soil1c, path = SPINUP_DIR)

p300_spinup_litr1c <- ggplot(data = pdg) +
  geom_line(aes(x=date,y=litr1c))
plot(p300_spinup_litr1c)
ggsave("p300_spinup_litr1c",p300_spinup_litr1c, path = SPINUP_DIR)


# -------
# Basin

p300_spinup_b_lai <- ggplot(data = bdg) +
  geom_line(aes(x=date,y=lai))
plot(p300_spinup_b_lai)
ggsave("p300_spinup_b_lai",p300_spinup_b_lai, path = SPINUP_DIR)



# Ratio overstory to understory carbon


# ---------------------------------------------------------------------
# Rattlesnake








