# rhessysR testing script
# Ryan Bart
# September 2016

# ---------------------------------------------------------------------

rhessys_version = "/Users/ryanrbart/bin/rhessys5.20.fire_off"
tec_file = "tecfiles/tec.p301_sim"
world_file = "worldfiles/world.p301_patch_2canopy_dated_seq"
world_hdr_file = "worldfiles/world.p301_patch_2canopy_dated_seq.hdr"
flow_file = "flowtables/flow.patch"
start_date = "1945 10 1 1"
end_date = "1945 10 7 1"
output_file = "out/cal5"
output_filename = "test"
comm_line_options = "-b"
parameter_type = "GRID"
m = 1.792761
k = 1.566492
m_v = 1.792761
k_v = 1.566492
pa = 7.896941
po = 1.179359
gw1 = 0.168035
gw2 = 0.178753


awk1 = c(15,25)
awk2 = c(100,1000)
awk3 = 1000

# List containing awk_file, input_file, output_file
awk1_filenames = list("awks/change.def.overstory_height_thresh.awk",
                      "defs/veg_p301_conifer_cec.def", "defs/veg1.tmp")
awk2_filenames = list("awks/change.def.understory_height_thresh.awk",
                      "defs/veg_p301_conifer_cec.def", "defs/veg2.tmp")
awk3_filenames = list("awks/change.def.pspread_loss_rel.awk",
                      "defs/veg_p301_conifer_cec.def", "defs/veg3.tmp")
awk_filenames = list(awk1_filenames)


output_selection = list()



run_rhessys_simulation(rhessys_version, tec_file = tec_file,
                       world_file = world_file, world_hdr_file = world_hdr_file,
                       flow_file = flow_file, start_date = start_date,
                       end_date = end_date, output_file = output_file,
                       output_filename = output_filename,
                       comm_line_options = comm_line_options,
                       parameter_type = parameter_type,
                       m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po,
                       gw1 = gw1, gw2 = gw2, awk_filenames=awk_filenames, awk1)


