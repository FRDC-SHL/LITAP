fix_names <- list("elev" = c("^elevation$" = "elev",
                             "^z$" = "elev",
                             "^northing$" = "y",
                             "^latitude$" = "y",
                             "^lat$" = "y",
                             "^easting$" = "x",
                             "^longitude$" = "x",
                             "^lon$" = "x",
                             "^long$" = "x",
                             "^gridcode$" = "zone",
                             "^code$" = "zone",
                             "^ecozone$" = "zone",
                             "^bec_zone$" = "zone",
                             "^beczone$" = "zone",
                             "^bec$" = "zone",
                             "^dss$" = "zone"),
                  "zone" = c("^id$" = "seqno",
                             "^cell$" = "seqno",
                             "^gridcode$" = "zone",
                             "^code$" = "zone",
                             "^ecozone$" = "zone",
                             "^bec_zone$" = "zone",
                             "^beczone$" = "zone",
                             "^bec$" = "zone",
                             "^dss$" = "zone"))
match_names <- dplyr::tribble(
  ~type, ~name, ~required,

  "elev", "x", FALSE,
  "elev", "y", FALSE,
  "elev", "elev", TRUE,

  "arule", "sortorder", TRUE,
  "arule", "file_in", TRUE,
  "arule", "attr_in", TRUE,
  "arule", "class_out", TRUE,
  "arule", "model_no", TRUE,
  "arule", "b", TRUE,
  "arule", "b_low", TRUE,
  "arule", "b_hi", TRUE,
  "arule", "b1",  TRUE,
  "arule", "b2",  TRUE,
  "arule", "d",  TRUE,
  "arule", "zone", FALSE,

  "crule", "f_name", TRUE,
  "crule", "fuzattr", TRUE,
  "crule", "attrwt", TRUE,
  "crule", "facet_no", TRUE,
  "crule", "f_code", TRUE,
  "crule", "zone", FALSE,

  "zone", "seqno", TRUE,
  "zone", "zone", TRUE)

arule_weti <- c("prof", "plan", "slope_pct", "aspect", "qweti",
                "qarea", "lnqarea", "new_asp")
arule_relief <- c("pctz2st", "pctz2pit", "z2pit")


test_dem <- load_file("../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/021ELEV.DBF",
                      nrow = 184, ncol = 187, missing_value = -9999, grid = 1,
                      edge = FALSE) %>%
  add_buffer()


# Column orders -------------------------------------------------------------
cols_order_stats <- list(
  "flow" = c("shedno", "edge_pit", "final", "end_pit", "shed_area",
             "pit_row", "pit_col", "pit_seqno", "pit_elev", "pour_elev", "pre_vol",
             "pit_vol", "varatio", "pit_area", "drains_to", "next_pit",
             "become", "remove", "in_row", "in_col", "in_seqno", "in_elev",
             "out_row", "out_col", "out_seqno", "out_elev"))

cols_order <- list(
  "flow" = c("seqno", "x", "y", "row", "col", "elev", "ddir", "drec",
             "upslope", "upslope_m", "uced",
             "vol2fl", "mm2fl", "parea",
             "initial_shed", "local_shed", "fill_shed", #pond_shed
             "inv_initial_shed", "inv_local_shed",
             "sgre", "sgr", "sgcn", "sgc", "scr", "scc",
             "hill_r_dir", "hill_c_dir", "hill_r_n", "hill_r_cell",
             "hill_c_n", "hill_c_cell", "edge_map", "ridge"),

  "form" = c("seqno", "x", "y", "row", "col", "elev", "ddir", "drec",
             "upslope",

             "slope_pct", "slope_deg", "aspect", "new_asp", "prof", "plan",
             "qarea1", "qarea2", "qweti1", "qweti2", "lnqarea1", "lnqarea2",

             "st_row", "st_col", "st_elev", "z2st", "n2st",
             "cr_row", "cr_col", "cr_elev", "z2cr", "n2cr",
             "pit_row", "pit_col", "pit_elev", "z2pit", "n2pit",
             "peak_row", "peak_col", "peak_elev", "z2peak", "n2peak",

             "z2top", "zpit2peak", "zcr2st", "zcr2pit",
             "ztop2pit", "ncr2st", "pmin2max",
             "pctz2st", "pctz2pit", "pctz2top",
             "pctn2st",

             "l2pit", "l2peak", "lpit2peak", "ppit2peakl",
             "l2str", "l2div", "lstr2div", "pstr2divl"
             ),

  "facet" = c("seqno", "x", "y", "row", "col", "elev", "zone",
              "convex_d", "concave_d", "planar_d",
              "convex_a", "concave_a", "planar_a",
              "high_wi", "low_wi", "near_level", "rel_steep",
              "near_div", "near_half", "near_chan", "near_peak", "near_mid",
              "near_pit", "hi_above", "planar_2x"),

  "wepp" = c("seqno", "x", "y", "row", "col", "elev", "ddir", "drec",
             "upslope",
             "seedtype", "shed_no", "shed_side", "hill_no",
             "chan_no", "chan_side", "segment_no",
             "slope_pct", "slope_deg", "aspect",
             "l2st", "n2st", "z2st",

             "initial_id", "final_id", "sort_order", "start_type", "start_seqno",
             "start_row", "start_col", "start_elev", "start_ddir",
             "end_type", "end_seqno", "end_row", "end_col", "end_elev",
             "len_cells", "len_meters", "width_m",
             "drain_seqno", "down_seg", "left_seg", "right_seg", "center_seg",
             "left_seqno", "right_seqno", "center_seqno",
             "left_hill", "right_hill", "top_hill",
             "left_imp", "right_imp", "top_imp", "chan_shape", "flow2crow",
             "impound",

             "chan_len", "num_points", "mean_slope", "gen_slope", "aspect", "profile"))

debug_files <- list(
  "flow" = c("dem_dir", "dem_initial", "dem_local", "dem_pond",
             "dem_idir", "dem_iinitial",
             "stats_initial", "stats_local"),
  "form" = c("dem_form", "dem_relief"),
  "facet" = c(),
  "wepp" = c("dem_wepp_form", "stats_renum", "dem_renum", "dem_hillsheds",
             "stats_upsegs", "stats_ddir2", "dem_ddir2", "stats_ordered",
             "stats_first_segs", "dem_ups", "dem_split", "dem_remarked",
             "dem_pits", "dem_ordered", "dem_flow", "dem_first_segs",
             "dem_merged", "dem_marked", "dem_cut"))



usethis::use_data(fix_names, match_names, arule_weti, arule_relief,
                  cols_order, cols_order_stats, debug_files,
                  internal = TRUE, overwrite = TRUE)
usethis::use_data(test_dem, internal = FALSE, overwrite = TRUE)


