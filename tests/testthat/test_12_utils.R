
# merge_all() -----------------------------------------------------------
test_that("merge_all() works as expected", {
  unlink("testElev", recursive = TRUE)

  skip_if(dir.exists("testElev"))
  file.copy(system.file("extdata", "testELEV", package = "LITAP"), to = ".",
            recursive = TRUE, overwrite = TRUE)

  t <- merge_all(folder = "testELEV") %>%
    expect_silent() %>%
    expect_s3_class("data.frame") %>%
    expect_named(
      c("seqno", "x", "y", "row", "col", "elev",
        "ddir", "drec", "upslope", "upslope_m", "uced",
        "vol2fl", "mm2fl", "parea",
        "initial_shed", "local_shed", "pond_shed", "fill_shed",
        "sgre", "sgr", "sgcn", "sgc", "scr", "scc", "hill_r_dir", "hill_c_dir",
        "hill_r_n", "hill_r_cell", "hill_c_n", "hill_c_cell", "edge_map",
        "inv_ddir", "inv_drec", "inv_upslope", "inv_upslope_m",
        "inv_initial_shed", "inv_local_shed", "inv_edge_map",

        "st_row", "st_col", "st_elev", "z2st", "n2st",
        "cr_row", "cr_col", "cr_elev", "z2cr", "n2cr",
        "pit_row", "pit_col", "pit_elev", "z2pit", "n2pit",
        "peak_row", "peak_col", "peak_elev", "z2peak", "n2peak",
        "z2top", "zpit2peak", "zcr2st", "zcr2pit", "ztop2pit", "ncr2st",
        "pmin2max", "pctz2st", "pctz2pit", "pctz2top", "pctn2st",
        "l2pit", "l2peak", "lpit2peak", "ppit2peakl", "l2str", "l2div", "lstr2div",
        "pstr2divl",

        "slope_pct", "slope_deg", "aspect", "new_asp",
        "prof",  "plan", "qarea1", "qarea2", "qweti1","qweti2",
        "lnqarea1", "lnqarea2"))

  expect_gt(nrow(t), 5000)
  expect_gt(ncol(t), 50)

  expect_false(any(stringr::str_detect(names(t), "\\.x|\\.y")))
  expect_true(file.exists("testELEV/all_points.rds"))

  # Check csv
  expect_silent(t <- merge_all(folder = "testELEV", out_format = "csv")) %>%
    expect_s3_class("data.frame")
  expect_true(file.exists("testELEV/all_points.csv"))

  unlink("testELEV", recursive = TRUE)
})
