

test_that("merge_flow_form() works as expected", {
  file.copy(system.file("extdata", "testELEV", package = "LITAP"), to = ".",
            recursive = TRUE)
  expect_silent(t <- merge_flow_form(folder = "testELEV")) %>%
    expect_s3_class("data.frame")

  expect_true(all(
    c("seqno", "upslope", "initial_shed", "hill_c_cell", "z2st",
      "pctz2st", "peak_row", "slope_deg", "prof", "qarea1", "qweti2") %in%
      names(t)))

  expect_gt(nrow(t), 5000)
  expect_gt(ncol(t), 50)

  expect_false(any(stringr::str_detect(names(t), "\\.x|\\.y")))
  expect_true(file.exists("testELEV/dem_flow_form_merged.rds"))

  # Check csv
  expect_silent(t <- merge_flow_form(folder = "testELEV", out_format = "csv")) %>%
    expect_s3_class("data.frame")
  expect_true(file.exists("testELEV/dem_flow_form_merged.csv"))

  unlink("./testELEV")
})
