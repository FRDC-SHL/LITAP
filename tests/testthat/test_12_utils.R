

test_that("merge_flow_form() works as expected", {
  dir <- system.file("extdata", "testELEV", package = "LITAP")
  expect_silent(t <- merge_flow_form(folder = dir)) %>%
    expect_s3_class("data.frame")

  expect_true(all(
    c("seqno", "upslope", "initial_shed", "hill_c_cell", "z2st",
      "pctz2st", "peak_row", "slope_deg", "prof", "qarea1", "qweti2") %in%
      names(t)))

  expect_gt(nrow(t), 5000)
  expect_gt(ncol(t), 50)

  expect_false(any(stringr::str_detect(names(t), "\\.x|\\.y")))
})
