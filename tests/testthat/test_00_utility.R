context("Utility Functions")

test_that("get_dir returns correct direction", {
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1), 1)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1), 2)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2), 3)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1), 4)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2), 5)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2), 6)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1), 7)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1), 8)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2), 9)
})

test_that("get_dir returns correct direction given ddir_opts", {

  #1
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(2, 3)), 2)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(4, 7)), 4)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(8, 6)), 8)

  #2
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(3, 6)), 3)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(1, 4)), 1)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(7, 8)), 7)

  #3
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(2, 1)), 2)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(6, 9)), 6)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(4, 8)), 8)

  #4
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(7, 8)), 7)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(1, 2)), 1)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(9, 3)), 9)

  #5
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, i), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(1, 2)), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(3, 4)), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(8, 9)), 5)

  #6
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(8, 9)), 9)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(2, 3)), 3)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(7, 1)), 7)

  #7
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(4, 1)), 4)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(8, 9)), 8)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(2, 6)), 6)

  #8
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(4, 7)), 7)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(9, 6)), 9)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(1, 3)), 1)

  #9
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, i), i)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(7, 8)), 8)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(3, 6)), 6)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(4, 2)), 4)
})

test_that("slope_gc works as expected", {
  grid <- 5
  expect_silent(s <- slope_gc(test_dem, grid = 5))
  expect_true(dplyr::all_equal(s[names(test_dem)], test_dem))

  s <- remove_buffer(s)

  # Row / Column Slope gradients
  expect_equal(s$sgc, abs(s$sgcn), tolerance = 0.00001)
  expect_equal(s$sgr, abs(s$sgre), tolerance = 0.00001)

  # Row / Column Slope gradients east / north
  max_cols <- max(s$col)
  for(i in c(500, 1000, 5000, 10000, 16000, 20000,
             24000, 26000, 30000, 32000, 34000)) {
    expect_equal(s$sgre[!!i], (s$elev[!!i - 1] - s$elev[!!i + 1]) / (2 * 5))
    expect_equal(s$sgcn[!!i], (s$elev[!!i + max_cols] - s$elev[!!i - max_cols]) / (2 * 5))
  }

  # Row / Column Slope curvature
  for(i in c(500, 1000, 5000, 10000, 16000, 20000,
             24000, 26000, 30000, 32000, 34000)) {
    expect_equal(s$scr[!!i], (2 * s$elev[!!i] - s$elev[!!i - 1]- s$elev[!!i + 1]) / (grid^2))
    expect_equal(s$scc[!!i], (2 * s$elev[!!i] - s$elev[!!i + max_cols] - s$elev[!!i - max_cols]) / (grid^2))
  }

})

test_that("merge_flow_form() works as expected", {
  dir <- system.file("extdata", "testELEV", package = "LITAP")
  expect_silent(t <- merge_flow_form(folder = dir)) %>%
    expect_s3_class("data.frame")

  expect_gt(nrow(t), 10000)
  expect_gt(ncol(t), 50)

  expect_false(any(stringr::str_detect(names(t), "\\.x|\\.y")))
})
