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
  expect_silent(s <- slope_gc(test_dem, grid = grid))
  expect_true(dplyr::all_equal(s[names(test_dem)], test_dem))

  s <- remove_buffer(s)

  # Row / Column Slope gradients
  expect_true(all(s$sgc != 0, na.rm = TRUE))
  expect_true(all(s$sgr != 0, na.rm = TRUE))
  expect_true(all(s$sgcn != 0, na.rm = TRUE))
  expect_true(all(s$sgre != 0, na.rm = TRUE))

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

  # Hills
  expect_true(all(s$hill_r_dir %in% c(1, 4)))
  expect_true(all(s$hill_c_dir %in% c(1, 3)))
  expect_true(all(s$hill_r_dir[s$sgre < 0] == 4))
  expect_true(all(s$hill_r_dir[s$sgre > 0] == 1))
  expect_true(all(s$hill_c_dir[s$sgcn < 0] == 3))
  expect_true(all(s$hill_c_dir[s$sgcn > 0] == 1))

  expect_lte(max(s$hill_r_n), max(s$col))
  expect_lte(max(s$hill_c_n), max(s$row))

  expect_true(all(s$hill_r_n[410:416] == 1))
  expect_true(all(s$hill_r_cell[410:416] == 36:42))

  expect_true(all(s$hill_r_n[417:425] == 2))
  expect_true(all(s$hill_r_cell[417:425] == 1:9))

  expect_true(all(s$hill_c_n[s$col == 122][1:10] == 1))
  expect_true(all(s$hill_c_cell[s$col == 122][1:10] == 1:10))

  expect_true(all(s$hill_c_n[s$col == 122][59:68] == 5))
  expect_true(all(s$hill_c_cell[s$col == 122][59:68] == 1:10))
})

test_that("merge_flow_form() works as expected", {
  dir <- system.file("extdata", "testELEV", package = "LITAP")
  expect_silent(t <- merge_flow_form(folder = dir)) %>%
    expect_s3_class("data.frame")

  expect_gt(nrow(t), 10000)
  expect_gt(ncol(t), 50)

  expect_false(any(stringr::str_detect(names(t), "\\.x|\\.y")))
})
