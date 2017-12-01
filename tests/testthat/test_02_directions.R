context("Sub routines")
f <- load_file(system.file("extdata", "testELEV.dbf", package = "LITAP"), nrow = 150, ncol = 150)

test_that("Directions", {
  expect_silent(d <- calc_ddir2(f))
  expect_equal_to_reference(d, "test_calc_ddir.rds")
})

test_that("Watersheds", {
  expect_silent(d <- calc_shed4(d))
  expect_equal_to_reference(d, "test_calc_shed.rds")
})

test_that("Initial Pit Removal", {
  expect_silent(d_local <- first_pitr1(d, max_area = 10, max_depth = 5))
  expect_equal_to_reference(d, "test_initial.rds")
})

test_that("Pond Pit Removal", {
  expect_silent(d_pond <- second_pitr1(d_local))
  expect_equal_to_reference(d, "test_initial.rds")
  d_local <- dplyr::left_join(d_local, dplyr::select(d_pond$db, local_shed, pond_shed) %>% dplyr::distinct(), by = "local_shed")
})

test_that("Fill Pit Removal", {
  expect_silent(d_fill <- third_pitr1(d_local))
  expect_equal_to_reference(d, "test_initial.rds")
})

test_that("Inversion", {
  expect_silent(d <- invert(f))
  expect_equal_to_reference(d, "test_initial.rds")
})

test_that("Inverted directions", {
  expect_silent(d <- calc_ddir2(d))
  expect_equal_to_reference(d, "test_initial.rds")
})

test_that("Inverted Watersheds", {
  expect_silent(d <- calc_shed4(d))
  expect_equal_to_reference(d, "test_initial.rds")
})

test_that("Inverted Initial Pit Removal", {
  expect_silent(d <- first_pitr1(d, max_area = 10, max_depth = 5))
  expect_equal_to_reference(d, "test_initial.rds")
})
