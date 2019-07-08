context("form_mapper() sub-routines")

test_that("Sub-functions", {
  flow_mapper(f, nrow = 11, ncol = 11, out_folder = dir, report = FALSE)
  grid <- 5

  # DB files
  expect_silent(db <- get_backups(dir, type = "fill")) %>%
    expect_is("data.frame")
  expect_silent(idb <- get_backups(dir, type = "ilocal")) %>%
    expect_is("data.frame")

  # Form
  expect_silent(d <- calc_form(db, grid))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_calc_form", print = TRUE)

  # Weti
  expect_silent(d <- calc_weti(db, grid))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_calc_weti", print = TRUE)

  # Relief
  expect_silent(db_relz <- calc_relz(db, idb))
  expect_known_output(as.data.frame(db_relz)[150:500,],
                      "./ref/test_calc_relz", print = TRUE)

  # Length
  expect_silent(d <- calc_length(db, db_relz))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_calc_length", print = TRUE)
})
