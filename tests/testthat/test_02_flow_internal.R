context("flow_mapper() sub-routines")
f <- load_file(system.file("extdata", "testELEV.dbf", package = "LITAP"), nrow = 150, ncol = 150)

test_that("Sub-functions", {
  # Directions
  expect_message(d <- calc_ddir2(f), NA)
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_calc_ddir", print = TRUE)

  # Watersheds"
  expect_silent(d <- calc_shed4(d))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_calc_shed", print = TRUE)

  #Initial Pit Removal
  expect_silent(d_local <- first_pitr1(d, max_area = 10, max_depth = 5))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_local", print = TRUE)

  # Pond Pit Removal
  expect_silent(d_pond <- second_pitr1(d_local))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_pond", print = TRUE)
  d_local <- dplyr::left_join(d_local, dplyr::select(d_pond$db, local_shed, pond_shed) %>%
                                dplyr::distinct(), by = "local_shed")

  # Fill Pit Removal
  expect_silent(d_fill <- third_pitr1(d_local))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_fill", print = TRUE)

  # Inversion
  expect_silent(d <- invert(f))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_iinitial", print = TRUE)

  # Inverted directions
  expect_message(d <- calc_ddir2(d), NA)
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_iddir", print = TRUE)

  # Inverted Watersheds
  expect_silent(d <- calc_shed4(d))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_ished", print = TRUE)

  # Inverted Initial Pit Removal
  expect_silent(d <- first_pitr1(d, max_area = 10, max_depth = 5))
  expect_known_output(as.data.frame(d)[150:500,],
                      "./ref/test_ilocal", print = TRUE)
})

test_that("elev_diff calculated correctly", {
  db_test <- tibble::tibble(seqno = 1:9,
                            col = rep(1:3, 3),
                            row = sort(rep(1:3, 3)),
                            elev = c(99, 98, 99,
                                     98, 97, 98,
                                     97, 96, 97)) %>%
    add_buffer() %>%
    calc_ddir2() %>%
    calc_shed4() %>%
    first_pitr1(max_area = 0, max_depth = 0) %>%
    remove_buffer()

  expect_equal(db_test$elev_diff, c(0, 0, 0,
                                    0, 5, 0,
                                    0, 12, 0))

  expect_equal(db_test$upslope, c(1, 1, 1,
                                  1, 4, 1,
                                  1, 9, 1))


  db_test <- tibble::tibble(seqno = 1:16,
                            col = rep(1:4, 4),
                            row = sort(rep(1:4, 4)),
                            elev = c(99, 98, 99, 98,
                                     98, 97, 98, 96,
                                     97, 96, 97, 90,
                                     90, 95, 92, 91)) %>%
    add_buffer() %>%
    calc_ddir2() %>%
    calc_shed4() %>%
    first_pitr1(max_area = 0, max_depth = 0) %>%
    remove_buffer()

  #flow_plot(db_test, type = "elev", seqno = TRUE, dir = TRUE)

  expect_equal(db_test$elev_diff, c(0, 0, 0, 0,
                                    0, 3, 0, 5,
                                    0, 6, 0, 29,
                                    24, 0, 0, 0))

  expect_equal(db_test$upslope, c(1, 1, 1, 1,
                                  1, 3, 1, 3,
                                  1, 5, 1, 8,
                                  8, 1, 1, 1))
})
