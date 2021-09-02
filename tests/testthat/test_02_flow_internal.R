suppressMessages(f <- load_file(system.file("extdata", "testELEV.dbf",
                                            package = "LITAP"),
                                nrow = 90, ncol = 90, grid = 1))


test_that("Sub-functions", {

  set.seed(777)
  s <- sample(1:8400, size = 500)

  # Directions
  expect_silent(d <- calc_ddir2(f, verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Watersheds"
  expect_silent(d <- calc_shed4(d, verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  #Initial Pit Removal
  expect_silent(d_local <- first_pitr1(d, max_area = 10, max_depth = 5,
                                       verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Pond Pit Removal
  expect_silent(d_pond <- second_pitr1(d_local, verbose = FALSE))
  expect_snapshot_value(sub_dem(d_pond$db, s), style = "json2")
  expect_snapshot_value(sub_dem(d_pond$stats, s), style = "json2")

  d_local <- dplyr::left_join(
    d_local,
    dplyr::select(d_pond$db, local_shed, pond_shed) %>%
      dplyr::distinct(),
    by = "local_shed")

  # Fill Pit Removal
  expect_silent(d_fill <- third_pitr1(d_local, verbose = FALSE))
  expect_snapshot_value(sub_dem(d_fill$db, s), style = "json2")
  expect_snapshot_value(sub_dem(d_fill$stats, s), style = "json2")

  # Inversion
  expect_silent(d <- invert(f)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Inverted directions
  expect_silent(d <- calc_ddir2(d, verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Inverted Watersheds
  expect_silent(d <- calc_shed4(d, verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Inverted Initial Pit Removal
  expect_silent(d <- first_pitr1(d, max_area = 10, max_depth = 5,
                                 verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")
})

test_that("elev_diff calculated correctly", {
  db_test <- tibble::tibble(seqno = 1:9,
                            col = rep(1:3, 3),
                            row = sort(rep(1:3, 3)),
                            elev = c(99, 98, 99,
                                     98, 97, 98,
                                     97, 96, 97)) %>%
    add_buffer() %>%
    calc_ddir2(verbose = FALSE) %>%
    calc_shed4(verbose = FALSE) %>%
    first_pitr1(max_area = 0, max_depth = 0, verbose = FALSE) %>%
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
    calc_ddir2(verbose = FALSE) %>%
    calc_shed4(verbose = FALSE) %>%
    first_pitr1(max_area = 0, max_depth = 0, verbose = FALSE) %>%
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
