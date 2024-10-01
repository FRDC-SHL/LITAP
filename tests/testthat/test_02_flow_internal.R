suppressMessages(f <- load_file(system.file("extdata", "testELEV.dbf",
                                            package = "LITAP"),
                                nrow = 90, ncol = 90, grid = 1))


# Subfunctions ------------------------------------------------------------


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
                                       method = "litap", verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")

  # Pond Pit Removal
  expect_silent(d_pond <- second_pitr1(d_local, method = "litap", verbose = FALSE))
  expect_snapshot_value(sub_dem(d_pond$db, s), style = "json2")
  expect_snapshot_value(sub_dem(d_pond$stats, s), style = "json2")

  d_local <- dplyr::left_join(
    d_local,
    dplyr::select(d_pond$db, local_shed, pond_shed) %>%
      dplyr::distinct(),
    by = "local_shed")

  # Fill Pit Removal
  expect_silent(d_fill <- third_pitr1(d_local, method = "litap", verbose = FALSE))
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
  expect_silent(d <- first_pitr1(d, max_area = 10, max_depth = 5, method = "litap",
                                 verbose = FALSE)) %>%
    sub_dem(s) %>%
    expect_snapshot_value(style = "json2")
})


# UCED -----------------------------------------------------------------
test_that("uced calculated correctly", {
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

  expect_equal(db_test$uced, c(0, 0, 0,
                               0, 5, 0,
                               0, 15, 0))

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

  # flow_plot(db_test, type = "elev", dir = TRUE) +
  #   geom_text(aes(label = elev, color = "Elevation")) +
  #   geom_text(aes(label = uced, colour = "UCED"), nudge_x = 0.25) +
  #   labs(title = "Uplsope Cumulative Elevation Drop") +
  #   scale_colour_manual(values = c("black", "blue"))


  expect_equal(db_test$uced, c(0, 0, 0, 0,
                               0, 3, 0, 5,
                               0, 8, 0, 41,
                               50, 0, 0, 0))

  expect_equal(db_test$upslope, c(1, 1, 1, 1,
                                  1, 3, 1, 3,
                                  1, 5, 1, 8,
                                  8, 1, 1, 1))



  db_test <- tibble::tibble(seqno = 1:36,
                            col = rep(1:6, 6),
                            row = sort(rep(1:6, 6)),
                            elev = c(99, 98, 99, 98,  111, 107,
                                     98, 97, 98, 96,  110, 109,
                                     97, 96, 97, 90,  109, 110,
                                     90, 95, 92, 91,   93,  95,
                                     89, 90, 94, 99,   94,  95,
                                     99, 99, 97, 101, 103,  101)) %>%
    add_buffer() %>%
    calc_ddir2(verbose = FALSE) %>%
    calc_shed4(verbose = FALSE) %>%
    first_pitr1(max_area = 0, max_depth = 0, method = "litap", verbose = FALSE) %>%
    remove_buffer()

  flow_plot(db_test, type = "elev", dir = TRUE, pits = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = elev, colour = "Elevation"),
                       nudge_x = -0.25, nudge_y = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = uced, colour = "UCED"),
                       nudge_x = 0.25, nudge_y = 0.25) +
    ggplot2::labs(title = "Uplsope Cumulative Elevation Drop") +
    ggplot2::scale_colour_manual(values = c("black", "blue"))


  expect_equal(db_test$uced, c(0, 0, 0, 13, 0, 2,
                               0, 3, 0, 20, 0, 0,
                               0, 8, 0, 169, 0, 0,
                               45, 0, 0, 23, 29, 15,
                               107, 38, 7, 0, 9, 6,
                               0, 0, 0, 0, 0, 0))

  expect_equal(db_test$upslope, c(1, 1, 1, 2, 1, 2,
                                  1, 3, 1, 4, 1, 1,
                                  1, 5, 1, 18, 1, 1,
                                  7, 1, 1, 4, 5, 2,
                                  16, 7, 2, 1, 2, 2,
                                  1, 1, 1, 1, 1, 1))


})
