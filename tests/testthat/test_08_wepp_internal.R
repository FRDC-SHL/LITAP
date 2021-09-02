test_that("mark_neighbours evaluates correctly", {

  # No neighbouring channel
  expect_silent(mark_neighbours(4, 2, 2, 2, 3, 2, 2, 2, 2, # elev,
                                2, 2, 2, 2, 4, 2, 2, 2, 2, # upslope,
                                1, 1, 1, 1, 2, 1, 1, 1, 1, # seqno
                                upslope_threshold = 3)) %>%
    expect_false()

  # Simple neighbouring channel
  expect_silent(mark_neighbours(4, 2, 2, 2, 3, 2, 2, 2, 2, # elev,
                                4, 2, 2, 2, 4, 2, 2, 2, 2, # upslope,
                                1, 1, 1, 1, 2, 1, 1, 1, 1, # seqno
                                upslope_threshold = 3)) %>%
    expect_true()

  # Tied elev means no neighbouring channel (focal has lower seqno)
  expect_silent(mark_neighbours(3, 2, 2, 2, 3, 2, 2, 2, 2, # elev,
                                4, 2, 2, 2, 4, 2, 2, 2, 2, # upslope,
                                10, 1, 1, 1, 2, 1, 1, 1, 1, # seqno
                                upslope_threshold = 3)) %>%
    expect_false()

  # Tied elev means no neighbouring channel (focal has higher seqno)
  expect_silent(mark_neighbours(3, 2, 2, 2, 3, 2, 2, 2, 2, # elev,
                                4, 2, 2, 2, 4, 2, 2, 2, 2, # upslope,
                                1, 1, 1, 1, 2, 1, 1, 1, 1, # seqno
                                upslope_threshold = 3)) %>%
    expect_true()

})

test_that("neighbour_channels identifies neighbouring channel cells", {
  db <- tidyr::expand_grid(col = 1:5, row = 1:5) %>%
    dplyr::mutate(seqno = 1:25,
                  elev = c(1.0, 1.1, 2.0, 2.2, 2.2,
                           1.5, 1.7, 2.9, 3.1, 3.1,
                           2.5, 3.0, 3.2, 4.1, 5.0,
                           3.0, 3.0, 3.0, 4.0, 4.5,
                           3.5, 2.5, 3.0, 4.0, 4.6)) %>%
    add_buffer() %>%
    calc_ddir2(verbose = TRUE) %>%
    expect_message("Fixing flat plateaus") %>%
    expect_message("Fixing flat plateaus") %>%
    dplyr::mutate(shedno = 1) %>%
    calc_upslopes() %>%
    expect_silent()

#flow_plot(db, dir = TRUE, type = "elev")

  expect_silent(check_neighbours(db, dplyr::filter(db, upslope > 5), 5))

})

test_that("mark_chan does channel cells", {
  db <- tidyr::expand_grid(col = 1:10, row = 1:10) %>%
    dplyr::mutate(seqno = 1:100,
                  elev = c(1.0, 1.1, 2.0, 2.2, 2.2, 1.0, 1.1, 2.0, 2.2, 2.2,
                           1.5, 1.7, 2.9, 3.1, 3.1, 1.5, 1.7, 2.9, 3.1, 3.1,
                           2.5, 3.0, 3.2, 4.1, 5.0, 2.5, 3.0, 3.2, 4.1, 5.0,
                           3.0, 3.0, 3.0, 4.0, 4.5, 3.0, 3.0, 3.0, 4.0, 4.5,
                           3.5, 2.5, 3.0, 4.0, 4.6, 3.5, 2.5, 3.0, 4.0, 4.6,
                           1.0, 1.1, 2.0, 2.2, 2.2, 1.0, 1.1, 2.0, 2.2, 2.2,
                           1.5, 1.7, 2.9, 3.1, 3.1, 1.5, 1.7, 2.9, 3.1, 3.1,
                           2.5, 3.0, 3.2, 4.1, 5.0, 2.5, 3.0, 3.2, 4.1, 5.0,
                           3.0, 3.0, 3.0, 4.0, 4.5, 3.0, 3.0, 3.0, 4.0, 4.5,
                           3.5, 2.5, 3.0, 4.0, 4.6, 3.0, 3.0, 3.0, 4.0, 4.5)) %>%
    add_buffer() %>%
    calc_ddir2(verbose = TRUE) %>%
    expect_message("Fixing flat plateaus") %>%
    expect_message("Fixing flat plateaus") %>%
    dplyr::mutate(shedno = 1) %>%
    calc_upslopes() %>%
    expect_silent()

  expect_silent(m <- mark_chan(db, upslope_threshold = 2))
  expect_true(all(m$chan_no %in% 0:9))


  # flow_plot(db, type = "elev", dir = TRUE) +
  #   ggplot2::geom_point(ggplot2::aes(colour = factor(chan_no)))

})
