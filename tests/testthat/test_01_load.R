context("Loading DEM")
f <- system.file("extdata", "testELEV.dbf", package = "LITAP")

test_that("DEM files load", {
  expect_silent(d <- load_dem(f, nrow = 150, ncol = 150))

  expect_is(d, "tbl_df")
  expect_equal(max(d$row), 150)
  expect_equal(max(d$col), 150)
  expect_equal(max(d$seqno), 22500)
  expect_equal(max(d$elev), 681.223)
  expect_equal(min(d$elev), 668.858)
})


test_that("Prep DB correct subset/buffer/edge", {
  for(i in 1:3) {
    rlim <- sample(0:147, 1)
    rlim <- c(rlim, sample((rlim+2):150, 1))
    clim <- sample(0:147, 1)
    clim <- c(clim, sample((clim+2):150, 1))

    expect_silent(d <- load_file(f, nrow = 150, ncol = 150, clim = clim, rlim = rlim))

    rmax <- (rlim[2] - rlim[1] + 1 + 2) # +2 for buffer
    cmax <- (clim[2] - clim[1] + 1 + 2) # +2 for buffer

    # Subset
    expect_is(d, "tbl_df")
    expect_equal(max(d$row), rmax)
    expect_equal(max(d$col), cmax)
    expect_equal(max(d$seqno), rmax * cmax)

    # Buffer
    expect_true(all(is.na(d$elev[d$row == 1])))
    expect_true(all(is.na(d$elev[d$row == rmax])))
    expect_true(all(is.na(d$elev[d$col == 1])))
    expect_true(all(is.na(d$elev[d$col == cmax])))
    expect_true(all(d$buffer[is.na(d$elev)]))

    # Edge of map
    expect_true(all(d$edge_map[d$row == 2 | d$row == (rmax-1)]))
    expect_true(all(d$edge_map[d$col == 2 | d$col == (cmax-1)]))
    expect_true(!any(d$edge_map[!d$buffer &
                                  d$row != 2 & d$row != (rmax-1) &
                                  d$col != 2 & d$col != (cmax-1)]))
  }
})

test_that("Informative warning/errors", {

  # Incorrect nrow/ncol
  for(i in 1:3) expect_error(load_file(f, nrow = sample(1:149, 1), ncol = sample(1:149, 1)),
                             "Number of rows and columns does not match the total number of cells")

  # Subset too small
  expect_error(load_file(f, nrow = 150, ncol = 150, clim = c(1,1), rlim = c(1,1)),
               "Subset is too small \\(less than 2x2\\)")

  # Subset too big
  expect_error(load_file(f, nrow = 150, ncol = 150, clim = c(1, 160)),
               "Subset cannot be bigger than data")

  # Subset incorrect
  for(i in c(1, NA, c("A", "B"))) expect_error(load_file(f, nrow = 150, ncol = 150, clim = i),
                                               "clim and rlim must be each be a vector of two")

  for(i in c(1, NA, c("A", "B"))) expect_error(load_file(f, nrow = 150, ncol = 150, rlim = i),
                                               "clim and rlim must be each be a vector of two")
})
