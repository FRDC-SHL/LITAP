context("Loading DEM")
f <- system.file("extdata", "testELEV.dbf", package = "LITAP")

expect_values <- function(d) {
  expect_equal(max(d$row), 711 + 2)
  expect_equal(max(d$col), 611 + 2)
  expect_equal(max(d$seqno), nrow(d))
  expect_equal(min(d$elev, na.rm = TRUE), 204.5368, tolerance = 0.00001)
  expect_equal(max(d$elev, na.rm = TRUE), 233.1556, tolerance = 0.00001)
}


test_that("DEM files load", {
  # skip_on_cran()
  # skip_on_appveyor()
  # skip_on_travis()

  # file <- "../Data/FES4 Input files/FES04Elev.DBF"
  # expect_silent(d <- load_dem(file))

  file <- system.file("extdata", "testELEV.dbf", package = "LITAP")
  expect_silent(d <- load_dem(file))
  expect_is(d, "data.frame")
})
#
# test_that("Grid files load and prep", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FES04_9n3_Original_SurferGrid.grd"
#   expect_silent(d <- load_raster(file))
#   expect_equal(nrow(d), 434421)
#   expect_equivalent(min(d$elev), 204.5368, tolerance = 0.00001)
#   expect_equivalent(max(d$elev), 233.1556, tolerance = 0.00001)
#
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })
#
# #d_raw <- load_raster("../Data/FES4 Input files/FES04_9n3_Original_SurferGrid.grd")
#
# test_that("AscII Grid files load and prep", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FES04_SuferGridAscII.grd"
#   # Weird warning only produced in test_that env ?
#   expect_error(expect_message(d <- load_raster(file), NA), NA)
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#
#   expect_error(expect_message(d <- load_file(file, verbose = FALSE), NA), NA)
#   expect_values(d)
# })
#
#
# test_that("ArcGis files load", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FES04_ARCGRID"
#   expect_silent(d <- load_raster(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)  ## PROBLEM!
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
#
#   file <- "../Data/FES4 Input files/FES04_ArcAscIIGrid.asc"
#   expect_silent(d <- load_raster(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })
#
# test_that("GeoTif files load", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FES04_GeoTiff.tif"
#   expect_silent(d <- load_raster(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })
#
# test_that("Float Grid files laod", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FloatGrid.flt"
#   expect_silent(d <- load_raster(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })
#
# test_that("Text files load", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#
#   file <- "../Data/FES4 Input files/FES04_XYZGrid.dat"
#   expect_silent(d <- load_txt(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
#
#   file <- "../Data/FES4 Input files/FES04_XYZ.dat"
#   expect_silent(d <- load_txt(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })
#
# test_that("Excel files load", {
#   skip_on_cran()
#   skip_on_appveyor()
#   skip_on_travis()
#   file <- "../Data/FES4 Input files/FES04_XYZ.xlsx"
#   expect_silent(d <- load_excel(file))
#   expect_equivalent(d_raw, d, tolerance = 0.00001)
#   expect_silent(d <- load_file(file, verbose = FALSE))
#   expect_values(d)
# })

test_that("DEM files prepared", {
  expect_error(d <- load_file(f))
  expect_message(d <- load_file(f, nrow = 150, ncol = 150))
  expect_silent(d <- load_file(f, nrow = 150, ncol = 150, verbose = FALSE))
  expect_equal(max(d$row, na.rm = TRUE), 150 + 2)
  expect_equal(max(d$col, na.rm = TRUE), 150 + 2)
  expect_equal(max(d$seqno, na.rm = TRUE), nrow(d))
  expect_equal(max(d$elev, na.rm = TRUE), 681.223)
  expect_equal(min(d$elev, na.rm = TRUE), 668.858)
})

test_that("Prep DB correct subset/buffer/edge", {
  for(i in 1:3) {
    rlim <- sample(0:147, 1)
    rlim <- c(rlim, sample((rlim+2):150, 1))
    clim <- sample(0:147, 1)
    clim <- c(clim, sample((clim+2):150, 1))

    expect_silent(d <- load_file(f, nrow = 150, ncol = 150,
                                 clim = clim, rlim = rlim, verbose = FALSE))

    rmax <- (rlim[2] - rlim[1] + 1 + 2) # +2 for buffer
    cmax <- (clim[2] - clim[1] + 1 + 2) # +2 for buffer

    # Subset
    expect_is(d, "data.frame")
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
