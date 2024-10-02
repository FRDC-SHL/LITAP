expect_values <- function(d, type = "grid") {
  expect_s3_class(d, "data.frame")
  expect_type(d$elev, "double")
  expect_type(d$x, "double")
  expect_type(d$y, "double")
  expect_type(d$row, "double")
  expect_type(d$col, "double")
  expect_equal(max(d$row), 711 + 2)
  expect_equal(max(d$col), 611 + 2)
  if(type != "dbf") expect_equal(max(d$x, na.rm = TRUE), 2405620)
  if(type != "dbf") expect_equal(max(d$y, na.rm = TRUE), 7566600)
  if(type == "dbf") expect_equal(max(d$x, na.rm = TRUE), 600)
  if(type == "dbf") expect_equal(max(d$y, na.rm = TRUE), 700)
  expect_equal(max(d$seqno), nrow(d))
  expect_equal(min(d$elev, na.rm = TRUE), 204.5368, tolerance = 0.00001)
  expect_equal(max(d$elev, na.rm = TRUE), 233.1556, tolerance = 0.00001)
  expect_equal(nrow(d), 437069)
}




# Load dem files ----------------------------------------------------------
test_that("simple DBF files prepared", {
  f <- system.file("extdata", "testELEV.dbf", package = "LITAP")

  expect_error(load_file(f), "require 'nrow' and 'ncol' arguments")
  expect_error(load_file(f, nrow = 90, ncol = 90),
               "require 'grid' argument") %>%
    suppressMessages()

  expect_message(d <- load_file(f, nrow = 90, ncol = 90, grid = 1),
                 "Using supplied") %>%
    expect_message("Adding buffer") %>%
    expect_message("Formating grid") %>%
    expect_message("No x/y in file")

  expect_silent(d <- load_file(f, nrow = 90, ncol = 90, grid = 1,
                               verbose = FALSE))
  expect_equal(max(d$row, na.rm = TRUE), 90 + 2)
  expect_equal(max(d$col, na.rm = TRUE), 90 + 2)
  expect_equal(max(d$seqno, na.rm = TRUE), nrow(d))
  expect_equal(max(d$elev, na.rm = TRUE), 679.974)
  expect_equal(min(d$elev, na.rm = TRUE), 669.989)
})


test_that("DBF files load and prep", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files", "FES04Elev.DBF",
                   package = "LITAP")

  expect_silent(d <- load_dem(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("Grid files load and prep", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                    "FES04_9n3_Original_SurferGrid.grd", package = "LITAP")

  expect_silent(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("AscII Grid files load and prep", {
  skip_on_cran()
  skip_on_ci()
  skip("Because of odd warning only when testing")
  f <- system.file("extdata", "test_input_files",
                   "FES04_SuferGridAscII.grd", package = "LITAP")

  # Weird warnings only when testing...?
  expect_warning(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_warning(d <- load_file(f, verbose = FALSE))
  expect_values(d)
})


test_that("ArcGis files load", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                      "FES04_ARCGRID", package = "LITAP")

  expect_silent(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()

  f <- system.file("extdata", "test_input_files",
                   "FES04_ArcAscIIGrid.asc", package = "LITAP")

  expect_silent(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("GeoTif files load", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                   "FES04_GeoTiff.tif", package = "LITAP")

  expect_silent(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("Float Grid files laod", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                   "FloatGrid.flt", package = "LITAP")

  expect_silent(d <- load_raster(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("Text files load", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                   "FES04_XYZGrid.dat", package = "LITAP")

  expect_silent(d <- load_txt(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()

  f <- system.file("extdata", "test_input_files",
                   "FES04_XYZ.dat", package = "LITAP")

  expect_silent(d <- load_txt(f))
  expect_equal(nrow(d), 434421)
  expect_silent(load_file(f, verbose = FALSE)) %>%
    expect_values()
})

test_that("Excel files load", {
  skip_on_cran()
  skip_on_ci()
  f <- system.file("extdata", "test_input_files",
                   "FES04_XYZ.xlsx", package = "LITAP")

  expect_error(d <- load_excel(f), NA)
  expect_equal(nrow(d), 434421)
  expect_error(load_file(f, verbose = FALSE), NA) %>%
    expect_values()
})

test_that("Prep DB correct subset/buffer/edge", {
  f <- system.file("extdata", "testELEV.dbf", package = "LITAP")
  for(i in 1:3) {
    rlim <- sample(1:87, 1)
    rlim <- c(rlim, sample((rlim+2):90, 1))
    clim <- sample(1:87, 1)
    clim <- c(clim, sample((clim+2):90, 1))

    expect_silent(d <- load_file(f, nrow = 90, ncol = 90,
                                 clim = clim, rlim = rlim,
                                 grid = 1, verbose = FALSE))

    rmax <- (rlim[2] - rlim[1] + 1 + 2) # +2 for buffer
    cmax <- (clim[2] - clim[1] + 1 + 2) # +2 for buffer

    # Subset
    expect_s3_class(d, "data.frame")
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
  f <- system.file("extdata", "testELEV.dbf", package = "LITAP")

  # Incorrect nrow/ncol
  for(i in 1:3) {
    expect_error(load_file(f, nrow = sample(1:79, 1), ncol = sample(1:79, 1),
                           grid = 1),
                 "Number of rows and columns does not match the total number of cells") %>%
      expect_message("Using supplied") %>%
      expect_message("Adding buffer") %>%
      expect_message("Formating grid")
  }

  # Subset too small
  expect_error(load_file(f, nrow = 90, ncol = 90, clim = c(1,1), rlim = c(1,1),
                         grid = 1),
               "Subset is too small \\(less than 2x2\\)") %>%
    expect_message("Using supplied") %>%
    expect_message("Subsetting data") %>%
    expect_message("Formating grid") %>%
    expect_message("No x/y in file")

  # Subset too big
  expect_error(load_file(f, nrow = 90, ncol = 90, clim = c(1, 160), grid = 1),
               "Subset cannot be bigger than data") %>%
    expect_message("Using supplied") %>%
    expect_message("Subsetting data") %>%
    expect_message("Formating grid") %>%
    expect_message("No x/y in file")

  # Subset incorrect
  for(i in c(1, NA, c("A", "B"))) {
    expect_error(load_file(f, nrow = 90, ncol = 90, clim = i, grid = 1),
                 "clim and rlim must be each be a vector of two") %>%
      expect_message("Using supplied") %>%
      expect_message("Subsetting data")
  }

  for(i in c(1, NA, c("A", "B"))) {
    expect_error(load_file(f, nrow = 90, ncol = 90, rlim = i, grid = 1),
                 "clim and rlim must be each be a vector of two") %>%
      expect_message("Using supplied") %>%
      expect_message("Subsetting data")
  }
})



# Load previous ------------------------------------------------------------

test_that("Load previous files", {
  skip("no test")
})

# Load rule files ---------------------------------------------------------

test_that("Rule files load", {
  skip("no test")
  # No errors

  # Expected output

  # Catch incorrect rules


})
