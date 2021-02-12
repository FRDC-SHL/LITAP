context("form_mapper() sub-routines")

test_that("calc_stream", {

  dir <- "/home/steffi/Projects/Business/LandmapR/Runs - LITAP/11_Ab02PV/"

  skip_if(!file.exists(file.path(dir, "form", "dem_relief.rds")))

  # DB files

  expect_silent(db <- get_previous(dir, step = "fill", where = "flow") %>%
                  dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
                  add_buffer()) %>%
    expect_is("data.frame")

  expect_silent({
    pond <- get_previous(dir, step = "pond", type = "stats", where = "flow")
    streams <- db %>%
      dplyr::mutate(shedno = fill_shed)
  })

  expect_silent({
    system.time(s1 <- calc_stream(streams, str_val = 1000, verbose = FALSE))
    system.time(s2 <- calc_stream2(streams, str_val = 1000, verbose = FALSE))
    system.time(s3 <- calc_stream3(streams, str_val = 1000, verbose = FALSE))
  })

  expect_true(dplyr::all_equal(s1, s2))
  expect_true(dplyr::all_equal(s2, s3))

  str2pits <- db %>%
    dplyr::mutate(shedno = local_shed)

  expect_silent({
    system.time(c1 <- calc_pit(str2pits, pond = pond, verbose = FALSE))
    system.time(c2 <- calc_pit2(str2pits, pond = pond, verbose = FALSE))
    system.time(c3 <- calc_pit3(str2pits, pond = pond, verbose = FALSE))
  })

  expect_true(dplyr::all_equal(c1, c2))
  expect_true(dplyr::all_equal(c2, c3))



  db2 <- get_previous("/home/steffi/Projects/Business/LandmapR/Runs - LITAP/Munger Test - LITAP/m35ELEV/",
                     step = "fill", where = "flow") %>%
    dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
    add_buffer()
  grid <- 10
  expect_silent(calc_weti(db2[1:2000,], grid, verbose = FALSE))

})

test_that("Sub-functions", {
  flow_mapper(f, nrow = 11, ncol = 11, out_folder = dir, report = FALSE)
  grid <- 5

  # DB files
  expect_silent(db <- get_previous(dir, step = "fill", where = "flow")) %>%
    expect_is("data.frame")
  db <- add_buffer(db)

  expect_silent(idb <- get_previous(dir, step = "ilocal", where = "flow")) %>%
    expect_is("data.frame")
  idb <- add_buffer(idb)

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
