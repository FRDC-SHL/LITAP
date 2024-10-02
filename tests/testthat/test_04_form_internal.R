test_that("calc_stream", {

  dir <- "/home/steffi/Projects/Business/LandmapR/Runs - LITAP/11_Ab02PV/"

  skip_if(!file.exists(file.path(dir, "form", "dem_relief.rds")))

  # DB files

  expect_silent(db <- get_previous(dir, where = "flow", step = "fill") %>%
                  dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
                  add_buffer()) %>%
    expect_s3_class("data.frame")

  expect_silent({
    pond <- get_previous(dir, where = "flow", step = "pond", type = "stats")
    streams <- db %>%
      dplyr::mutate(shedno = fill_shed)
  })

  expect_silent({
    system.time(s1 <- calc_stream(streams, str_val = 1000, verbose = FALSE))
    system.time(s2 <- calc_stream2(streams, str_val = 1000, verbose = FALSE))
    system.time(s3 <- calc_stream3(streams, str_val = 1000, verbose = FALSE))
  })

  expect_equal(s1, s2)
  expect_equal(s2, s3)

  str2pits <- db %>%
    dplyr::mutate(shedno = local_shed)

  expect_silent({
    system.time(c1 <- calc_pit(str2pits, pond = pond, verbose = FALSE))
    system.time(c2 <- calc_pit2(str2pits, pond = pond, verbose = FALSE))
    system.time(c3 <- calc_pit3(str2pits, pond = pond, verbose = FALSE))
  })

  expect_equal(c1, c2)
  expect_equal(c2, c3)



  db2 <- get_previous(paste0("/home/steffi/Projects/Business/LandmapR/Runs - ",
                             "LITAP/Munger Test - LITAP/m35ELEV/"),
                      where = "flow", step = "fill") %>%
    dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
    add_buffer()
  grid <- 10
  expect_silent(calc_weti(db2[1:2000,], grid, verbose = FALSE))

})

test_that("Sub-functions", {
  suppressMessages(flow_mapper(f, nrow = 11, ncol = 11, grid = 5,
                               out_folder = dir, report = FALSE, clean = TRUE))

  # DB files
  expect_silent(db <- get_previous(dir, where = "flow", step = "fill")) %>%
    expect_s3_class("data.frame")
  db <- add_buffer(db)

  expect_silent(idb <- get_previous(dir, where = "flow", step = "inverted")) %>%
    expect_s3_class("data.frame")
  idb <- add_buffer(idb)

  # Form
  expect_silent(d <- calc_form(db, grid = 5))
  expect_snapshot_output(d)

  # Weti
  expect_silent(d <- calc_weti(db, grid = 5, verbose = FALSE))
  expect_snapshot_output(d)

  # Relief
  expect_silent(db_relz <- calc_relz(db, idb, str_val = 10000, ridge_val = 1000,
                                     verbose = FALSE))
  expect_snapshot_output(db_relz)

  # Length
  expect_silent(d <- calc_length(db, db_relz, grid = 5, verbose = FALSE))
  expect_snapshot_output(d)
})
