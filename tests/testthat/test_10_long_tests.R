test_that("flow_mapper()", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = "./test_long/",
                             nrow = 90, ncol = 90, report = FALSE)) %>%
    expect_message("CALCULATING WATERSHEDS") %>%
    expect_message("REMOVING INITIAL PITS") %>%
    expect_message("CALCULATING POND \\(GLOBAL\\) WATERSHEDS") %>%
    expect_message("CALCULATING FILL PATTERNS") %>%
    expect_message("INVERTING DEM") %>%
    expect_message("CALCULATING INVERTED DIRECTIONS") %>%
    expect_message("CALCULATING INVERTED WATERSHEDS") %>%
    expect_message("REMOVING INVERTED INITIAL PITS") %>%
    expect_message("SAVING OUTPUT") %>%
    expect_message("SKIPPING CREATING REPORT") %>%
    expect_message("Run took:")
})

test_that("form_mapper()", {
  skip_on_cran()
  expect_message(form_mapper(folder = "./test_long/", grid = 5)) %>%
    expect_message("CALCULATING FORM") %>%
    expect_message("CALCULATING WETNESS INDICES") %>%
    expect_message("CALCULATING RELIEF DERIVITIVES") %>%
    expect_message("CALCULATING SLOPE LENGTH") %>%
    expect_message("Run took:")
})

test_that("flow_mapper() output", {
  skip_on_cran()

  set.seed(4444)
  s <- sample(1:8400, size = 500)

  step <- "flow"

  for(i in list.files(glue::glue("./test_long/{step}/"), pattern = "*.rds",
                      full.names = TRUE, recursive = TRUE)){

    # Subsample dem files (HUGE!)
    if(stringr::str_detect(i, "dem_[a-z]+.rds$")) {
      readRDS(i) %>%
        dplyr::slice(s) %>%
        saveRDS(i)
    }

    p <- file.path(glue::glue("./test_long/{step}/"), basename(i))
    if(interactive()) message(p) else expect_snapshot_file(path = p)
  }
})

test_that("form_mapper() output", {
  skip_on_cran()

  set.seed(4444)
  s <- sample(1:8400, size = 500)

  step <- "form"

  for(i in list.files(glue::glue("./test_long/{step}/"),
                      pattern = "(form|length|relz|weti)[.]*.rds",
                      full.names = TRUE, recursive = TRUE)){

    # Subsample dem files (HUGE!)
    if(stringr::str_detect(i, "dem_[a-z]+.rds$")) {
      readRDS(i) %>%
        dplyr::slice(s) %>%
        saveRDS(i)
    }

    p <- file.path(glue::glue("./test_long/{step}/"), basename(i))
    if(interactive()) message(p) else expect_snapshot_file(path = p)
  }
})
