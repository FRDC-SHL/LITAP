test_that("flow_mapper()", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = "./test_long/",
                             nrow = 150, ncol = 150, report = FALSE)) %>%
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

  for(i in list.files("./test_long/", pattern = ".rds",
                      recursive = TRUE)){
    expect_snapshot_file(path = file.path("./test_long", i))
  }
})


test_that("form_mapper()", {
  skip_on_cran()
  expect_message(form_mapper(folder = "./test_long", grid = 5)) %>%
    expect_message("CALCULATING FORM") %>%
    expect_message("CALCULATING WETNESS INDICES") %>%
    expect_message("CALCULATING RELIEF DERIVITIVES") %>%
    expect_message("CALCULATING SLOPE LENGTH") %>%
    expect_message("Run took:")

  for(i in list.files("./test_long", pattern = "(form|length|relz|weti)[.]*.rds",
                      recursive = TRUE)){
    expect_snapshot_file(path = file.path("./test_long", i))
    }
})
