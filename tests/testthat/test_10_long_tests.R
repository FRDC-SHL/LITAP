test_that("flow_mapper()", {
  skip_on_cran()
  set.seed(4444)
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = "./test_long/",
                             nrow = 90, ncol = 90, grid = 5, report = FALSE)) %>%
    expect_message("CALCULATING WATERSHEDS") %>%
    expect_message("REMOVING INITIAL PITS") %>%
    expect_message("CALCULATING POND \\(GLOBAL\\) WATERSHEDS") %>%
    expect_message("CALCULATING FILL PATTERNS") %>%
    expect_message("INVERTING DEM") %>%
    expect_message("CALCULATING INVERTED DIRECTIONS") %>%
    expect_message("CALCULATING INVERTED WATERSHEDS") %>%
    expect_message("REMOVING INVERTED PITS") %>%
    expect_message("SKIPPING CREATING REPORT") %>%
    expect_message("Run took:")
})

test_that("form_mapper()", {
  skip_on_cran()
  set.seed(4444)
  expect_message(form_mapper(folder = "./test_long/")) %>%
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

  for(i in list.files("./test_long/flow/", pattern = "*.rds",
                      full.names = TRUE, recursive = TRUE)){
    readRDS(i) %>%
      sub_dem(s) %>%
      expect_snapshot_value(style = "json2")
  }
})

test_that("form_mapper() output", {
  skip_on_cran()

  set.seed(4444)
  s <- sample(1:8400, size = 500)

  for(i in list.files("./test_long/form/",
                      pattern = "(form|length|relz|weti)[.]*.rds",
                      full.names = TRUE, recursive = TRUE)){

    readRDS(i) %>%
      sub_dem(s) %>%
      dplyr::select(-dplyr::any_of(c("cell_t", "count_d", "count_o", "elev_sum"))) %>%
      expect_snapshot_value(style = "json2")
  }
})
