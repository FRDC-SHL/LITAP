test_that("output formats chains - csv", {
  unlink("./testELEV/")

  ext <- "csv"

  # flow
  expect_silent(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                               package = "LITAP"),
                            out_folder = "./testELEV/", nrow = 90, ncol = 90,
                            grid = 5,
                            quiet = TRUE, clean = TRUE, out_format = ext))
  expect_true(all(stringr::str_detect(list.files("./testELEV/flow"),
                                      paste0(".", !!ext, "$"))))

  # form
  expect_silent(form_mapper(folder = "./testELEV/", quiet = TRUE, clean = TRUE))
  expect_true(all(stringr::str_detect(list.files("./testELEV/form"),
                                      paste0(".", !!ext, "$"))))

  # facet
  crule <- system.file("extdata", "crule.dbf", package = "LITAP")
  expect_silent(facet_mapper(folder = "./testELEV/", clean = TRUE,
                             arule = NULL, crule = crule, quiet = TRUE))
  expect_true(all(stringr::str_detect(list.files("./testELEV/facet"),
                                      paste0(".", !!ext, "$"))))

  # wepp
  expect_silent(wepp_mapper(folder = "./testELEV/", quiet = TRUE, clean = TRUE))
  expect_true(all(stringr::str_detect(list.files("./testELEV/wepp"),
                                      paste0(".", !!ext, "$"))))

  unlink("./testELEV/")

})
