context("long test")

test_that("flow_mapper()", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = ".",
                             nrow = 150, ncol = 150))

  for(i in list.files("./test_long/", pattern = ".rds",
                      recursive = TRUE)){
    expect_known_output(readr::read_rds(file.path("./testELEV", i)),
                        file = file.path("./ref/test_long/", i))
  }
})


test_that("form_mapper()", {
  skip_on_cran()
  expect_message(form_mapper(folder = "./testELEV", grid = 5))

  for(i in list.files("./testELEV", pattern = "(form|len|relz|weti).rds",
                      recursive = TRUE)){
    expect_known_output(readr::read_rds(file.path("./testELEV", i)),
                        file = file.path("./ref/test_long/", i))
  }

  unlink("./testELEV", recursive = TRUE)
})
