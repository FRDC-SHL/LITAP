context("Long test")

test_that("Full example runs", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = "./test_long",
                             nrow = 150, ncol = 150))

  for(i in list.files("./test_long/", pattern = ".rds",
                      recursive = TRUE)){
    expect_known_output(readr::read_rds(file.path("./test_long/", i)),
                        file = file.path("./ref/test_long/", i))
  }

  unlink("./test_long", recursive = TRUE)

})
