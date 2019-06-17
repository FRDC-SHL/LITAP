context("Long test")

test_that("Full example runs", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             folder_out = "./test_long",
                             nrow = 150, ncol = 150))

  t <- list.files("./test_long", recursive = TRUE, full.names = TRUE)
  expect_length(t, 42)
  file.remove(t)
  file.remove("./test_long/backup/", "./test_long/dbf", "./test_long/final")
  file.remove("./test_long")

})
