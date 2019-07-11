context("long test")

test_that("flow_mapper()", {
  skip_on_cran()
  expect_message(flow_mapper(file = system.file("extdata", "testELEV.dbf",
                                                package = "LITAP"),
                             out_folder = "./test_long/",
                             nrow = 150, ncol = 150, report = FALSE))

  for(i in list.files("./test_long/", pattern = ".rds",
                      recursive = TRUE)){
    out <- basename(stringr::str_extract(i, "^[^.]*"))
    o <- readr::read_rds(file.path("./test_long", i))
    if("db" %in% names(o)) o <- o$db
    if(stringr::str_detect(i, "backup")) n <- 150:300 else n <- 1:nrow(o)
      expect_known_output(as.data.frame(o[n,]),
                          file = file.path("./ref/test_long/", out),
                          print = TRUE)
  }
})


test_that("form_mapper()", {
  skip_on_cran()
  expect_message(form_mapper(folder = "./test_long", grid = 5))

  for(i in list.files("./test_long", pattern = "(form|len|relz|weti).rds",
                      recursive = TRUE)){
      out <- basename(stringr::str_extract(i, "^[^.]*"))
      o <- readr::read_rds(file.path("./test_long", i))
      if("db" %in% names(o)) o <- o$db
      if(stringr::str_detect(i, "backup")) n <- 150:300 else n <- 1:nrow(o)
      expect_known_output(as.data.frame(o[n,]),
                          file = file.path("./ref/test_long/", out),
                          print = TRUE)
    }
})
