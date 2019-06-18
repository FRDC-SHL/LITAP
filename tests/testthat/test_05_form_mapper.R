context("form_mapper() Run options")

resume_options <- c("form", "weti", "relief", "length")

f <- system.file("extdata", "testELEV_mini.dbf", package = "LITAP")
dir <- "./test_form"
dir.create(dir)
flow_mapper(f, nrow = 11, ncol = 11, out_folder = dir)

test_that("Quiet is quiet and that runs resume and end as they should", {

  for(i in resume_options){
    expect_warning(expect_message(
      form_mapper(folder = file.path(dir, "testELEV_mini"),
                  grid = 5,
                  verbose = TRUE,
                  resume = i, end = i,
                  report = FALSE, log = FALSE)), NA)

    expect_warning(expect_silent(
      form_mapper(folder = file.path(dir, "testELEV_mini"),
                  grid = 5,
                  verbose = TRUE, quiet = TRUE,
                  resume = i, end = i,
                  report = FALSE, log = FALSE)), NA)
  }
  unlink(dir, recursive = TRUE)
})

