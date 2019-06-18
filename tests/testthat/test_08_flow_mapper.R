context("Run options")

cont_options <- c("directions", "watersheds", "local", "pond", "fill",
                  "inverted", "iwatersheds", "ilocal")

f <- system.file("extdata", "testELEV_mini.dbf", package = "LITAP")


test_that("Quiet is quiet and that runs resume and end as they should", {
  dir <- tempdir()
  for(i in cont_options){
    #cat(i, "\n")
    expect_warning(expect_message(flow_mapper(f, nrow = 11, ncol = 11,
                                              verbose = TRUE, resume = i, end = i,
                                              report = FALSE, log = FALSE,
                                              folder_out = dir)), NA)

    expect_warning(expect_silent(flow_mapper(f, nrow = 11, ncol = 11,
                                             verbose = TRUE, quiet = TRUE, resume = i, end = i,
                                             report = FALSE, log = FALSE,
                                             folder_out = dir)), NA)
  }

  temp <- list.files(dir, full.names = TRUE, recursive = TRUE)
  unlink(temp, recursive = TRUE)
})
