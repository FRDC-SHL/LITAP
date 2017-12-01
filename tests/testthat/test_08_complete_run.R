context("Run options")

cont_options <- c("directions", "watersheds", "local", "pond", "fill",
                  "inverted", "iwatersheds", "ilocal")

f <- system.file("tests", "testthat", "testELEV.dbf", package = "LITAP")


test_that("Quiet is quiet and that runs continue and end as they should", {
  for(i in cont_options){
    #cat(i, "\n")
    expect_warning(expect_message(complete_run(f, nrow = 11, ncol = 11,
                                               verbose = TRUE, continue = i, end = i,
                                               report = FALSE, log = FALSE)), NA)

    expect_warning(expect_silent(complete_run(f, nrow = 11, ncol = 11,
                                              verbose = TRUE, quiet = TRUE, continue = i, end = i,
                                              report = FALSE, log = FALSE)), NA)
  }

  dir <- system.file("tests", "testthat", package = "LITAP")
  temp <- list.files(system.file("tests", "testthat", package = "LITAP"), full.names = TRUE, recursive = TRUE)
  temp <- temp[stringr::str_detect(temp, "(/backup/)|(/final/)|(/dbf/)")]
  file.remove(temp)
  file.remove(paste0(dir, "/backup/"))
  file.remove(paste0(dir, "/final/"))
  file.remove(paste0(dir, "/dbf/"))
})
