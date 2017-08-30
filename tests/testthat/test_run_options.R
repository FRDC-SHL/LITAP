library(LITAP)
context("Run options")

cont_options <- c("directions", "watersheds", "watershed_area", "local", "pond", "fill",
                  "inverted", "iwatersheds", "iwatershed_area", "ilocal")

test_that("Quiet is quiet and that runs continue and end as they should", {
  for(i in cont_options){
    expect_warning(expect_message(complete_run(file = "testELEV.dbf", nrow = 11, ncol = 11,
                                               verbose = TRUE, continue = i, end = i,
                                               report = FALSE)), NA)

    expect_warning(expect_silent(complete_run(file = "testELEV.dbf", nrow = 11, ncol = 11,
                                              verbose = TRUE, quiet = TRUE, continue = i, end = i,
                                              report = FALSE)), NA)
  }

  f <- list.files(".", full.names = TRUE, recursive = TRUE)
  f <- f[stringr::str_detect(f, "(/backup/)|(/final/)|(/dbf/)")]
  file.remove(f)
  file.remove("./backup/")
  file.remove("./final/")
  file.remove("./dbf/")

})
