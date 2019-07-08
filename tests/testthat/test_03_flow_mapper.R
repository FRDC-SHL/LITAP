context("flow_mapper() run options")

test_that("Quiet is quiet and that runs resume and end as they should", {

  resume_options <- c("directions", "watersheds", "local", "pond", "fill",
                      "inverted", "iwatersheds", "ilocal")

  for(i in resume_options){
    expect_warning(expect_message(flow_mapper(f, nrow = 11, ncol = 11,
                                              verbose = TRUE, resume = i, end = i,
                                              report = FALSE, log = FALSE,
                                              out_folder = dir)), NA)

    expect_warning(expect_silent(flow_mapper(f, nrow = 11, ncol = 11,
                                             verbose = TRUE, quiet = TRUE, resume = i, end = i,
                                             report = FALSE, log = FALSE,
                                             out_folder = dir)), NA)
  }

})
