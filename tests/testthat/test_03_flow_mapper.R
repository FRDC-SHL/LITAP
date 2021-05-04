test_that("Quiet is quiet and that runs resume and end as they should", {

  resume_options <- c("directions", "watersheds", "local", "pond", "fill",
                      "slope",
                      "inverted", "iwatersheds", "ilocal")

  for(i in resume_options){
    suppressMessages(
      expect_message(flow_mapper(f, nrow = 11, ncol = 11,
                                 verbose = TRUE, resume = i, end = i,
                                 report = FALSE, log = FALSE,
                                 out_folder = dir), "CALCULATING") %>%
        expect_warning(NA)
    )

    expect_silent(flow_mapper(f, nrow = 11, ncol = 11,
                              verbose = FALSE, quiet = TRUE, resume = i, end = i,
                              report = FALSE, log = FALSE,
                              out_folder = dir)) %>%
      expect_warning(NA)
  }

})


test_that("slope values in flow data", {
  s <- readRDS("./test_functions/flow/dem_fill.rds")

  expect_true(all(c("sgre", "sgr", "sgcn", "sgc", "scr", "scc",
                    "hill_r_dir", "hill_c_dir", "hill_r_n") %in% names(s)))
})
