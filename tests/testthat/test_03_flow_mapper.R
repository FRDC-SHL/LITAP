test_that("Quiet is quiet", {

  flow_mapper(f, nrow = 11, ncol = 11, grid = 5,
              quiet = TRUE, report = FALSE, log = FALSE, out_folder = dir) %>%
    expect_silent()
})

test_that("Runs resume and as they should", {

  unlink(dir)

  resume_options <- c("directions", "watersheds", "local", "pond", "fill",
                      "slope", "idirections", "iwatersheds", "inverted")

  for(i in resume_options) {

    flow_mapper(f, nrow = 11, ncol = 11, grid = 5,
                verbose = TRUE, resume = i, debug = TRUE,
                report = FALSE, log = FALSE,
                out_folder = dir) %>%
      expect_message("CALCULATING") %>%
      suppressMessages()

  }

})


test_that("slope values in flow data", {
  s <- readRDS("./test_functions/flow/dem_fill.rds")

  expect_true(all(c("sgre", "sgr", "sgcn", "sgc", "scr", "scc",
                    "hill_r_dir", "hill_c_dir", "hill_r_n") %in% names(s)))
})


test_that("columns ordered as expected", {
  s <- readRDS("./test_functions/flow/dem_fill.rds")

  expect_named(s, c("seqno", "x", "y", "row", "col", "elev", "ddir", "drec",
                    "upslope", "upslope_m",
                    "vol2fl", "mm2fl", "parea",
                    "initial_shed", "local_shed", "pond_shed", "fill_shed",
                    "sgre", "sgr", "sgcn", "sgc", "scr", "scc",
                    "hill_r_dir", "hill_c_dir", "hill_r_n", "hill_r_cell",
                    "hill_c_n", "hill_c_cell", "edge_map", "ridge"))
})

