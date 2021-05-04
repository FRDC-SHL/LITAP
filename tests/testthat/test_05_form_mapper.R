test_that("Quiet is quiet and that runs resume and end as they should", {

  resume_options <- c("form", "weti", "relief", "length")

  for(i in resume_options){
    suppressMessages(
      expect_warning(expect_message(
        form_mapper(folder = dir,
                    grid = 5,
                    verbose = TRUE,
                    resume = !!i, end = !!i,
                    log = FALSE)), NA))

    expect_warning(expect_silent(
      form_mapper(folder = dir,
                  grid = 5,
                  verbose = TRUE, quiet = TRUE,
                  resume = !!i, end = !!i,
                  log = FALSE)), NA)
  }

})
