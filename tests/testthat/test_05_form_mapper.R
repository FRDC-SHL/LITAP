test_that("Quiet is quiet", {
  form_mapper(folder = dir,
              verbose = TRUE, quiet = TRUE,
              log = FALSE) %>%
  expect_silent()
})

test_that("Runs resume as they should", {

  resume_options <- c("form", "weti", "relief", "length")

  for(i in resume_options){
    form_mapper(folder = dir,
                verbose = TRUE,
                resume = i, debug = TRUE,
                log = FALSE) %>%
      expect_message() %>%
      suppressMessages()
  }

})
