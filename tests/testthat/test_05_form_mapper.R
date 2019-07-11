context("form_mapper() Run options")

test_that("Quiet is quiet and that runs resume and end as they should", {

  resume_options <- c("form", "weti", "relief", "length")

  for(i in resume_options){
    expect_warning(expect_message(
      form_mapper(folder = dir,
                  grid = 5,
                  verbose = TRUE,
                  resume = i, end = !!i,
                  report = FALSE, log = FALSE)), NA)

    expect_true(file.exists(file.path(dir, "/backup/", paste0(!!i, ".rds"))))

    expect_warning(expect_silent(
      form_mapper(folder = dir,
                  grid = 5,
                  verbose = TRUE, quiet = TRUE,
                  resume = i, end = !!i,
                  report = FALSE, log = FALSE)), NA)
  }

})
