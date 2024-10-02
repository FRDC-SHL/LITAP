test_that("Missing arule and crule files", {

  system.file("extdata", "testELEV", package = "LITAP") %>%
    facet_mapper(arule = "", crule = "") %>%
    expect_error("arule file \\(''\\) doesn't exist")

  system.file("extdata", "testELEV", package = "LITAP") %>%
    facet_mapper(arule = system.file("extdata", "arule.dbf", package = "LITAP"),
                 crule = "") %>%
    expect_message() %>%
    suppressMessages() %>%
    expect_error("crule file \\(''\\) doesn't exist")
})

test_that("fuzc_sum()", {
  skip("test")
  crule <- load_extra("../TestFiles/Munger Test - LITAP/LM3CRULE.dbf",
                      type = "crule") %>%
    format_rule(type = "crule") %>%
    rbind(., .) %>%
    dplyr::mutate(zone = c(rep(0, 99), rep(1, 99))) %>%
    prep_rule(type = "crule")


  fuzzattr <- get_previous("../Runs - FlowMapR/Munger Test - LITAP/m35ELEV/",
                           where = "facet", step = "fuza") %>%
    add_buffer()


})

test_that("arule file create", {



})
