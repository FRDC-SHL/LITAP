test_that("fuzc_sum()", {
  skip("test")
  crule <- load_extra("../TestFiles/Munger Test - LITAP/LM3CRULE.dbf",
                      type = "crule") %>%
    format_rule(type = "crule") %>%
    rbind(., .) %>%
    dplyr::mutate(zone = c(rep(0, 99), rep(1, 99))) %>%
    prep_rule(type = "crule")


  fuzzattr <- get_previous("../Runs - FlowMapR/Munger Test - LITAP/m35ELEV/",
                           step = "fuza", where = "facet") %>%
    add_buffer()


})
