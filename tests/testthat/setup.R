
f <- system.file("extdata", "testELEV_mini.dbf", package = "LITAP")
dir <- test_path("test_functions")

sub_dem <- function(dem, s) {
  dem %>%
    dplyr::slice(s) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 5))
}

# Prep runs for local tests
if(FALSE) {

  flow_mapper(file = file.path("../Runs - FlowMapR/Steffi_LandMapR_tests/",
                               "11_Ab02PV/FlowMapR/021ELEV.DBF"),
              nrow = 184, ncol = 187, max_area = 1, max_depth = 0.1, grid = 1,
              out_folder = "../Runs - LITAP/11_Ab02PV_test", debug = TRUE)

  form_mapper(folder = "../Runs - LITAP/11_Ab02PV_test/",
              str_val = 200, ridge_val = 200,
              verbose = TRUE, debug = TRUE)

}
