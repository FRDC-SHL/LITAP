
f <- system.file("extdata", "testELEV_mini.dbf", package = "LITAP")
dir <- "./test_functions"

sub_dem <- function(dem, s) {
  dem %>%
    dplyr::slice(s) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 5))
}
