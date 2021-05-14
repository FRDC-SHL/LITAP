
# Create loading data subsets
xlim <- c(30, 40)
ylim <- c(30, 40)

d <- load_file("../Runs - FlowMapR/Steffi_LandMapR_tests/01_H1/FlowMapR/H10Elev.dbf",
              nrow = 121, ncol = 121, missing_value = -9999,
              rlim = ylim, clim = xlim, edge = FALSE) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV_mini.dbf")

d$ELEV <- as.character(d$ELEV)
foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV_mini_chr.dbf")


# Dem for vignettes
clim <- c(86, 175)
rlim <- c(86, 175)
# clim <- NULL
# rlim <- NULL
d <- load_file("../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/021ELEV.DBF",
               nrow = 184, ncol = 187, missing_value = -9999,
               rlim = rlim, clim = clim, edge = FALSE) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV.dbf")

# Run to get output of subset dem for vignettes
flow_mapper(file = "./inst/extdata/testELEV.dbf", nrow = 90, ncol = 90,
            max_area = 1, max_depth = 0.1)
form_mapper(folder = "./inst/extdata/testELEV", grid = 10,
            str_val = 1000, ridge_val = 1000)
facet_mapper(folder = "./inst/extdata/testELEV",
             arule = system.file("extdata", "arule.dbf", package = "LITAP"),
             crule = system.file("extdata", "crule.dbf", package = "LITAP"))

unlink("./inst/extdata/testELEV/backup/", recursive = TRUE)

