
# Create loading data subsets
xlim <- c(30, 40)
ylim <- c(30, 40)

d <- load_file("../TestFiles/Li_FlowMapR_tests/01_H1/FlowMapR/H10Elev.dbf",
              nrow = 121, ncol = 121, missing_value = -9999,
              rlim = ylim, clim = xlim, edge = FALSE) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV_mini.dbf")

# Dem for vignettes
clim <- c(26, 175)
rlim <- c(26, 175)
# clim <- NULL
# rlim <- NULL
d <- load_file("../TestFiles/Li_FlowMapR_tests/11_Ab02PV/FlowMapR/021ELEV.DBF",
               nrow = 184, ncol = 187, missing_value = -9999,
               rlim = rlim, clim = clim, edge = FALSE) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV.dbf")

# Run to get output of subset dem for vignettes
flow_mapper(file = "./inst/extdata/testELEV.dbf", nrow = 150, ncol = 150,
            max_area = 1, max_depth = 0.1)

