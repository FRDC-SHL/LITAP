
xlim <- c(30, 40)
ylim <- c(30, 40)

d <- foreign::read.dbf("../TestFiles/01_H1/FlowMapR/H10Elev.dbf") %>%
  file_prep(nrow = 121, ncol = 121, missing_value = -9999) %>%
  dplyr::filter(row >= rlim[1] & row <= rlim[2] & col >= clim[1] & col <= clim[2]) %>%
  dplyr::mutate(seqno = 1:length(seqno),
                row = row - min(row) + 1,
                col = col - min(col) + 1) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./tests/testELEV.dbf")

# Subset Dem for vignettes
clim <- c(26, 175)
rlim <- c(26, 175)
d <- foreign::read.dbf("../TestFiles/11_Ab02PV/FlowMapR/021ELEV.DBF") %>%
  file_prep(nrow = 184, ncol = 187, missing_value = -9999) %>%
  dplyr::filter(row >= rlim[1] & row <= rlim[2] & col >= clim[1] & col <= clim[2]) %>%
  dplyr::mutate(seqno = 1:length(seqno),
                row = row - min(row) + 1,
                col = col - min(col) + 1) %>%
  dplyr::select(ELEV = elev)

foreign::write.dbf(as.data.frame(d), "./inst/extdata/testELEV.dbf")

# Run to get output of subset dem for vignettes
complete_run(file = "./inst/extdata/testELEV.dbf", nrow = 150, ncol = 150,
             max_area = 1, max_depth = 0.1, continue = "local", end = "local")
