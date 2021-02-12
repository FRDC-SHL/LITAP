fix_names <- list("elev" = c("^elevation$" = "elev",
                             "^z$" = "elev",
                             "^northing$" = "y",
                             "^latitude$" = "y",
                             "^lat$" = "y",
                             "^easting$" = "x",
                             "^longitude$" = "x",
                             "^lon$" = "x",
                             "^long$" = "x",
                             "^gridcode$" = "zone",
                             "^code$" = "zone",
                             "^ecozone$" = "zone",
                             "^bec_zone$" = "zone",
                             "^beczone$" = "zone",
                             "^bec$" = "zone",
                             "^dss$" = "zone"),
                  "zone" = c("^id$" = "seqno",
                             "^cell$" = "seqno",
                             "^gridcode$" = "zone",
                             "^code$" = "zone",
                             "^ecozone$" = "zone",
                             "^bec_zone$" = "zone",
                             "^beczone$" = "zone",
                             "^bec$" = "zone",
                             "^dss$" = "zone"))
match_names <- dplyr::tribble(
  ~type, ~name, ~required,

  "elev", "x", FALSE,
  "elev", "y", FALSE,
  "elev", "elev", TRUE,

  "arule", "sortorder", TRUE,
  "arule", "file_in", TRUE,
  "arule", "attr_in", TRUE,
  "arule", "class_out", TRUE,
  "arule", "model_no", TRUE,
  "arule", "b", TRUE,
  "arule", "b_low", TRUE,
  "arule", "b_hi", TRUE,
  "arule", "b1",  TRUE,
  "arule", "b2",  TRUE,
  "arule", "d",  TRUE,
  "arule", "zone", FALSE,

  "crule", "f_name", TRUE,
  "crule", "fuzattr", TRUE,
  "crule", "attrwt", TRUE,
  "crule", "facet_no", TRUE,
  "crule", "f_code", TRUE,
  "crule", "zone", FALSE,

  "zone", "seqno", TRUE,
  "zone", "zone", TRUE)

arule_weti <- c("prof", "plan", "slope_pct", "aspect", "qweti",
                "qarea", "lnqarea", "new_asp")
arule_relief <- c("pctz2st", "pctz2pit", "z2pit")

usethis::use_data(fix_names, match_names, arule_weti, arule_relief,
                  internal = TRUE, overwrite = TRUE)

