arule_percentiles <- function(weti, relief, edge_row, edge_col, quiet) {

  # Calculate buffers at 5% if not provided
  if(is.null(edge_row)) {
    edge_row <- round(length(unique(weti$row[!weti$buffer])) * 0.05)
  }
  if(is.null(edge_col)) {
    edge_col <- round(length(unique(weti$col[!weti$buffer])) * 0.05)
  }

  if(!quiet) message("Using buffer of ", edge_row, " rows ('edge_row') ",
                     "and ", edge_col, " cols ('edge_col') per side")

    weti %>%
    dplyr::left_join(dplyr::select(relief,
                                   "seqno", "pctz2st", "pctz2pit", "z2pit",
                                   "z2st", "zpit2peak", "zcr2st", "lpit2peak",
                                   "lstr2div"),
                     by = "seqno") %>%
    dplyr::filter(.data$row > (!!edge_row + 1),
                  .data$row < (max(.data$row) - (!!edge_row + 1)),
                  .data$col > (!!edge_col + 1),
                  .data$col < (max(.data$col) - (!!edge_col + 1))) %>%
    dplyr::select("elev", "prof", "plan", "slope" = "slope_pct", "aspect",
                  "qarea", "qweti", "z2st", "z2pit", "zpit2peak", "zcr2st",
                  "pctz2st", "pctz2pit", "lpit2peak", "lstr2div") %>%
    dplyr::summarize(
      #n = sum(!is.na(.data$elev)),
      dplyr::across(
        .cols = -"elev",
        .fns = list(p99 = ~stats::quantile(., 0.99, na.rm = TRUE),
                    p95 = ~stats::quantile(., 0.95, na.rm = TRUE),
                    p90 = ~stats::quantile(., 0.90, na.rm = TRUE),
                    p85 = ~stats::quantile(., 0.85, na.rm = TRUE),
                    p80 = ~stats::quantile(., 0.80, na.rm = TRUE),
                    p75 = ~stats::quantile(., 0.75, na.rm = TRUE),
                    p70 = ~stats::quantile(., 0.70, na.rm = TRUE),
                    p65 = ~stats::quantile(., 0.65, na.rm = TRUE),
                    p60 = ~stats::quantile(., 0.60, na.rm = TRUE),
                    p55 = ~stats::quantile(., 0.55, na.rm = TRUE),
                    p50 = ~stats::quantile(., 0.50, na.rm = TRUE),
                    p45 = ~stats::quantile(., 0.45, na.rm = TRUE),
                    p40 = ~stats::quantile(., 0.40, na.rm = TRUE),
                    p35 = ~stats::quantile(., 0.35, na.rm = TRUE),
                    p30 = ~stats::quantile(., 0.30, na.rm = TRUE),
                    p25 = ~stats::quantile(., 0.25, na.rm = TRUE),
                    p20 = ~stats::quantile(., 0.20, na.rm = TRUE),
                    p15 = ~stats::quantile(., 0.15, na.rm = TRUE),
                    p10 = ~stats::quantile(., 0.10, na.rm = TRUE),
                    p05 = ~stats::quantile(., 0.05, na.rm = TRUE),
                    p01 = ~stats::quantile(., 0.01, na.rm = TRUE),
                    avg = ~mean(., na.rm = TRUE),
                    sd  = ~sd(., na.rm = TRUE),
                    min = ~min(., na.rm = TRUE),
                    max = ~max(., na.rm = TRUE),
                    n   = ~sum(!is.na(.)))))
}


arule_derive <- function(perc) {

  arule_template() %>%
    dplyr::mutate(b = c(big_or_min(perc$prof_p90, 0.1),
                        big_or_min(perc$prof_p10, -0.1),
                        0,
                        big_or_min(perc$plan_p90, 0.1),
                        big_or_min(perc$plan_p10, -0.1),
                        0,
                        perc$qweti_p90,
                        perc$qweti_p10,
                        0,
                        perc$slope_p90,
                        100,
                        50,
                        0,
                        100,
                        50,
                        0,
                        perc$z2pit_p90),
                  b_low = c(rep(0, 11), 50, rep(0, 2), 50, rep(0, 2)),
                  b_hi = b_low,
                  d = c(big_or_min((perc$prof_p90 - perc$prof_p65)/2, 0.01),
                        big_or_min((perc$prof_p35 - perc$prof_p10)/2, 0.01),
                        big_or_min((perc$prof_p75 - perc$prof_p25)/2, 0.01),
                        big_or_min((perc$plan_p90 - perc$plan_p65)/2, 0.01),
                        big_or_min((perc$plan_p35 - perc$plan_p10)/2, 0.01),
                        big_or_min((perc$plan_p75 - perc$plan_p25)/2, 0.01),
                        (perc$qweti_p90 - perc$qweti_p50)/2,
                        (perc$qweti_p50 - perc$qweti_p10)/2,
                        big_or_min(perc$slope_p25/2, 0.01),
                        big_or_min((perc$slope_p90 - perc$slope_p25)/2, 0.01),
                        (100 - perc$pctz2st_p75) / 2,
                        (perc$pctz2st_p75 - perc$pctz2st_p25) / 2,
                        perc$pctz2st_p25 / 2,
                        (100 - perc$pctz2pit_p75) / 2,
                        (perc$pctz2pit_p75 - perc$pctz2pit_p25) / 2,
                        perc$pctz2pit_p25 / 2,
                        (perc$z2pit_p90 - perc$z2pit_p70) / 2),
                  b1 = b_calcs(calc, b, d, b_low, b_hi, 1),
                  b2 = b_calcs(calc, b, d, b_low, b_hi, 2)) %>%
    dplyr::relocate("d", .after = dplyr::last_col()) %>%
    dplyr::select(-"calc")

}

big_or_min <- function(val, cutoff){
  dplyr::if_else(abs(val) < abs(cutoff), cutoff, val)
}

b_calcs <- function(calc, b, d, b_low, b_hi, btype) {
  if(btype == 1) x <- dplyr::case_when(calc == "bd1" ~ b - d,
                                       calc == "bd2" ~ 0,
                                       calc == "lhd" ~ b_low - d)
  if(btype == 2) x <- dplyr::case_when(calc == "bd1" ~ 0,
                                       calc == "bd2" ~ b + d,
                                       calc == "lhd" ~ b_hi + d)
  x
}


arule_template <- function() {
  dplyr::tribble(
    ~sortorder, ~file_in,   ~attr_in,   ~class_out,   ~model_no, ~calc,
    1,          "formfile", "PROF",     "CONVEX_D",   4,         "bd1",
    2,          "formfile", "PROF",     "CONCAVE_D",  5,         "bd2",
    3,          "formfile", "PROF",     "PLANAR_D",   1,         "lhd",
    4,          "formfile", "PLAN",     "CONVEX_A",   4,         "bd1",
    5,          "formfile", "PLAN",     "CONCAVE_A",  5,         "bd2",
    6,          "formfile", "PLAN",     "PLANAR_A",   1,         "lhd",
    7,          "formfile", "QWETI",    "HIGH_WI",    4,         "bd1",
    8,          "formfile", "QWETI",    "LOW_WI",     5,         "bd2",
    9,          "formfile", "SLOPE",    "NEAR_LEVEL", 5,         "bd2",
    10,         "formfile", "SLOPE",    "REL_STEEP",  4,         "bd1",
    11,         "relzfile", "PCTZ2ST",  "NEAR_DIV",   4,         "bd1",
    12,         "relzfile", "PCTZ2ST",  "NEAR_HALF",  1,         "lhd",
    13,         "relzfile", "PCTZ2ST",  "NEAR_CHAN",  5,         "bd2",
    14,         "relzfile", "PCTZ2PIT", "NEAR_PEAK",  4,         "bd1",
    15,         "relzfile", "PCTZ2PIT", "NEAR_MID",   1,         "lhd",
    16,         "relzfile", "PCTZ2PIT", "NEAR_PIT",   5,         "bd2",
    17,         "relzfile", "Z2PIT",    "HI_ABOVE",   4,         "bd1")

}

percentiles_format <- function(perc) {
  perc %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "name", values_to = "value") %>%
    tidyr::separate(name, into = c("parameter", "name"),
                    sep = "_", remove = TRUE) %>%
    tidyr::pivot_wider(names_from = "parameter", values_from = "value") %>%
    dplyr::mutate(
      name = stringr::str_replace(.data$name, "^p[0]*([0-9]{1,2})", "\\1%"),
      name = factor(.data$name, levels = c("n", "avg", "sd", "min", "1%",
                                           paste0(seq(5,95,5), "%"),
                                           "99%", "max"))) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::select("name", "slope", "aspect", "prof", "plan",
                  "qarea1" = "qarea", "qweti1" = "qweti",
                  "z2st", "z2pit", "zpit2peak", "zcr2st", 'pctz2st', "pctz2pit",
                  "lpit2peak", "lstr2div")
}
