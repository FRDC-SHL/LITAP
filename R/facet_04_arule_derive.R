arule_percentiles <- function(weti, relief, edge_row, edge_col) {
  weti %>%
    dplyr::left_join(dplyr::select(relief,
                                   "seqno", "pctz2st", "pctz2pit", "z2pit"),
                     by = "seqno") %>%
    dplyr::filter(.data$row > (!!edge_row + 1),
                  .data$row < (max(.data$row) - (!!edge_row + 1)),
                  .data$col > (!!edge_col + 1),
                  .data$col < (max(.data$col) - (!!edge_col + 1))) %>%
    dplyr::select("prof", "plan", "slope" = "slope_pct", "qweti",
                  "pctz2st", "pctz2pit", "z2pit") %>%
    dplyr::summarize(
      n = dplyr::n(),
      dplyr::across(
        .fns = list(p90 = ~stats::quantile(., 0.90, na.rm = TRUE),
                    p75 = ~stats::quantile(., 0.75, na.rm = TRUE),
                    p70 = ~stats::quantile(., 0.70, na.rm = TRUE),
                    p65 = ~stats::quantile(., 0.65, na.rm = TRUE),
                    p50 = ~stats::quantile(., 0.50, na.rm = TRUE),
                    p35 = ~stats::quantile(., 0.35, na.rm = TRUE),
                    p25 = ~stats::quantile(., 0.25, na.rm = TRUE),
                    p10 = ~stats::quantile(., 0.10, na.rm = TRUE))))
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
    dplyr::select(-"n") %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "percentile", values_to = "value") %>%
    tidyr::separate(percentile, into = c("parameter", "percentile"),
                    sep = "_", remove = TRUE) %>%
    tidyr::pivot_wider(names_from = "percentile", values_from = "value") %>%
    dplyr::arrange(factor(
      .data$parameter, levels = c("n", unique(tolower(arule_template()$attr_in))))) %>%
    dplyr::select("parameter", paste0("p", c(10, 25, 50, 65, 70, 75, 90)))
}
