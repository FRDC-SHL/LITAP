lsm_fuza <- function(weti = weti, relief = relief, arule = arule) {

  fuzzattr <- dplyr::select(weti, "seqno", "row", "col", "buffer",
                            "prof", "plan", "slope_pct", "aspect", "qweti",
                            "qarea", "lnqarea", "new_asp") %>%
    dplyr::left_join(dplyr::select(relief, "seqno", "row", "col", "buffer",
                                   "pctz2st", "pctz2pit", "z2pit"),
                     by = c("seqno", "row", "col", "buffer"))
  # ETC.!!!! ADD IN EXTRA COLS

  for(a in seq_len(nrow(arule))) {
    fuzzattr[[arule$class_out[a]]] <- arule_models(
      model = 1,
      x = fuzzattr[[arule$attr_in[a]]],
      b = arule$b[a],
      b_low = arule$b_low[a], b_hi = arule$b_hi[a],
      b1 = arule$b1[a], b2 = arule$b2[a],
      d = arule$d[a])
  }

  if(all(c("planar_d", "planar_a") %in% names(fuzzattr))) {
    fuzzattr <- dplyr::mutate(fuzzattr,
                              planar_2x = (.data$planar_d + .data$planar_a)/2)
  }
  fuzzattr
}

lsm_fuzc <- function(fuzzattr, crule) {

  crule <- crule %>%
    dplyr::group_by(.data$f_name) %>%
    dplyr::mutate(relwt = attrwt / sum(attrwt)) %>%
    dplyr::ungroup() %>%
    dplyr::select(f_name, fuzattr, relwt)

  fuzzval <- dplyr::select(fuzzattr, "seqno")

  fuzzattr <- fuzzattr %>%
    dplyr::select("seqno", tidyselect::all_of(unique(crule$fuzattr))) %>%
    tidyr::pivot_longer(cols = c(-"seqno"),
                        names_to = "fuzattr", values_to = "value")


  f <- dplyr::inner_join(fuzzattr, crule, by = "fuzattr") %>%
    dplyr::group_by(seqno, f_name) %>%
    dplyr::summarize(value = sum(value * relwt))

  max_f <- f %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(max_facet = dplyr::nth(.data$f_name, n = -1, order_by = .data$value),
                     max_2nd_facet = dplyr::nth(.data$f_name, n = -2, order_by = .data$value))

  f %>%
    dplyr::full_join(max_f, by = "seqno") %>%
    tidyr::pivot_wider(names_from = "f_name", values_from = "value")
}
