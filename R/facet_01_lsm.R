prep_rule <- function(rule, type = "crule") {
  if(type == "crule") {
    rule <- rule %>%
      dplyr::group_by(.data$f_name, .data$zone) %>%
      dplyr::mutate(relwt = .data$attrwt / sum(.data$attrwt)) %>%
      dplyr::ungroup() %>%
      dplyr::select("zone", "f_name", "fuzattr", "relwt")
  }
  rule
}

get_attr <- function(weti, relief, arule) {
  attr <- dplyr::select(weti, "seqno", "zone",
                        tidyselect::all_of(arule_weti)) %>%
    dplyr::left_join(
      dplyr::select(relief, "seqno", tidyselect::all_of(arule_relief)),
      by = "seqno")
}


lsm_fuza <- function(attr, arule, procedure) {

  # Create holder data
  fuzzattr <- dplyr::select(attr, "seqno", "new_asp")
  # Calculate fuzzy attributes for each cell
  for(a in seq_len(nrow(arule))) {
    f <- dplyr::filter(attr, .data$zone == arule$zone[a])
    f <- dplyr::mutate(
      f,
      !! arule$class_out[a] := arule_models(
        model = !!arule$model_no[a],
        x = !!rlang::sym(arule$attr_in[a]),
        b = !!arule$b[a],
        b_low = !!arule$b_low[a], b_hi = !!arule$b_hi[a],
        b1 = !!arule$b1[a], b2 = !!arule$b2[a],
        d = !!arule$d[a])) %>%
      dplyr::select("seqno", "zone", tidyselect::any_of(arule$class_out[a]))

    #f$prof[8284] # 9.411 vs. 10.085

    # arule_models(x = 10.085, model = 4, b = 14.3190, b_low = 0,
    #              b_hi = 0, b1 = 9.17350, b2 = 0, d = 5.14550)


    fuzzattr[f$seqno, names(f)] <- f
  }

  if(all(c("planar_d", "planar_a") %in% names(fuzzattr))) {
    fuzzattr <- dplyr::mutate(fuzzattr,
                              planar_2x = (.data$planar_d + .data$planar_a)/2)
  }

  # For Second option
  if(procedure == "bc_pem") {
    fuzzattr <- dplyr::mutate(
      fuzzattr,
      ne_aspect = dplyr::if_else(.data$new_asp > 180, 0, .data$ne_aspect),
      sw_aspect = dplyr::if_else(.data$new_asp < 180, 0, .data$sw_aspect),
      steep_sw = (.data$steep * .data$sw_aspect) / 100,
      steep_ne = (.data$steep * .data$ne_aspect) / 100,
      gentle_sw = (.data$slopelt20 * .data$sw_aspect) / 100,
      gentle_ne = (.data$slopelt20 * .data$ne_aspect) / 100)
  }

  fuzzattr
}

lsm_fuzc <- function(fuzzattr, crule) {
  fuzc_sum(fuzzattr, crule) %>%
    fuzc_max()
}

fuzc_sum <- function(fuzzattr, crule) {
  crule_order <- unique(crule$f_name)

  f <- fuzzattr %>%
    dplyr::select("seqno", "zone",
                  tidyselect::all_of(unique(crule$fuzattr))) %>%
    dplyr::arrange(.data$zone, .data$seqno) %>%
    dplyr::filter(!is.na(.data$zone))

  seqnos <- f$seqno

  f %>%
    tidyr::nest(data = c(-"zone")) %>%
    dplyr::left_join(crule, by = "zone") %>%
    dplyr::mutate(data = purrr::pmap(list(.data$data, .data$fuzattr, .data$relwt),
                                     ~..1[[..2]] * ..3)) %>%
    dplyr::group_by(.data$zone, .data$f_name) %>%
    dplyr::summarize(data = suppressMessages(list(dplyr::bind_cols(data)))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(data = purrr::map(data, ~round(rowSums(., na.rm = TRUE))),
                  f_name = factor(.data$f_name, levels = crule_order)) %>%
    dplyr::arrange(.data$f_name) %>%
    tidyr::pivot_wider(names_from = "f_name", values_from = "data") %>%
    tidyr::unnest(cols = c(-"zone", dplyr::everything())) %>%
    dplyr::mutate(seqno = seqnos)
}

fuzc_max <- function(f) {
  max_f <- dplyr::select(f, -"seqno", -"zone")

  n <- names(max_f)
  temp <- dplyr::tibble(max_facet = NA, max_value = 0,
                        max_2nd_facet = NA, max_2nd_value = 0,
                        .rows = nrow(max_f))

  for(i in seq_along(n)) {
    w <- which(max_f[[i]] >= temp$max_value)
    temp$max_facet[w] <- i
    temp$max_value[w] <- max_f[[i]][w]
  }

  for(i in seq_along(n)) {
    w <- which(max_f[[i]] >= temp$max_2nd_value & temp$max_facet != i)
    temp$max_2nd_facet[w] <- i
    temp$max_2nd_value[w] <- max_f[[i]][w]
  }

  dplyr::mutate(temp,
                max_facet = replace(.data$max_facet, .data$max_value == 0, NA_real_),
                max_2nd_facet = replace(.data$max_2nd_facet, .data$max_2nd_value == 0, NA_real_),
                max_facet_name = n[.data$max_facet],
                max_2nd_facet_name = n[.data$max_2nd_facet]) %>%
    dplyr::bind_cols(f, .)

}
