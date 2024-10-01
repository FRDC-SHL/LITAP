
pit_stat1 <- function(db, w = NULL, shed = "shedno", method, verbose) {

  db$shedno <- db[[shed]]

  if(length(unique(db$shedno[!is.na(db$shedno)])) > 1){ # If more than one watershed

    # Subset watersheds
    if(is.null(w)) w <- unique(db$shedno)

    if(verbose) message("    Assessing pour points ", appendLF = is.null(w))

    # For each watershed calculate the pour_point (details of the point at which tip to another watershed)

    # LITAP takes the lowest neighbour for the out point, whereas FlowMapR takes
    # the *last* neighbour that could be a pour point in this order (where F is
    # the focal cell (in point).
    #
    #  1  2  3
    #  4  F  6
    #  7  8  9

    # Take only ridge cells
    db_pits <- db %>%
      dplyr::filter(.data$shedno %in% !!w, .data$ridge == TRUE) %>%
      dplyr::select("seqno", "shedno", "elev", "upslope")

    db_pits <- db_pits %>%
      nb_values(db, max_cols = max(db$col), col = c("elev", "shedno", "seqno"), db_sub = .)

    pp <- dplyr::filter(db_pits, .data$shedno != .data$shedno_n)

    if(length(unique(pp$shedno)) != length(unique(db_pits$shedno[!is.na(db_pits$shedno)]))) {
      pp <- db_pits %>%
        dplyr::filter(!(.data$shedno %in% pp$shedno), .data$shedno != .data$shedno_n) %>%
        dplyr::bind_rows(pp)
    }

    pp <- dplyr::mutate(pp, pour_elev = purrr::map2_dbl(.data$elev, .data$elev_n, max))
    pp <- dplyr::group_by(pp, .data$seqno)
    pp <- dplyr::mutate(pp, min_elev_n = min(.data$elev_n, na.rm = TRUE))
    pp <- dplyr::ungroup(pp)

    if(method == "litap") {
      pp <- dplyr::filter(pp, .data$elev_n == .data$min_elev_n)
    }

    # Take first neighbour lower than the **cumulative min** pour_elev
    # (corresponds to FlowMapR last neighbour - see chekneigh1 and getneig1)

    if(method == "landmapr") {
      np <- c(7, 8, 9, 4, 5, 6, 1, 2, 3)
      pp <- dplyr::group_by(pp, .data$seqno)
      pp <- dplyr::mutate(pp, pour_elev_orig = pour_elev,
                          lm_n = .env$np[.data$n]) # Convert to landmapr order
      pp <- dplyr::arrange(pp, .data$lm_n)
      pp <- dplyr::mutate(pp, pour_elev = dplyr::lag(cummin(.data$pour_elev), default = Inf))
      pp <- dplyr::filter(pp, .data$elev_n < .data$pour_elev)
      pp <- dplyr::filter(pp, .data$lm_n == max(.data$lm_n))
      pp <- dplyr::mutate(pp, pour_elev = .data$pour_elev_orig)
    }

    pp <- dplyr::group_by(pp, .data$shedno)
    pp <- dplyr::filter(pp, .data$pour_elev == min(.data$pour_elev, na.rm = TRUE))  # Must be lowest pour point
    pp <- dplyr::arrange(pp, .data$shedno, .data$elev, .data$upslope, .data$seqno) # Sort by elev, upslope


    # Because LandMapR *only* adds to pit area if hits a new *low* elevation, not
    # a tied elevation. LITAP takes all equal to or below pour elev which can
    # include a few extra.
    if(method == "landmapr") {
      omits <- dplyr::summarize(
        pp,
        omit_cell = dplyr::n_distinct(.data$seqno) - 1,
        omit_elev = sum(.data$elev[-1], na.rm = TRUE))
    }

    pp <- dplyr::slice(pp, 1)                           # Take first one

    pp <- pp %>%
      dplyr::left_join(dplyr::select(db, "seqno", "col", "row"), by = c("seqno")) %>%
      dplyr::left_join(dplyr::select(db, "seqno", "col", "row"), by = c("seqno_n" = "seqno"),
                       suffix = c("", "_out"))

    pp <- pp %>%
      dplyr::select("shedno", "in_seqno" = "seqno", "in_row" = "row", "in_col" = "col",
                    "in_elev" = "elev",
                    "out_seqno" = "seqno_n", "out_row" = "row_out", "out_col" = "col_out",
                    "out_elev" = "elev_n",
                    "drains_to" = "shedno_n",
                    "pour_elev")

    # Add other calculations
    stats <- db %>%
      dplyr::filter(shedno %in% w) %>%
      dplyr::left_join(pp, by = "shedno") %>%
      dplyr::group_by(shedno) %>%
      dplyr::mutate(shed_area = length(shedno)) %>%
      dplyr::filter(elev <= pour_elev) %>%
      dplyr::summarize(
        shed_area = unique(shed_area),
        edge_pit = any(edge_map),
        pit_area = length(shedno),
        pit_vol = sum(pour_elev - elev),
        pit_elev = elev[ddir == 5],
        pit_seqno = seqno[ddir == 5],
        pit_row = row[ddir == 5],
        pit_col = col[ddir == 5],
        pre_vol = 0,
        varatio = dplyr::if_else(shed_area > 0,
                                 pit_vol / shed_area * 1000, 0)) %>%
      dplyr::right_join(pp, by = "shedno")

    if(method == "landmapr") {
      # Correction for number of cells included
      stats <- stats %>%
        dplyr::left_join(omits, by = "shedno") %>%
        dplyr::mutate(
          pit_area = .data$pit_area - .data$omit_cell,
          pit_vol = .data$pit_vol - (.data$pour_elev * .data$omit_cell - .data$omit_elev),
          varatio = dplyr::if_else(
            .data$shed_area > 0, .data$pit_vol / .data$shed_area * 1000, 0)) %>%
        dplyr::select(-"omit_cell", -"omit_elev")
    }


  } else {
    # If only one watershed
    stats <- db %>%
      dplyr::filter(!is.na(elev)) %>%
      dplyr::summarize(shedno = unique(shedno[!is.na(shedno)]),
                       pit_vol = NA,
                       pit_elev = elev[ddir == 5],
                       pit_seqno = seqno[ddir == 5],
                       pit_row = row[ddir == 5],
                       pit_col = col[ddir == 5],
                       shed_area = seq_along(pit_vol),
                       out_elev = NA,
                       out_seqno = NA,
                       drains_to = NA,
                       out_row = NA,
                       out_col = NA,
                       in_seqno = NA,
                       in_elev = NA,
                       in_row = NA,
                       in_col = NA,
                       pour_elev = NA,
                       edge_pit = TRUE,
                       pit_area = NA,
                       pre_vol = NA,
                       varatio = NA)
  }


  if("pond_shed" %in% names(db)) {
    stats <- dplyr::left_join(stats,
                              dplyr::select(db, "shedno", "pond_shed") %>%
                                dplyr::distinct(), by = "shedno")
  }

  stats
}

out_stat <- function(pit_stat) {
  p <- dplyr::select(pit_stat, "shedno", "edge_pit", "pit_elev",
                     "pit_seqno", "pour_elev")
  pit_stat %>%
    dplyr::select(-dplyr::ends_with("_out")) %>%
    dplyr::left_join(p, by = c("drains_to" = "shedno"), suffix = c("", "_out"))
}



calc_vol2fl <- function(db, i_stats, verbose) {

  # According to DEMProces.cpp (C++ flowmapr), Edge pits are skipped
  i_stats <- dplyr::filter(i_stats, edge_pit == FALSE)

  db <- dplyr::mutate(db, shedno = initial_shed)

  vol <- db[, lapply(db, class) != "list"]  %>% # remove lists
    dplyr::filter(!is.na(shedno))

  if(nrow(vol) > 0 & nrow(i_stats) > 0) {
    vol <- vol %>%
      # Add stats
      dplyr::right_join(dplyr::select(i_stats, shedno, pour_elev, shed_area),
                       by = "shedno") %>%
      tidyr::nest(data = c(-shedno)) %>%
      dplyr::mutate(vol = purrr::map(data, vol2fl, verbose = verbose)) %>%
      tidyr::unnest(vol)

    db <- dplyr::left_join(db, vol, by = c("shedno", "elev"))
    db[is.na(db$parea), c("mm2fl", "vol2fl", "parea")] <- 0
  } else {
    db <- dplyr::mutate(db, vol2fl = 0, mm2fl = 0, parea = 0)
  }

  db
}

# for each watershed look at slices of elevations, calculate the volumes and add together
vol2fl <- function(db, verbose) {

  if(any(db$shed_area <= 0)) {
    if(verbose) message("Shed area <= 0, is this reasonable?")
  }

  vol_stats <- db %>%
    dplyr::arrange(elev, dplyr::desc(upslope)) %>%
    dplyr::filter(elev <= pour_elev) %>%
    dplyr::group_by(elev, shed_area) %>%
    dplyr::summarize(total_cells = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(parea = cumsum(total_cells),
                  last_elev = dplyr::lag(elev, default = 0),
                  last_elev = replace(last_elev, last_elev == 0, elev[last_elev == 0]),
                  elev_diff = (elev - last_elev) * 1000,
                  vol2fl = NA)
  #curr_vol = (elev_diff * (total_cells - 1)),
  #vol2fl = 0.1 + cumsum(curr_vol))

  if(nrow(vol_stats) > 0) {
    for(i in 1:nrow(vol_stats)) {
      prev <- vol_stats$vol2fl[i-1]
      if(length(prev) == 0) prev <- 0.1
      vol_stats$vol2fl[i] <-  prev + (vol_stats$elev_diff[i] * (vol_stats$parea[i]-1))
    }
  }

  vol_stats %>%
    dplyr::mutate(mm2fl = dplyr::if_else(shed_area > 0,
                                         vol2fl/shed_area,
                                         vol2fl/1)) %>%
    dplyr::select(elev, vol2fl, mm2fl, parea)
}
