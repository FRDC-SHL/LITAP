
pit_stat1 <- function(db, w = NULL, verbose = FALSE) {

  if(length(unique(db$shedno[!is.na(db$shedno)])) > 1){ # If more than one watershed

    # Subset watersheds
    if(is.null(w)) w <- unique(db$shedno)

    if(verbose) message("    Assessing pour points ", appendLF = is.null(w))

    # For each watershed calculate the pour_point (details of the point at which tip to another watershed)

    # Take only ridge cells
    db_pits <- db %>%
      dplyr::filter(shedno %in% w, ridge == TRUE) %>%
      dplyr::select(seqno, shedno, elev, upslope)

    # # Calculate neighbouring sheds to split up files
    # shed_groups <- pp %>%
    #   dplyr::select(seqno, shedno) %>%
    #   nb_values(db, max_cols = max(db$col), col = "shedno", db_sub = .) %>%
    #   dplyr::select(-seqno, -n) %>%
    #   dplyr::filter(shedno != shedno_n) %>%
    #   dplyr::distinct() %>%
    #   dplyr::arrange(shedno, shedno_n)
    #
    # # Get ridge stats to further reduce selection
    # ridge_stats <- db %>%
    #   dplyr::filter(ridge == TRUE) %>%
    #   dplyr::group_by(shedno) %>%
    #   dplyr::summarize(max_elev = max(elev, na.rm = TRUE), min_elev = min(elev, na.rm = TRUE))
    #
    # shed_groups <- shed_groups %>%
    #   dplyr::left_join(ridge_stats, by = "shedno") %>%
    #   dplyr::left_join(ridge_stats, by = c("shedno_n" = "shedno"), suffix = c("", "_n")) %>%
    #   dplyr::group_by(shedno) %>%
    #   dplyr::summarize(max_elev = min(c(max_elev, max_elev_n), na.rm = TRUE),
    #                    min_elev = min(c(min_elev, min_elev_n[min_elev_n >= min_elev]), na.rm = TRUE))
#
#     pp <- pp %>%
#       dplyr::left_join(shed_groups, by = "shedno") %>%
#       dplyr::filter(elev >= min_elev, elev <= max_elev)


    db_pits <- db_pits %>%
      nb_values(db, max_cols = max(db$col), col = c("elev", "shedno", "seqno"), db_sub = .)

    pp <- dplyr::filter(db_pits, shedno != shedno_n)

    if(length(unique(pp$shedno)) != length(unique(db_pits$shedno[!is.na(db_pits$shedno)]))) {
      pp <- db_pits %>%
        dplyr::filter(!(shedno %in% pp$shedno), shedno != shedno_n) %>%
        dplyr::bind_rows(pp)
    }

    pp <- dplyr::mutate(pp, pour_elev = purrr::map2_dbl(elev, elev_n, max))
    pp <- dplyr::group_by(pp, seqno)
    pp <- dplyr::mutate(pp, min_elev_n = min(elev_n, na.rm = TRUE))
    pp <- dplyr::ungroup(pp)
    pp <- dplyr::filter(pp, elev_n == min_elev_n)

    pp <- dplyr::group_by(pp, shedno)
    pp <- dplyr::filter(pp, pour_elev == min(pour_elev, na.rm = TRUE))  # Must be lowest pour point
    #pp <- dplyr::filter(pp, upslope == max(upslope, na.rm = TRUE))     # Must be with maximum upslope contributions
    #pp <- dplyr::filter(pp, elev_n == min(elev_n, na.rm = TRUE))        # Must have lowest pour TO elevation
    pp <- dplyr::arrange(pp, shedno, seqno)                            # Sort by ascending seqno
    pp <- dplyr::slice(pp, 1)                                               # Take first one

    pp <- pp %>%
      dplyr::left_join(dplyr::select(db, seqno, col, row), by = c("seqno")) %>%
      dplyr::left_join(dplyr::select(db, seqno, col, row), by = c("seqno_n" = "seqno"), suffix = c("", "_out"))

    pp <- pp %>%
      dplyr::select(shedno, in_seqno = seqno, in_row = row, in_col = col, in_elev = elev,
                    out_seqno = seqno_n, out_row = row_out, out_col = col_out, out_elev = elev_n, drains_to = shedno_n,
                    pour_elev)

    # Add other calculations
    stats <- db %>%
      dplyr::filter(shedno %in% w) %>%
      dplyr::left_join(pp, by = "shedno") %>%
      dplyr::group_by(shedno) %>%
      dplyr::mutate(shed_area = length(shedno)) %>%
      dplyr::filter(elev <= pour_elev) %>%
      dplyr::summarize(shed_area = shed_area[1],
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
    stats <- dplyr::left_join(stats, dplyr::select(db, shedno, pond_shed) %>%
                                dplyr::distinct(), by = "shedno")
  }

  return(stats)
}

out_stat <- function(pit_stat) {
  pit_stat %>%
    dplyr::select(-dplyr::ends_with("_out")) %>%
    dplyr::left_join(dplyr::select(pit_stat, shedno, edge_pit, pit_elev, pit_seqno, pour_elev),
                     by = c("drains_to" = "shedno"), suffix = c("", "_out"))
}



calc_vol2fl <- function(db, i_stats, verbose) {

  # According to DEMProces.cpp (C++ flowmapr), Edge pits are skipped
  i_stats <- dplyr::filter(i_stats, edge_pit == FALSE)

  db <- dplyr::mutate(db, shedno = initial_shed)

  vol <- db[, lapply(db, class) != "list"]  %>% # remove lists
    dplyr::filter(!is.na(shedno))

  if(nrow(vol) > 0 & nrow(i_stats) > 0) {
    vol <- vol %>%
      dplyr::right_join(dplyr::select(i_stats, shedno, pour_elev, shed_area), #add stats
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
    dplyr::summarize(total_cells = length(elev)) %>%
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

  # db <- dplyr::left_join(db, vol_stats, by = "elev") %>%
  #   mutate_cond(is.na(parea), mm2fl = 0, vol2fl = 0, parea = 0)
}
