
calc_ddir2 <- function(db, verbose) {

  # Calculate elevation of 'shifted' directions (neighbours)
  db <- nb_values(db, max_cols = max(db$col), "elev")

  db1 <- finddir2(db) # Calc Flow direction

  # Check for flat plateaus
  # - get neighbouring directions

  db_flats <- db1 %>%
    dplyr::filter(flatcell == TRUE) %>%
    nb_values(db = db1, max_cols = max(db$col), col = c("ddir", "elev"), db_sub = .) %>%
    dplyr::group_by(seqno)

  end <- FALSE
  a <- 0
  while(!end) {
    if(verbose) message("     - Fixing flat plateaus round ", a <- a + 1, "...")

    # Calculate change in ddir
    db_flats <- db_flats %>%
      dplyr::mutate(elev_diff = elev[1] - elev_n) %>%
      dplyr::summarize(ddir = n[elev_diff >= 0 & !is.na(elev_diff) &
                                ddir_n != 5 & !is.na(ddir_n)][1]) %>%
      dplyr::filter(!is.na(ddir))

    if(nrow(db_flats) == 0) end <- TRUE

    if(!end) {
      # Change db1
      db1$ddir[db_flats$seqno] <- db_flats$ddir

      # Recalculate neighbours
      db_flats <- db1 %>%
        dplyr::filter(!is.na(ddir) & ddir == 5) %>%
        nb_values(db = db1, max_cols = max(db$col), col = c("ddir", "elev"), db_sub = .) %>%
        dplyr::group_by(seqno)
    }
  }

  # Deal with flow into depressions (flatin)
  # - Get the area of the depression, assign middle cell to a slightly lower elevation to break ties


  # Get patches of pit cells
  db_flats <- db1 %>%
    dplyr::filter(ddir == 5 & !is.na(ddir)) %>%
    nb_values(db = db1, max_cols = max(db$col), col = c("seqno", "ddir"), db_sub = .) %>%
    dplyr::filter(ddir_n == 5 & !is.na(ddir_n) & n != 5) %>%
    dplyr::mutate(patch = NA) %>%
    dplyr::arrange(seqno)

  if(nrow(db_flats) > 0) {
    p_n <- 1
    for(i in 1:nrow(db_flats)) {
      cell <- db_flats$seqno[i]
      if(is.na(db_flats$patch[i])) {
        db_flats$patch[db_flats$seqno == cell] <- p_n
        p_n <- p_n + 1
      }
      p <- db_flats$patch[i]
      cells_n <- unique(db_flats$seqno[db_flats$seqno_n %in% cell])
      p_old <- unique(db_flats$patch[db_flats$seqno %in% cells_n])
      p_old <- p_old[!is.na(p_old) & p_old != p]
      db_flats$patch[db_flats$seqno %in% cells_n] <- p
      if(length(p_old) > 0) db_flats$patch[db_flats$patch %in% p_old] <- p
    }

    # Get middle cell in a patch
    pit_centres <- db_flats %>%
      dplyr::select(-n, -ddir_n, -seqno_n) %>%
      dplyr::distinct() %>%
      dplyr::group_by(patch) %>%
      dplyr::mutate(centre = list(c(round(median(col)), round(median(row)))),
                    cell = purrr::map2(col, row, ~c(.x, .y)),
                    dist = purrr::map2_dbl(centre, cell, ~sqrt(sum((.y - .x)^2))),
                    dist_min = min(dist, na.rm = TRUE),
                    n_p = length(seqno)) %>%
      dplyr::summarize(seqno = seqno[dist == dist_min][1],
                       n_p = unique(n_p)) %>%
      dplyr::filter(n_p > 1) %>%
      dplyr::pull(seqno)

    # Recalculate flow directions for all flat cells in a patch towards the center
    # Iterate over all new flow flat cells until finished

    # Get directions to pit centers by pit patch
    db_flats <- db_flats %>%
      dplyr::group_by(seqno) %>%
      dplyr::mutate(ddir_opts = list(n)) %>%  # Get only neighbouring pit cells
      dplyr::select(-n, -ddir_n, -seqno_n) %>%
      dplyr::distinct(seqno, .keep_all = TRUE) %>%
      dplyr::mutate(centre = seqno %in% pit_centres) %>%
      dplyr::group_by(patch) %>%
      dplyr::mutate(row_f = row[centre][1], col_f = col[centre][1]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ddir_new = purrr::pmap_dbl(list(row = row, col = col,
                                                    row_f = row_f, col_f = col_f,
                                                    ddir_opts = ddir_opts), # make sure only directs to pit cell
                                               get_dir))

    db1$ddir[db_flats$seqno] <- db_flats$ddir_new
  }

  # Get flow direction (seqno of next cell)
  flow_values(db1, max_cols = max(db$col), col = "seqno") %>%
    dplyr::rename(drec = seqno_next)

  # Fix circular flow among flat cells
  # Shouldn't be necessary anymore, as specified lowest cell already?

  # Check for side-by-side pits, replace so one flows into the other
  # Shouldn't be necessary anymore, as specified lowest cell already?
}

calc_ddir <- function(db, verbose, n_clusters) {

  # Surround in impossible elevation
  db <- add_buffer(db) %>%
    dplyr::arrange(seqno)

  # For each cell cacluate the direction of flow (the buffer cells on an edge
  # will get NA)

  # Get neighbour list

  if(verbose) message("  1. Identifying adjacent cells")
  db <- dplyr::mutate(db,
                      adjacent = purrr::map(seqno, ~adj(.x, nrow = max(db$row),
                                                        ncol = max(db$col))))

  if(verbose) message("  2. Identifying adjacent elevations")

  # Divide and conquor to calculate neighbours
  if(nrow(db) < 1000) {
    db <- dplyr::mutate(db, n = purrr::map(adjacent, ~neighbours(a = .x, db = db)))
  } else {
    db <- divide(db, size = 20)
  }

  # Get flow direction
  if(verbose) message("  3. Calculating flow direction")

  db2 <- db %>%
    dplyr::mutate(ddir = purrr::map_dbl(n, finddir)) %>%
    dplyr::mutate(flatcell = (ddir == 5)) # Record flat cells

  # Deal with flat cells on plateaus: flatout
  if(verbose) message("  4. Fixing plateaus")
  db3 <- db2
  change <- TRUE
  a = 1
  while(change) {
    if(verbose) message("     - Fixing flat plateaus round ", a, "...")
    n5 <- sum(db3$ddir == 5, na.rm = TRUE)
    for(i in db3$seqno[db3$ddir == 5 & !is.na(db3$ddir)]){
      db3$ddir[db3$seqno == i] <- flatout(db3$n[[i]], db = db3)
    }
    a <- a + 1
    change <- sum(db3$ddir == 5, na.rm = TRUE) != n5
  }

  # Deal with flow into depressions (flatin)
  db4 <- db3
  db_ref <- dplyr::select(db4, seqno, ddir)
  change <- TRUE
  a <- 1
  if(verbose) message("  5. Fixing depressions")
  while(change) {
    if(verbose) message("     - Fixing flat depressions round ", a, "...")
    n5 <- sum(db4$ddir == 5, na.rm = TRUE)
    for(i in db4$seqno[db4$ddir == 5 & !is.na(db4$ddir)]){
      db4$ddir[db4$seqno == i] <- flatin(n = db4$n[[i]], db = db_ref)
    }
    a <- a+1
    change <- sum(db4$ddir == 5, na.rm = TRUE) != n5
  }

  # Get flow direction
  if(verbose) message("  6. Tracing flow directions")

  db5 <- db4 %>%
    dplyr::mutate(drec = purrr::map2_dbl(seqno, ddir, ~adj(.x,
                                                       nrow = max(db4$row),
                                                       ncol = max(db4$col), index = .y)))

  # Fix circular flow among flat cells
  if(verbose) message("  7. Fixing cicular flow among flat cells")

  # Which flat (originally)?
  cells <- dplyr::filter(db5, flatcell) %>% .$seqno

  for(c in cells){
    # For all cells that used to be flat, trace the path
    path <- trace_flow2(c, db5)
    last <- path[length(path)]

    # If the last obs in the path already exists, we have a circular path
    # Change the cell of the repeated obs to ddir = 5 and redo the drec to point to itself
    if(last %in% path[-length(path)]) {

      db5$ddir[db5$seqno == last] <- 5
      db5$drec[db5$seqno == last] <- last
    }
  }

  # Check for side-by-side pits, replace so one flows into the other
  if(verbose) message("  8. Check for adjacent pits, redirect flow")
  #flats <- dplyr::filter(db5, ddir == 5) %>% dplyr::pull(n)
  flats <- db5$n[db5$ddir == 5 & !is.na(db5$ddir)]
  db_ref <- dplyr::select(db5, seqno, ddir)
  for(n in flats) {
    pit <- neighbour_pit(n = n, db = db_ref)
    if(!is.null(pit)) {
      cell <- n$seqno[n$index == 5]
      db5$ddir[seqno == cell] <- pit$index
      db5$drec[seqno == cell] <- pit$seqno
      # db5 <- db5 %>%
      #   dplyr::mutate(ddir = replace(ddir, seqno == cell, pit$index),
      #                 drec = replace(drec, seqno == cell, pit$seqno))
    }
  }

  dplyr::arrange(db5, seqno)
}
