
calc_ddir2 <- function(db, verbose = FALSE) {


  # Calculate 8 columns reflecting the elevation of 'shifted' directions (neighbours)
  db <- nb_values(db, max_cols = max(db$col), "elev")

  db1 <- finddir2(db) # Calc Flow direction

  # Check for flat plateaus
  # - get neighbouring directions

  db_flats <- db1 %>%
    dplyr::filter(flatcell == TRUE) %>%
    nb_values(db = db1, max_cols = max(db$col), col = c("ldir", "elev"), db_sub = .) %>%
    dplyr::group_by(seqno)

  end <- FALSE
  a <- 0
  while(!end) {
    if(verbose) message("     - Fixing flat plateaus round ", a <- a + 1, "...")

    # Caclualate change in ldir
    db_flats <- db_flats %>%
      dplyr::mutate(elev_diff = elev[1] - elev_n) %>%
      dplyr::summarize(ldir = n[elev_diff >= 0 & !is.na(elev_diff) &
                                ldir_n != 5 & !is.na(ldir_n)][1]) %>%
      dplyr::filter(!is.na(ldir))

    if(nrow(db_flats) == 0) end <- TRUE

    if(!end) {
      # Change db1
      db1$ldir[db_flats$seqno] <- db_flats$ldir

      # Recalculate neighbours
      db_flats <- db1 %>%
        dplyr::filter(!is.na(ldir) & ldir == 5) %>%
        nb_values(db = db1, max_cols = max(db$col), col = c("ldir", "elev"), db_sub = .) %>%
        dplyr::group_by(seqno)
    }
  }

  # Deal with flow into depressions (flatin)
  # - Get the area of the depression, assign middle cell to a slightly lower elevation to break ties


  # Get patches of pit cells
  db_flats <- db1 %>%
    dplyr::filter(ldir == 5 & !is.na(ldir)) %>%
    nb_values(db = db1, max_cols = max(db$col), col = c("seqno", "ldir"), db_sub = .) %>%
    dplyr::filter(ldir_n == 5 & !is.na(ldir_n) & n != 5) %>%
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
      db_flats$patch[db_flats$seqno %in% cells_n] <- p
    }

    # Get middle cell in a patch
    pit_centres <- db_flats %>%
      dplyr::select(-n, -ldir_n, -seqno_n) %>%
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
      dplyr::mutate(ldir_opts = list(n)) %>%  # Get only neighbouring pit cells
      dplyr::select(-n, -ldir_n, -seqno_n) %>%
      dplyr::distinct(seqno, .keep_all = TRUE) %>%
      dplyr::mutate(centre = seqno %in% pit_centres) %>%
      dplyr::group_by(patch) %>%
      dplyr::mutate(row_f = row[centre][1], col_f = col[centre][1]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ldir_new = purrr::pmap_dbl(list(row = row, col = col,
                                                    row_f = row_f, col_f = col_f,
                                                    ldir_opts = ldir_opts), # make sure only directs to pit cell
                                           get_dir))

    db1$ldir[db_flats$seqno] <- db_flats$ldir_new
  }

  # Get flow direction (seqno of next cell)
  db1 <- flow_values(db1, max_cols = max(db$col), col = "seqno") %>%
    dplyr::rename(drec = seqno_next)

  # Fix circular flow among flat cells
  # Shouldn't be necessary anymore, as specified lowest cell already?

  # Check for side-by-side pits, replace so one flows into the other
  # Shouldn't be necessary anymore, as specified lowest cell already?

  return(db1)
}

calc_ddir <- function(db, verbose = FALSE, n_clusters = 7) {

  # Surround in impossible elevation
  db <- add_buffer(db) %>%
    dplyr::arrange(seqno)

  # For each cell cacluate the direction of flow (the buffer cells on an edge
  # will get NA)

  # Get neighbour list

  if(verbose) message("  1. Identifying adjacent cells")
  db <- dplyr::mutate(db,
                      adjacent = purrr::map(seqno, ~adj(.x, nrow = max(db$row), ncol = max(db$col))))

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
    dplyr::mutate(ldir = purrr::map_dbl(n, finddir)) %>%
    dplyr::mutate(flatcell = (ldir == 5)) # Record flat cells

  # Deal with flat cells on plateaus: flatout
  if(verbose) message("  4. Fixing plateaus")
  db3 <- db2
  change <- TRUE
  a = 1
  while(change) {
    if(verbose) message("     - Fixing flat plateaus round ", a, "...")
    n5 <- sum(db3$ldir == 5, na.rm = TRUE)
    for(i in db3$seqno[db3$ldir == 5 & !is.na(db3$ldir)]){
      db3$ldir[db3$seqno == i] <- flatout(db3$n[[i]], db = db3)
    }
    a <- a + 1
    change <- sum(db3$ldir == 5, na.rm = TRUE) != n5
  }

  # Deal with flow into depressions (flatin)
  db4 <- db3
  db_ref <- dplyr::select(db4, seqno, ldir)
  change <- TRUE
  a <- 1
  if(verbose) message("  5. Fixing depressions")
  while(change) {
    if(verbose) message("     - Fixing flat depressions round ", a, "...")
    n5 <- sum(db4$ldir == 5, na.rm = TRUE)
    for(i in db4$seqno[db4$ldir == 5 & !is.na(db4$ldir)]){
      db4$ldir[db4$seqno == i] <- flatin(n = db4$n[[i]], db = db_ref)
    }
    a <- a+1
    change <- sum(db4$ldir == 5, na.rm = TRUE) != n5
  }

  # Get flow direction
  if(verbose) message("  6. Tracing flow directions")

  db5 <- db4 %>%
    dplyr::mutate(drec = purrr::map2_dbl(seqno, ldir, ~adj(.x,
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
    # Change the cell of the repeated obs to ldir = 5 and redo the drec to point to itself
    if(last %in% path[-length(path)]) {

      db5$ldir[db5$seqno == last] <- 5
      db5$drec[db5$seqno == last] <- last
    }
  }

  # Check for side-by-side pits, replace so one flows into the other
  if(verbose) message("  8. Check for adjacent pits, redirect flow")
  #flats <- dplyr::filter(db5, ldir == 5) %>% dplyr::pull(n)
  flats <- db5$n[db5$ldir == 5 & !is.na(db5$ldir)]
  db_ref <- dplyr::select(db5, seqno, ldir)
  for(n in flats) {
    pit <- neighbour_pit(n = n, db = db_ref)
    if(!is.null(pit)) {
      cell <- n$seqno[n$index == 5]
      db5$ldir[seqno == cell] <- pit$index
      db5$drec[seqno == cell] <- pit$seqno
      # db5 <- db5 %>%
      #   dplyr::mutate(ldir = replace(ldir, seqno == cell, pit$index),
      #                 drec = replace(drec, seqno == cell, pit$seqno))
    }
  }

  db5 <- dplyr::arrange(db5, seqno)

  return(db5)
}
