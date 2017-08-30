#' @export
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
