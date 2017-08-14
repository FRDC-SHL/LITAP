#' @export
calc_ddir <- function(db, parallel = TRUE, verbose = FALSE, n_clusters = 7) {

  # Surround in impossible elevation
  db <- add_buffer(db) %>%
    dplyr::arrange(seqno)

  #if(verbose) step0 <- flow_plot(db)

  # For each cell cacluate the direction of flow (the buffer cells on an edge
  # will get NA)

  # Get neighbour list
  if(verbose) message("Calculating neighbour list")

  if(parallel) {
    cluster <- multidplyr::create_cluster(n_clusters) %>%
      multidplyr::cluster_library("magrittr") %>%
      multidplyr::cluster_assign_value("adj", adj) %>%
      multidplyr::cluster_assign_value("neighbours", neighbours) %>%
      multidplyr::cluster_assign_value("db", db) %>%
      multidplyr::cluster_assign_value("finddir", finddir)

    suppressWarnings({
      db <- db %>%
        dplyr::mutate(parallel = rep(1:7, length.out = nrow(db))) %>%
        multidplyr::partition(parallel, cluster = cluster)
    })
  }
  db <- dplyr::mutate(db,
                      adjacent = purrr::map(seqno, ~adj(.x, nrow = max(db$row), ncol = max(db$col))),
                      n = purrr::map(adjacent, ~neighbours(a = .x, db = db[, c("seqno", "elev")])))

  # Get flow direction
  if(verbose) message("Calculating flow direction")

  db2 <- db %>%
    dplyr::mutate(ldir = purrr::map_dbl(n, finddir)) %>%
    dplyr::mutate(flatcell = (ldir == 5)) # Record flat cells


  if(parallel) {
    db2 <- dplyr::collect(db2) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(seqno)
  }

  # Deal with flat cells on plateaus: flatout
  if(verbose) message("Fixing plateaus")
  db3 <- db2
  change <- TRUE
  a = 1
  while(change) {
    if(verbose) message("  Fixing flat plateaus round ", a, "...")
    n5 <- sum(db3$ldir == 5, na.rm = TRUE)
    for(i in db3$seqno[db3$ldir == 5 & !is.na(db3$ldir)]){
      db3$ldir[db3$seqno == i] <- flatout(db3$n[[i]], db = db3)
    }
    a <- a + 1
    change <- sum(db3$ldir == 5, na.rm = TRUE) != n5
  }

  # Deal with flow into depressions (flatin)
  db4 <- db3
  change <- TRUE
  a <- 1
  if(verbose) message("Fixing depressions")
  while(change) {
    if(verbose) message("  Fixing flat depressions round ", a, "...")
    n5 <- sum(db4$ldir == 5, na.rm = TRUE)
    for(i in db4$seqno[db4$ldir == 5 & !is.na(db4$ldir)]){
      db4$ldir[db4$seqno == i] <- flatin(n = db4$n[[i]], db = db4)
    }
    a <- a+1
    change <- sum(db4$ldir == 5, na.rm = TRUE) != n5
  }

  # Get flow direction
  if(verbose) message("Tracing flow directions")

  if(parallel) {
    cluster <- multidplyr::create_cluster(n_clusters) %>%
      multidplyr::cluster_library("magrittr") %>%
      multidplyr::cluster_assign_value("adj", adj) %>%
      multidplyr::cluster_assign_value("db4", db4)

    suppressWarnings({
      db4 <- db4 %>%
        multidplyr::partition(parallel, cluster = cluster)
    })
  }

  db5 <- db4 %>%
    dplyr::mutate(drec = purrr::map2(seqno, ldir, ~adj(.x, nrow = max(db4$row),
                                                       ncol = max(db4$col), index = .y)))

  if(parallel) {
    db5 <- dplyr::collect(db5) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(seqno)
  }

  db5 <-  db5 %>%
    tidyr::unnest(drec)

  # Fix circular flow among flat cells
  if(verbose) message("Fix cicular flow among flat cells")

  # Which flat (originally)?
  cells <- dplyr::filter(db5, flatcell) %>% .$seqno

  for(c in cells){
    # For all cells that used to be flat, trace the path
    path <- trace_flow(c, db5)
    last <- path[length(path)]

    # If the last obs in the path already exists, we have a circular path
    # Change the cell of the repeated obs to ldir = 5 and redo the drec to point to itself
    if(last %in% path[-length(path)]) {
      db5 <- db5 %>%
        dplyr::mutate(ldir = replace(ldir, seqno == last, 5),
                      drec = replace(drec, seqno == last, last))
    }
  }

  #if(verbose) step4_dep <- flow_plot(db5[db5$row < 15 & db5$row > 5 & db5$col > 30 & db5$col < 45,], dir = TRUE)

  # Check for side-by-side pits, replace so one flows into the other
  if(verbose) message("Check for adjacent pits, redirect flow")
  flats <- dplyr::filter(db5, ldir == 5) %>% dplyr::pull(n)
  for(n in flats) {
    pit <- neighbour_pit(n = n, db = db5)
    if(!is.null(pit)) {
      cell <- n$seqno[n$index == 5]
      db5 <- db5 %>%
        dplyr::mutate(ldir = replace(ldir, seqno == cell, pit$index),
                      drec = replace(drec, seqno == cell, pit$seqno))
    }
  }

  # if(verbose) step5_dep <- flow_plot(db5[db5$row < 15 & db5$row > 5 & db5$col > 30 & db5$col < 45,], dir = TRUE)

  # if(verbose) gridExtra::grid.arrange(step1_dep, step2_dep,
  #                                     step3_dep, step4_dep, step5_dep, nrow = 1)
  db5 <- dplyr::arrange(db5, seqno)

  if(parallel) db5 <- dplyr::select(db5, -parallel)

  return(db5)
}
