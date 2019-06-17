get_pour_point2 <- function(db_w, w, db, verbose = FALSE){

  if(verbose)  message("      - Watershed ", w)

  pour_elev <- 1000000000
  overspill <- FALSE
  i <- 1
  pits <- NULL
  pp <- NULL

  db_w <- db_w %>%
    dplyr::mutate(shed_edge = purrr::map_lgl(adjacent, ~shed_edge(a = .x, db = db, w = w))) %>%
    mutate_cond(is.na(shed_edge), shed_edge = FALSE)

  while(!overspill & i <= nrow(db_w)) {

    a <- nrow(db_w[db_w$elev == db_w$elev[i],])
    layer <- db_w[db_w$elev == db_w$elev[i] & db_w$shed_edge,]

    if(nrow(layer) > 0) {

      n <- layer %>%
        dplyr::mutate(n = purrr::map(adjacent, ~neighbours(.x, db))) %>%
        tidyr::unnest(n, .sep = "_") %>%
        dplyr::filter(n_shedno != w)

      if(nrow(n) > 0){
        min_elev <- min(n$n_elev, na.rm = TRUE)

        n2 <- n %>%
          dplyr::filter(n_elev == min_elev,
                        n_elev < pour_elev,
                        elev <= pour_elev)

        if(nrow(n2) > 0){

          if(nrow(n2) > 1) n2 <- dplyr::filter(n2, seqno == min(seqno))

          if(nrow(n2) > 1) {
            if(length(unique(n2$n_shedno)) == 1) {
              n2 <- dplyr::filter(n2, n_seqno == min(n_seqno))
            } else {
              n2 <- dplyr::filter(n2, n_seqno == min(n_seqno))
              #browser()
              #stop("Identical cells which lead to different watersheds, how to break the tie?")
            }
          }

          cell <- which(n2$seqno == db_w$seqno)
          pour_elev <- max(n2$n_elev, n2$elev)
          pp <- tibble::tibble(out_elev = n2$n_elev,
                               out_seqno = n2$n_seqno,
                               drains_to = n2$n_shedno,
                               out_row = n2$n_row,
                               out_col = n2$n_col,
                               in_seqno = n2$seqno,
                               in_elev = n2$elev,
                               in_row = n2$row,
                               in_col = n2$col,
                               pour_elev = pour_elev,
                               edge_pit = any(db_w$edge_cell[1:cell]),
                               pit_area = cell)
        }

        # min_elev only applies to current focal cell
        # NOTE: VERY tempted to make this: min_elev >= pour_elev to allow overflow where otherwise wouldn't be
        if(min_elev > pour_elev & any(layer$elev > pour_elev)) {
          overspill <- TRUE
          pits <- db_w[1:pp$pit_area,] %>%
            dplyr::mutate(pour_elev = pour_elev)
        }
      }
    }
    i <- i + a
  }

  if(is.null(pits)) {
    pits <- db_w %>%
      dplyr::mutate(pour_elev = pour_elev)
  }

  if(is.null(pp)) {
    stop("pit_stat returned no pour point for watershed ", w)
    #browser()
    ## CHECK THIS!!!
    pp <- tibble::tibble(out_elev = db_w$elev[1],
                         out_seqno = db_w$seqno[1],
                         drains_to = w,
                         out_row = db_w$row[1],
                         out_col = db_w$col[1],
                         in_seqno = db_w$seqno[1],
                         in_elev = db_w$elev[1],
                         in_row = db_w$row[1],
                         in_col = db_w$col[1],
                         pour_elev = pour_elev,
                         edge_pit = FALSE,
                         pit_area = 0)
  }

  return(tibble::tibble(pits = list(pits), pour_point = list(pp)))
}

get_pour_point <- function(db_w, w, db, verbose = FALSE){

  if(verbose)  message("  - Watershed number ", w)

  pour_elev <- 1000000000
  overspill <- FALSE
  i <- 1
  pits <- NULL

  db_w <- db_w %>%
    dplyr::mutate(shed_edge = purrr::map_lgl(adjacent, ~shed_edge(a = .x, db = db, w = w))) %>%
    mutate_cond(is.na(shed_edge), shed_edge = FALSE)

  while(!overspill & i <= nrow(db_w)) {
    if(db_w$shed_edge[i]) {
      focal <- db_w[i, ]

      n <- neighbours(db_w$adjacent[[i]], db) %>%
        dplyr::filter(shedno != w)

      if(nrow(n) > 0){
        min_elev <- min(n$elev, na.rm = TRUE)

        for(a in 1:nrow(n)) {

          if(n$elev[a] < pour_elev && focal$elev <= pour_elev) {
            #message("Cell = ", i)
            pour_elev <- max(n$elev[a], focal$elev)
            pp <- tibble::tibble(out_elev = n$elev[a],
                                 out_seqno = n$seqno[a],
                                 drains_to = n$shedno[a],
                                 out_row = n$row[a],
                                 out_col = n$col[a],
                                 in_seqno = focal$seqno,
                                 in_elev = focal$elev,
                                 in_row = focal$row,
                                 in_col = focal$col,
                                 pour_elev = pour_elev,
                                 edge_pit = any(db_w$edge_cell[1:i]),
                                 pit_area = i)
          }
        }

        # min_elev only applies to current focal cell
        if(min_elev > pour_elev & focal$elev > pour_elev) {
          overspill <- TRUE
          pits <- db_w[1:pp$pit_area,] %>%
            dplyr::mutate(pour_elev = pour_elev)
          pour_point <- pp
        }
      }
    }
    i <- i + 1
  }

  if(is.null(pits)) {
    pits <- db_w %>%
      dplyr::mutate(pour_elev = pour_elev)
  }

  return(tibble::tibble(pits = list(pits), pour_point = list(pour_point)))
}


pit_stat <- function(db, w = NULL, verbose = FALSE) {
  db <- db %>%
    dplyr::mutate(edge_cell = purrr::map_lgl(adjacent, ~edge_pit(a = .x, db = db)))

  if(length(unique(db$shedno[!is.na(db$shedno)])) > 1){
    db_pit <- db %>%
      dplyr::filter(buffer == FALSE, !is.na(shedno)) %>%
      dplyr::group_by(shedno) %>%
      dplyr::mutate(shed_area = length(shedno)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(shedno, elev, upslope_n)

    # Subset watersheds
    if(!is.null(w)) db_pit <- db_pit %>% dplyr::filter(shedno %in% w)

    # For each watershed IN ORDER, calculate the pour_point (details of the point at which tip to another watershed)

    pour_point <- tibble::tibble()
    pits <- tibble::tibble()

    if(verbose) message("    Assessing pour points: ", appendLF = is.null(w))

    stats <- db_pit %>%
      tidyr::nest(- shedno, .key = "db_w")

    stats <- stats %>%
      dplyr::group_by(shedno) %>%
      dplyr::mutate(pp = purrr::map2(db_w, shedno, ~ get_pour_point2(.x, .y, db = db, verbose = verbose)))

    stats <- stats %>%
      dplyr::ungroup() %>%
      dplyr::select(-db_w) %>%
      tidyr::unnest(pp) %>%
      dplyr::mutate(pits = purrr::map(pits,
                                      ~ dplyr::summarize(.x,
                                                         pit_vol = sum(pour_elev - elev),
                                                         pit_elev = elev[ldir == 5],
                                                         pit_seqno = seqno[ldir == 5],
                                                         pit_row = row[ldir == 5],
                                                         pit_col = col[ldir == 5],
                                                         shed_area = unique(shed_area)))) %>%
      tidyr::unnest(pits) %>%
      tidyr::unnest(pour_point) %>%
      dplyr::mutate(pre_vol = 0,
                    varatio = dplyr::if_else(shed_area > 0,
                                             pit_vol / shed_area * 1000, 0))

  } else {
    # If only one watershed
    stats <- db %>%
      dplyr::filter(!is.na(elev)) %>%
      dplyr::summarize(shedno = unique(shedno[!is.na(shedno)]),
                       pit_vol = NA,
                       pit_elev = elev[ldir == 5],
                       pit_seqno = seqno[ldir == 5],
                       pit_row = row[ldir == 5],
                       pit_col = col[ldir == 5],
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



calc_vol2fl <- function(db, i_stats) {

  # Where no change in ShedNo from local (or initial?) to ShedNow (pond shed) then all 0's ???
  db <- dplyr::mutate(db, shedno = initial_shed)
  vol <- db[, lapply(db, class) != "list"] %>% # remove lists
    dplyr::filter(!is.na(shedno), local_shed != pond_shed)

  if(nrow(vol) > 0) {
    vol <- vol %>%
      dplyr::left_join(dplyr::select(i_stats, shedno, pour_elev, shed_area), #add stats
                       by = "shedno") %>%
      tidyr::nest(-shedno) %>%
      dplyr::mutate(vol = purrr::map(data, vol2fl)) %>%
      tidyr::unnest(vol)
    db <- dplyr::left_join(db, vol, by = c("shedno", "elev")) %>%
      mutate_cond(is.na(parea), mm2fl = 0, vol2fl = 0, parea = 0)
  } else {
    db <- dplyr::mutate(db, vol2fl = 0, mm2fl = 0, parea = 0)
  }

  return(db)
}

# for each watershed look at slices of elevations, calculate the volumes and add together
vol2fl <- function(db) {

  if(any(db$shed_area <= 0)) {
    if(verbose) message("Shed area <= 0, is this reasonable?")
  }

  vol_stats <- db %>%
    dplyr::arrange(elev, dplyr::desc(upslope_n)) %>%
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

  for(i in 1:nrow(vol_stats)) {
    prev <- vol_stats$vol2fl[i-1]
    if(length(prev) == 0) prev <- 0.1
    vol_stats$vol2fl[i] <-  prev + (vol_stats$elev_diff[i] * (vol_stats$parea[i] - 1))
  }

  vol_stats <- vol_stats %>%
    dplyr::mutate(mm2fl = dplyr::if_else(shed_area > 0,
                                         vol2fl/shed_area,
                                         vol2fl/1)) %>%
    dplyr::select(elev, vol2fl, mm2fl, parea)

  # db <- dplyr::left_join(db, vol_stats, by = "elev") %>%
  #   mutate_cond(is.na(parea), mm2fl = 0, vol2fl = 0, parea = 0)

  return(vol_stats)
}
