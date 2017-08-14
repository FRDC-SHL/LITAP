first_pitr <- function(db, max_area = 10, max_depth = 0.5, parallel = FALSE) {

  # Working with initial_shed
  db$shedno <- db$initial_shed

  w_stats <- pit_stat(db, parallel = parallel) %>%
    dplyr::arrange(dplyr::desc(pit_elev), varatio)  ## shedord

  # In sequence, for each watershed, see if should be combined with another
  # Combine if too small (max_area) or too shallow (max_depth)
  w_all <- unique(w_stats$shedno)
  for(w in w_all){
    w_rm <- dplyr::filter(w_stats, shedno == w) %>%
      dplyr::mutate(remove = pit_area <= max_area | pour_elev - pit_elev <= max_depth)

    if(w_rm$remove){

      db <- remove_pit(w_rm, w_stats, db)

      # Update shed statistics
      w_stats <- pit_stat(db, parallel = parallel) %>%
        dplyr::arrange(dplyr::desc(pit_elev), varatio)  ## shedord

    } else message("Skipping watershed number ", w_rm$shedno)
  }

  # Save as local_shed numbers
  db <- dplyr::mutate(db, local_shed = shedno)

  return(db)
}

#' @import magrittr
second_pitr <- function(db, parallel = FALSE) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat(db, parallel = parallel) %>%
    dplyr::arrange(pit_elev, varatio) %>% ## shedord2  (PITELEV*10^10 + varatio) TAG SHEDORD2
    dplyr::mutate(removed = FALSE, final = FALSE, nextpit = 0, becomes = 0)


  #w_stats %>% select(shedno, edge_pit, pour_elev, pit_elev, out_shed, out_elev)


  # Starting conditions
  pond <- w_stats
  removed <- vector()
  final_pits <- vector()
  end <- FALSE
  w <- w_stats$shedno[1]

  while(!end){

    #w_rm <- find_lowest(w, w_stats, final_pits = pond$shedno[pond$final])
    w_rm <- find_lowest(w, w_stats, final_pits = final_pits)

    w_focal <- dplyr::filter(w_stats, shedno == w_rm$shedno)
    w_drain <- dplyr::filter(w_stats, shedno == w_rm$out_shed)

    #if(w_rm$shedno == 36) browser()

    if(!(w_focal$edge_pit & w_drain$edge_pit)) {

      new_shed <- max(db$shedno, na.rm = TRUE) + 1

      removed <- c(removed, w_rm$shedno, w_rm$out_shed)

      # Remove shed
      db <- remove_pit(w_rm, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_rm$shedno, w_rm$out_shed),
                                       new_shed))

      # Update shed statistics
      w_stats <- pit_stat(db, parallel = parallel) %>%
        dplyr::arrange(pit_elev, varatio) %>%
        dplyr::mutate(removed = FALSE, final = FALSE, nextpit = 0, becomes = 0)## shedord

      # Keep track of pond statistics (ie. keep track of all watersheds)
      # pond <- dplyr::bind_rows(w_focal, w_drain) %>%
      #   dplyr::mutate(removed = TRUE,
      #                 final = w_rm$at_final,
      #                 nextpit = out_shed,
      #                 becomes = new_shed) %>%
      #   dplyr::bind_rows(pond, .)
      pond <- dplyr::bind_rows(pond, w_stats[w_stats$shedno == new_shed,]) %>%
        mutate_cond(shedno %in% c(w_focal$shedno, w_drain$shedno),
                    removed = TRUE,
                    final = w_rm$at_final,
                    nextpit = out_shed,
                    becomes = new_shed)

      pond$nextpit[pond$removed == FALSE & pond$final == FALSE & pond$nextpit == w_focal$shedno |
                     pond$nextpit == w_drain$shedno] <- new_shed

    } else {
      message("Watersheds ", w_focal$shedno, " and ", w_drain$shedno, " are FINAL sheds")
      final_pits <- unique(c(final_pits, w_rm$shedno, w_rm$out_shed))

      pond <- pond %>%
        mutate_cond(shedno %in% w_focal$shedno, nextpit = w_drain$shedno) %>%
        mutate_cond(shedno %in% w_drain$shedno, nextpit = w_focal$shedno)
      # p <- dplyr::bind_rows(w_focal, w_drain) %>%
      #   dplyr::mutate(removed = FALSE,
      #                 final = TRUE,
      #                 nextpit = out_shed,
      #                 becomes = 0)#w_stats[w_stats$shedno %in% c(w_rm$shedno, w_rm$out_shed), ]
    }

    # Get next shed
    done <- unique(c(final_pits, removed))

    w <- dplyr::filter(w_stats, !(shedno %in% done))
    if(nrow(w) > 0) {
      w <-  w$shedno[1]
    } else end <- TRUE

  }

  # Save as global_shed numbers
  db <- dplyr::mutate(db, global_shed = shedno)

  return(list("db" = db, "stats" = pond))
}

third_pitr <- function(db, parallel = FALSE) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat(db, parallel = parallel) %>%
    dplyr::arrange(varatio)

  # Starting conditions
  fill <- tibble::tibble()
  end <- FALSE
  done <- vector()
  w <- w_stats$shedno[1]

  while(!end){

    w_focal <- dplyr::filter(w_stats, shedno == w)
    w_drain <- dplyr::filter(w_stats, shedno == w_focal$out_shed)

    message("Assessing ", w)

    if(w_focal$global_shed == w_drain$global_shed) {
      new_shed <- max(db$shedno, na.rm = TRUE) + 1

      db <- remove_pit(w_focal, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_focal$shedno, w_drain$shedno),
                                       new_shed))

      # Update shed statistics
      w_stats <- pit_stat(db, parallel = parallel) %>%
        dplyr::arrange(varatio)

      fill <- dplyr::bind_rows(dplyr::bind_cols(w_focal, becomes = new_shed),
                            dplyr::bind_cols(w_drain, becomes = new_shed)) %>%
        dplyr::mutate(final = FALSE) %>%
        dplyr::arrange(shedno) %>%
        dplyr::bind_rows(fill, .)

      done <- c(done, w_focal$shedno, w_drain$shedno)

    } else {
      done <- c(done, w_focal$shedno)
    }

    # Get next shed
    w <- dplyr::filter(w_stats, !(shedno %in% done))
    if(nrow(w) > 0) {
      w <-  w$shedno[1]
    } else end <- TRUE

  }

  add <- w_stats %>%
    dplyr::filter(!(shedno %in% fill$shedno)) %>%
    dplyr::mutate(becomes = shedno, final = TRUE, removed = FALSE) %>%
    dplyr::arrange(shedno)

  fill <- dplyr::bind_rows(fill, add)



  db <- dplyr::mutate(db, fill_shed = shedno)

  return(list("db" = db, "stats" = fill))
}



remove_pit <- function(w_rm, w_stats, db) {

  db <- dplyr::arrange(db, seqno)

  w_rm <- w_rm %>%
    dplyr::mutate(direction = ifelse(pit_elev >= pit_elev_out, "out", "in"))

  if(w_rm$direction == "out"){
    w_rm <- dplyr::select(w_rm,
                          pit_seqno,
                          in_seqno, out_seqno,
                          pour_elev,
                          shedno, out_shed)
  } else {
    w_rm <- dplyr::select(w_rm,
                          pit_seqno = pit_seqno_out,
                          in_seqno = out_seqno, out_seqno = in_seqno,
                          pour_elev = pour_elev_out,
                          shedno = out_shed, out_shed = shedno)
  }

  message("Removing watershed number ", w_rm$shedno, ": ")

  #if(w_rm$shedno == 2) browser()

  ## Remove Pit

  # Reverse directions of these cells (both regular and pit cell)

  # - Second reflows the pit cell. NOTE, if pit cell is the pour point, then
  #   this may be NA (fixed in next step)
  # - Third reflow the last cell in the new flow path (in_seqno) to point
  #   towards the first cell in the next flow path (out_seqno). This fixes any
  #   NA's from the previous step

  # 1. Get original flow down to old pit
  new_flow <- trace_flow(cell = w_rm$in_seqno, db = db)

  new_ldir <- db %>%
    dplyr::filter(seqno %in% new_flow) %>%
    # First get correct flow order
    dplyr::mutate(seqno = factor(seqno, levels = new_flow)) %>%
    dplyr::arrange(seqno) %>%
    # Second, reverse directions
    dplyr::mutate(ldir = 10 - dplyr::lag(ldir),
                  seqno = as.numeric(as.character(seqno))) %>%
    # Third, replace flow direction of pour point to flow into new watershed
    mutate_cond(seqno == w_rm$in_seqno,
                     ldir = purrr::map_dbl(n, ~ .x$index[.x$seqno == w_rm$out_seqno]),
                     drec = w_rm$out_seqno) %>%
    # Fourth, reorder to match db
    dplyr::arrange(seqno)


  db <- db %>%
    # Apply new flow directions
    dplyr::mutate(ldir = replace(ldir, seqno %in% new_flow, new_ldir$ldir)) %>%
    # Caluclate new downstream flow directions
    mutate_cond(seqno %in% new_flow,
                drec = purrr::map2_dbl(seqno, ldir, ~adj(.x,
                                                         nrow = max(db$row),
                                                         ncol = max(db$col),
                                                         index = .y)))

  # Get new flow path from old pit centre to new pit centre
  new_flow <- trace_flow(cell = w_rm$pit_seqno, db = db)

  # Update upslope starting with old pit and looping through to new,
  #    adding upslope from previous to new cell along the way
  for(i in 1:length(new_flow)) {
    db <- db %>%
      mutate_cond(seqno == new_flow[i],
                  upslope = purrr::map(seqno, ~ calc_upslope(.x, db)))
  }

  db <- db %>%
    # Update upslope_n
    dplyr::mutate(upslope_n = purrr::map_int(upslope, length)) %>%
    # Update watershed number
    mutate_cond(shedno %in% w_rm$shedno,
                shedno = w_rm$out_shed)
}
