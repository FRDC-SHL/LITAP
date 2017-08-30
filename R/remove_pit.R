first_pitr <- function(db, max_area = 10, max_depth = 0.5, verbose = FALSE) {

  # Working with initial_shed
  db$shedno <- db$initial_shed

  if(max_area > 0 | max_depth > 0) {  # No point if nothing to remove
    # Initial Shed Statistics
    w_stats <- pit_stat(db, verbose = verbose) %>%
      out_stat() %>%
      dplyr::arrange(dplyr::desc(pit_elev), varatio) %>%
      dplyr::mutate(remove = (pit_area <= max_area | ((pour_elev - pit_elev) <= max_depth)) &
                      !edge_pit) #which to remove (only if not edge pit!)

    # In sequence, for each watershed that should be removed
    # Combine if too small (max_area) or too shallow (max_depth)
    done <- FALSE
    while(!done){

      w_rm <- w_stats %>%
        dplyr::filter(remove == TRUE) %>%
        dplyr::slice(1) # Get next to remove

      if(nrow(w_rm) == 1){
        sheds <- c(w_rm$shedno, w_rm$out_shed)
        if(verbose) message("    Combining watersheds ", paste0(sheds, collapse = " and "))

        db <- remove_pit(w_rm, w_stats, db)

        # Update shed statistics but only for the two sheds involved
        w_stats <- w_stats %>%
          dplyr::filter(!(shedno %in% sheds)) %>%
          dplyr::bind_rows(pit_stat(db, w = sheds, verbose = verbose)) %>%
          # Which to remove
          dplyr::mutate(remove = (pit_area <= max_area | ((pour_elev - pit_elev) <= max_depth)) &
                          !edge_pit) %>% #which to remove (only if not edge pit!)
          # Sort order
          dplyr::arrange(dplyr::desc(pit_elev), varatio)

        shed_rm <- sheds[!(sheds %in% w_stats$shedno)] # Shed removed
        shed_kept <- sheds[sheds %in% w_stats$shedno] # Shed kept

        if(any(!is.na(w_stats$out_shed))){
          # Update references to old shed
          w_stats <- w_stats %>%
            mutate_cond(out_shed == shed_rm, out_shed = shed_kept)
        }

        w_stats <- w_stats %>%
          out_stat()

      } else done <- TRUE
    }
  }

  # Save as local_shed numbers
  db <- dplyr::mutate(db, local_shed = shedno)

  return(db)
}

#' @import magrittr
second_pitr <- function(db, verbose = FALSE) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat(db, verbose = verbose) %>%
    out_stat() %>%
    dplyr::arrange(pit_elev, varatio) %>% ## shedord2  (PITELEV*10^10 + varatio) TAG SHEDORD2
    dplyr::mutate(removed = FALSE, final = FALSE, next_pit = out_shed, becomes = 0)

  # Starting conditions
  pond <- w_stats
  removed <- vector()
  final_pits <- vector()
  done <- FALSE
  w <- w_stats$shedno[1]

  while(!done){

    #w_rm <- find_lowest(w, w_stats, final_pits = pond$shedno[pond$final])
    w_rm <- find_lowest(w, w_stats, final_pits = final_pits, removed = removed, verbose = verbose)

    w_focal <- dplyr::filter(w_stats, shedno == w_rm$shedno)
    w_drain <- dplyr::filter(w_stats, shedno == w_rm$out_shed)

    if(!(w_focal$edge_pit & w_drain$edge_pit)) {

      new_shed <- max(db$shedno, na.rm = TRUE) + 1
      if(verbose) message("  Combining sheds ", w_focal$shedno, " and ", w_drain$shedno, " into new shed ", new_shed)

      removed <- c(removed, w_rm$shedno, w_rm$out_shed)

      # Remove shed
      db <- remove_pit(w_rm, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_rm$shedno, w_rm$out_shed),
                                       new_shed))

      # Update shed statistics but only for the two sheds involved

      w_stats <- w_stats %>%
        mutate_cond(shedno %in% c(w_focal$shedno, w_drain$shedno),
                    removed = TRUE,
                    final = w_rm$at_final,
                    next_pit = out_shed,
                    becomes = new_shed) %>%
        #dplyr::filter(!(shedno %in% c(w_focal$shedno, w_drain$shedno))) %>%
        dplyr::bind_rows(
          dplyr::mutate(pit_stat(db, w = new_shed, verbose = verbose),
                        removed = FALSE,
                        final = FALSE,
                        next_pit = out_shed,
                        becomes = 0)) %>%
        # Sort order
        dplyr::arrange(pit_elev, varatio)

      # Update references to old shed
      if(any(!is.na(w_stats$out_shed))){
        w_stats <- w_stats %>%
          mutate_cond(out_shed %in% c(w_focal$shedno, w_drain$shedno), out_shed = new_shed) %>%
          mutate_cond(removed == FALSE & final == FALSE & (next_pit %in% c(w_focal$shedno, w_drain$shedno)),
                      next_pit = new_shed)
      }

      w_stats <- w_stats %>%
        out_stat()

      # Keep track of pond statistics (ie. keep track of all watersheds)
      # pond <- dplyr::bind_rows(w_focal, w_drain) %>%
      #   dplyr::mutate(removed = TRUE,
      #                 final = w_rm$at_final,
      #                 next_pit = out_shed,
      #                 becomes = new_shed) %>%
      #   dplyr::bind_rows(pond, .)
      pond <- dplyr::bind_rows(pond, w_stats[w_stats$shedno == new_shed,]) %>%
        mutate_cond(shedno %in% c(w_focal$shedno, w_drain$shedno),
                    removed = TRUE,
                    final = w_rm$at_final,
                    next_pit = out_shed,
                    becomes = new_shed)

      pond$next_pit[pond$removed == FALSE & pond$final == FALSE &
                     (pond$next_pit == w_focal$shedno | pond$next_pit == w_drain$shedno)] <- new_shed

    } else {
      if(verbose) message("  Watersheds ", w_focal$shedno, " and ", w_drain$shedno, " are FINAL sheds")
      final_pits <- unique(c(final_pits, w_rm$shedno, w_rm$out_shed))

      w_stats <- w_stats %>%
        mutate_cond(shedno %in% c(w_focal$shedno, w_drain$shedno), final = TRUE)

      # pond <- pond %>%
      #   mutate_cond(shedno %in% w_focal$shedno, next_pit = w_drain$shedno) %>%
      #   mutate_cond(shedno %in% w_drain$shedno, next_pit = w_focal$shedno)
      # p <- dplyr::bind_rows(w_focal, w_drain) %>%
      #   dplyr::mutate(removed = FALSE,
      #                 final = TRUE,
      #                 next_pit = out_shed,
      #                 becomes = 0)#w_stats[w_stats$shedno %in% c(w_rm$shedno, w_rm$out_shed), ]
    }

    # Get next shed
    finished <- unique(c(final_pits, removed))

    w <- dplyr::filter(w_stats, !(shedno %in% finished))
    if(nrow(w) > 0) {
      w <-  w$shedno[1]
    } else done <- TRUE

  }

  # Update final sheds

  pond <- mutate_cond(pond, shedno %in% final_pits, final = TRUE)

  # Save as pond_shed numbers
  db <- dplyr::mutate(db, pond_shed = shedno)

  return(list("db" = db, "stats" = pond))
}

third_pitr <- function(db, verbose = FALSE) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat(db, verbose = verbose) %>%
    out_stat() %>%
    dplyr::arrange(varatio) %>%
    dplyr::mutate(out_shed_orig = out_shed,
                  next_pit = out_shed,
                  end_pit = pond_shed,
                  removed = FALSE)

  # Starting conditions
  fill <- tibble::tibble()
  done <- FALSE
  finished <- vector()
  w <- w_stats$shedno[1]

  while(!done){

    w_focal <- dplyr::filter(w_stats, shedno == w)
    w_drain <- dplyr::filter(w_stats, shedno == w_focal$out_shed)

    # if(verbose) message("  Assessing ", w)

    if(w_focal$end_pit == w_drain$end_pit) {
      new_shed <- max(db$shedno, na.rm = TRUE) + 1
      if(verbose) message("  Combining sheds ", w_focal$shedno, " and ", w_drain$shedno, " to new shed ", new_shed)

      db <- remove_pit(w_focal, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_focal$shedno, w_drain$shedno),
                                       new_shed))

      # Update shed statistics but only for the two sheds involved
      w_new <- pit_stat(db, w = new_shed, verbose = verbose) %>%
        dplyr::mutate(out_shed_orig = out_shed,
                      next_pit = out_shed,
                      end_pit = pond_shed,
                      removed = FALSE)

      w_stats <- w_stats %>%
        dplyr::filter(!(shedno %in% c(w_focal$shedno, w_drain$shedno))) %>%
        dplyr::bind_rows(w_new) %>%
        out_stat() %>%
        # Sort order
        dplyr::arrange(varatio)

      # Update references to old shed
      if(any(!is.na(w_stats$out_shed))){
        w_stats <- w_stats %>%
          mutate_cond(out_shed %in% c(w_focal$shedno, w_drain$shedno),
                      out_shed_orig = out_shed, #save original outshed direction
                      out_shed = new_shed)
      }


      w_stats <- w_stats %>%
        out_stat()

      # Update fill data frame
      fill <- dplyr::bind_rows(dplyr::bind_cols(w_focal, becomes = new_shed),
                               dplyr::bind_cols(w_drain, becomes = new_shed)) %>%
        dplyr::mutate(final = FALSE) %>%
        dplyr::arrange(shedno) %>%
        dplyr::bind_rows(fill, .)

      finished <- c(finished, w_focal$shedno, w_drain$shedno)

    } else {
      finished <- c(finished, w_focal$shedno)
    }


    # Get next shed
    w <- dplyr::filter(w_stats, !(shedno %in% finished))
    if(nrow(w) > 0) {
      w <-  w$shedno[1]
    } else done <- TRUE

  }

  # When all is done, add sheds that weren't removed
  add <- w_stats
  if(nrow(fill) > 0) add <- dplyr::filter(add, !(shedno %in% fill$shedno) & removed == FALSE)

  add <- add %>%
    dplyr::mutate(becomes = shedno, final = TRUE) %>%
    dplyr::arrange(shedno)

  fill <- dplyr::bind_rows(fill, add) %>%
    dplyr::mutate(next_pit = out_shed,
                  out_shed = out_shed_orig) %>%
    dplyr::select(-out_shed_orig)

  db <- dplyr::mutate(db, fill_shed = shedno)

  return(list("db" = db, "stats" = fill))
}

remove_pit <- function(w_rm, w_stats, db, verbose = FALSE) {

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

  #message("Removing watershed number ", w_rm$shedno, ": ")

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
