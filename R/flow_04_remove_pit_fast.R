first_pitr1 <- function(db, max_area, max_depth, verbose) {

  # Working with initial_shed
  db$shedno <- db$initial_shed

  if(max_area > 0 | max_depth > 0) {  # No point if nothing to remove

    # # Calculate neighbouring sheds to split up files
    # shed_groups <- db %>%
    #   dplyr::filter(ridge == TRUE) %>%
    #   dplyr::select(seqno, shedno) %>%
    #   nb_values(db, max_cols = max(db$col), col = "shedno", db_sub = .) %>%
    #   dplyr::select(-seqno, -n) %>%
    #   dplyr::filter(!is.na(shedno_n), shedno != shedno_n) %>%
    #   dplyr::distinct() %>%
    #   dplyr::arrange(shedno, shedno_n)

    # Initial Shed Statistics
    w_stats <- pit_stat1(db, verbose = verbose) %>%
      out_stat() %>%
      dplyr::arrange(dplyr::desc(pit_elev), varatio) %>%
      dplyr::mutate(remove = (pit_area <= max_area |
                                ((pour_elev - pit_elev) <= max_depth)) &
                      !edge_pit) #which to remove (only if not edge pit!)

    # In sequence, for each watershed that should be removed
    # Combine if too small (max_area) or too shallow (max_depth)
    done <- FALSE
    while(!done){

      w_rm <- w_stats %>%
        dplyr::filter(remove == TRUE) %>%
        dplyr::slice(1) # Get next to remove

      if(nrow(w_rm) == 1){
        sheds <- c(w_rm$shedno, w_rm$drains_to)
        if(verbose) message("    Combining watersheds ", paste0(sheds, collapse = " and "))

        db <- remove_pit1(w_rm, w_stats, db, update_elev = TRUE)

        # Update shed statistics but only for the two sheds involved
        w_stats <- w_stats %>%
          dplyr::filter(!(shedno %in% sheds)) %>%
          dplyr::bind_rows(pit_stat1(db, w = sheds, verbose = verbose)) %>%
          # Which to remove
          dplyr::mutate(remove = (pit_area <= max_area |
                                    ((pour_elev - pit_elev) <= max_depth)) &
                          !edge_pit) %>% #which to remove (only if not edge pit!)
          # Sort order
          dplyr::arrange(dplyr::desc(pit_elev), varatio)

        shed_rm <- sheds[!(sheds %in% w_stats$shedno)] # Shed removed
        shed_kept <- sheds[sheds %in% w_stats$shedno] # Shed kept

        if(any(!is.na(w_stats$drains_to))){
          # Update references to old shed
          w_stats <- dplyr::mutate(w_stats,
                                   drains_to = dplyr::if_else(.data$drains_to == shed_rm,
                                                              shed_kept,
                                                              .data$drains_to))
        }

        w_stats <- out_stat(w_stats)

      } else done <- TRUE
    }
  }

  if(verbose) message("\n")

  # Update uced
  if(verbose) message("    Calculating upslope cumulative elevation drop")
  db <- calc_upslopes(db, type = "uced")

  # Save as local_shed numbers
  dplyr::mutate(db,
                local_shed = shedno,
                local_ddir = ddir,
                local_uced = uced) %>%
    dplyr::select(-"shedno")
}


# Includes second vol2fl/mm2fl/parea calculations
second_pitr1 <- function(db, verbose) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat1(db, verbose = verbose) %>%
    out_stat() %>%
    dplyr::arrange(pit_elev, varatio) %>% ## shedord2  (PITELEV*10^10 + varatio) TAG SHEDORD2
    dplyr::mutate(removed = FALSE, final = FALSE, next_pit = drains_to, becomes = 0)

  # Starting conditions
  pond <- w_stats
  removed <- vector()
  final_pits <- vector()
  done <- FALSE
  w <- w_stats$shedno[1]

  while(!done){

    #w_rm <- find_lowest(w, w_stats, final_pits = pond$shedno[pond$final])
    w_rm <- find_lowest(w, w_stats, final_pits = final_pits,
                        removed = removed, verbose = verbose)

    w_focal <- dplyr::filter(w_stats, shedno == w_rm$shedno)
    w_drain <- dplyr::filter(w_stats, shedno == w_rm$drains_to)

    if(!(w_focal$edge_pit & w_drain$edge_pit)) {

      new_shed <- max(db$shedno, na.rm = TRUE) + 1
      if(verbose) message("  Combining sheds ", w_focal$shedno, " and ",
                          w_drain$shedno, " into new shed ", new_shed)

      removed <- c(removed, w_rm$shedno, w_rm$drains_to)

      # Remove shed
      db <- remove_pit1(w_rm, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_rm$shedno, w_rm$drains_to),
                                       new_shed))

      # Update shed statistics but only for the two sheds involved
      shed_update <- w_stats$shedno %in% c(w_focal$shedno, w_drain$shedno)
      w_stats$removed[shed_update] <- TRUE
      w_stats$final[shed_update] <- w_rm$at_final


      # drains_to set as out_shed in original pit_stat1
      w_stats$next_pit[shed_update] <- w_stats$drains_to[shed_update]
      w_stats$becomes[shed_update] <- new_shed

      w_stats <- dplyr::bind_rows(
        w_stats,
        dplyr::mutate(pit_stat1(db, w = new_shed, verbose = verbose),
                      removed = FALSE,
                      final = FALSE,
                      next_pit = drains_to,
                      becomes = 0)) %>%
        # Sort order
        dplyr::arrange(pit_elev, varatio)

      # Update references to old shed
      if(any(!is.na(w_stats$drains_to))){

        w_stats <- w_stats %>%
          dplyr::mutate(
            drains_to = dplyr::if_else(
              drains_to %in% c(w_focal$shedno, w_drain$shedno),
              new_shed, .data$drains_to),
            next_pit = dplyr::if_else(
              removed == FALSE & final == FALSE &
                (.data$next_pit %in% c(w_focal$shedno, w_drain$shedno)),
              new_shed,
              .data$next_pit))
      }

      w_stats <- w_stats %>%
        out_stat()

      pre_vol <- dplyr::case_when(
        w_focal$pour_elev == w_drain$pour_elev ~ w_focal$pit_vol + w_drain$pit_vol,
        w_focal$pour_elev < w_drain$pour_elev ~ w_focal$pit_vol,
        w_focal$pour_elev > w_drain$pour_elev ~ w_drain$pit_vol)

      w_stats$pre_vol[w_stats$shedno == new_shed] <- pre_vol

      # Keep track of pond statistics (ie. keep track of all watersheds)
      # pond <- dplyr::bind_rows(w_focal, w_drain) %>%
      #   dplyr::mutate(removed = TRUE,
      #                 final = w_rm$at_final,
      #                 next_pit = drains_to,
      #                 becomes = new_shed) %>%
      #   dplyr::bind_rows(pond, .)
      pond <- dplyr::bind_rows(pond, w_stats[w_stats$shedno == new_shed,])
      sheds_update <- pond$shedno %in% c(w_focal$shedno, w_drain$shedno)
      pond$removed[sheds_update] <- TRUE
      pond$final[sheds_update] <- w_rm$at_final
      pond$next_pit[sheds_update] <- pond$drains_to[sheds_update]
      pond$becomes[sheds_update] <- new_shed

      pond$next_pit[pond$removed == FALSE & pond$final == FALSE &
                     (pond$next_pit == w_focal$shedno | pond$next_pit == w_drain$shedno)] <- new_shed

      # Calc second vol2mm etc.
      vol <- db %>%
        dplyr::filter(shedno == new_shed) %>%
        dplyr::left_join(dplyr::select(w_stats, shedno, pour_elev, shed_area),
                         by = "shedno") %>%
        vol2fl(., verbose = verbose) %>%
        dplyr::mutate(shedno = new_shed)

      # Only replace cells with new overflows (i.e. elev must be in vol)
      db_new <- dplyr::filter(db, .data$shedno == !!new_shed, .data$elev %in% vol$elev,
                              parea == 0) %>%  # Only replace ones with no info
        dplyr::select(-vol2fl, -mm2fl, -parea) %>%
        dplyr::left_join(vol, by = c("shedno", "elev")) %>%
        dplyr::arrange(seqno)

      db_new[is.na(db_new$parea), c("mm2fl", "vol2fl", "parea")] <- 0

      db[db_new$seqno, c("vol2fl", "parea")] <- db_new[, c("vol2fl", "parea")]

    } else {
      if(verbose) message("  Watersheds ", w_focal$shedno, " and ",
                          w_drain$shedno, " are FINAL sheds")
      final_pits <- unique(c(final_pits, w_rm$shedno, w_rm$drains_to))

      w_stats$final[w_stats$shedno %in% c(w_focal$shedno, w_drain$shedno)] <- TRUE
    }

    # Get next shed
    finished <- unique(c(final_pits, removed))

    w <- dplyr::filter(w_stats, !(shedno %in% finished))
    if(nrow(w) > 0) {
      w <-  w$shedno[1]
    } else done <- TRUE

  }

  # Update final sheds
  pond$final[pond$shedno %in% final_pits] <- TRUE

  # Save as pond_shed numbers
  db <- dplyr::mutate(db, pond_shed = shedno) %>%
    dplyr::select(-"shedno")

  if(verbose) message("\n")

  list("db" = db, "stats" = pond)
}

third_pitr1 <- function(db, verbose) {

  # Working with local_shed
  db$shedno <- db$local_shed

  w_stats <- pit_stat1(db, verbose = verbose) %>%
    out_stat() %>%
    dplyr::arrange(varatio) %>%
    dplyr::mutate(drains_to_orig = drains_to,
                  next_pit = drains_to,
                  end_pit = pond_shed,
                  removed = FALSE)

  # Starting conditions
  fill <- tibble::tibble()
  done <- FALSE
  finished <- vector()
  w <- w_stats$shedno[1]

  while(!done){

    w_focal <- dplyr::filter(w_stats, shedno == w)
    w_drain <- dplyr::filter(w_stats, shedno == w_focal$drains_to)

    if(w_focal$end_pit == w_drain$end_pit) {
      new_shed <- max(db$shedno, na.rm = TRUE) + 1
      if(verbose) message("  Combining sheds ", w_focal$shedno, " and ",
                          w_drain$shedno, " to new shed ", new_shed)

      db <- remove_pit1(w_focal, w_stats, db) %>%
        dplyr::mutate(shedno = replace(shedno,
                                       shedno %in% c(w_focal$shedno, w_drain$shedno),
                                       new_shed))

      # Update shed statistics but only for the two sheds involved
      w_new <- pit_stat1(db, w = new_shed, verbose = verbose) %>%
        dplyr::mutate(drains_to_orig = drains_to,
                      next_pit = drains_to,
                      end_pit = pond_shed,
                      removed = FALSE)

      w_stats <- w_stats %>%
        dplyr::filter(!(shedno %in% c(w_focal$shedno, w_drain$shedno))) %>%
        dplyr::bind_rows(w_new) %>%
        out_stat() %>%
        # Sort order
        dplyr::arrange(varatio)

      # Update references to old shed
      if(any(!is.na(w_stats$drains_to))){
        w_stats <- dplyr::mutate(
          w_stats,
          drains_to_orig = .data$drains_to, #save original outshed direction
          drains_to = dplyr::if_else(.data$drains_to %in% c(w_focal$shedno, w_drain$shedno),
                                     new_shed, .data$drains_to))
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

      # Update mm2fl
      db_new <- db %>%
        dplyr::filter(shedno == new_shed, vol2fl != 0) %>%
        dplyr::left_join(dplyr::select(w_stats, shedno, shed_area),
                         by = "shedno") %>%
        dplyr::filter(mm2fl < w_focal$varatio) %>%
        dplyr::mutate(mm2fl = vol2fl/shed_area)

      db[db_new$seqno, "mm2fl"] <- db_new[, "mm2fl"]

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
    dplyr::mutate(next_pit = drains_to,
                  drains_to = drains_to_orig) %>%
    dplyr::select(-drains_to_orig)

  if(verbose) message("\n")

  # Update uced
  if(verbose) message("    Calculating new elevation differences")
  db <- calc_upslopes(db, type = "uced")

  db <- dplyr::rename(db, "fill_shed" = "shedno")

  list("db" = db, "stats" = fill)
}

remove_pit1 <- function(w_rm, w_stats, db, update_elev = FALSE, verbose) {

  w_rm <- w_rm %>%
    dplyr::mutate(direction = ifelse(pit_elev >= pit_elev_out, "out", "in"))

  if(w_rm$direction == "out"){
    w_rm <- dplyr::select(w_rm,
                          pit_seqno,
                          in_seqno, out_seqno,
                          pour_elev,
                          shedno, drains_to)
  } else {
    w_rm <- dplyr::select(w_rm,
                          pit_seqno = pit_seqno_out,
                          in_seqno = out_seqno, out_seqno = in_seqno,
                          pour_elev = pour_elev_out,
                          shedno = drains_to, drains_to = shedno)
  }

  ## Remove Pit

  # Reverse directions of these cells (both regular and pit cell)

  # - Second reflows the pit cell. NOTE, if pit cell is the pour point, then
  #   this may be NA (fixed in next step)
  # - Third reflow the last cell in the new flow path (in_seqno) to point
  #   towards the first cell in the next flow path (out_seqno). This fixes any
  #   NA's from the previous step

  # 1. Get original flow down to old pit
  new_flow <- trace_flow2(cell = w_rm$in_seqno, drec = db$drec)

  # Reverse directions
  new_ddir <- db[new_flow, ] %>%
    dplyr::mutate(ddir = 10 - dplyr::lag(ddir))

  # Fix pour_point direction to pour to new shed
  d <- nb_values(db, max_cols = max(db$col), col = "seqno",
                 db_sub = new_ddir[new_ddir$seqno == w_rm$in_seqno,]) %>%
    dplyr::filter(seqno_n == w_rm$out_seqno) %>%
    dplyr::pull(n)

  new_ddir$ddir[new_ddir$seqno == w_rm$in_seqno] <- d

  new_ddir <- flow_values(db, max_cols = max(db$col), col = "seqno", db_sub = new_ddir)

  # Apply new flow directions
  db$ddir[new_ddir$seqno] <- new_ddir$ddir
  db$drec[new_ddir$seqno] <- new_ddir$seqno_next

  # Get new flow path from old pit centre to new pit centre
  new_flow <- trace_flow2(cell = w_rm$pit_seqno, drec = db$drec)

  # Update upslope starting with old pit and looping through to new,
  #    adding upslope from previous to new cell along the way

  old_upslope <- c(0, rev(db$upslope[new_flow[1:which(new_flow == w_rm$in_seqno)]]))
  new_upslope <- cumsum(rev(diff(old_upslope, lag = 1)))

  # Correct old shed upslope
  db$upslope[new_flow[1:which(new_flow == w_rm$in_seqno)]] <- new_upslope

  # Add to new shed upslope
  db$upslope[new_flow[-(1:which(new_flow == w_rm$in_seqno))]] <- db$upslope[new_flow[-(1:which(new_flow == w_rm$in_seqno))]] + new_upslope[length(new_upslope)]

  # Update elevation of db (FlowMapR_2009.txt line 1964)
  if(update_elev) {
    db$elev[db$shedno == w_rm$shedno & db$elev < w_rm$pour_elev &
              !is.na(db$shedno) & !is.na(db$elev)] <- w_rm$pour_elev
  }

  # Update watershed number
  dplyr::mutate(db, shedno = dplyr::if_else(shedno %in% w_rm$shedno,
                                            w_rm$drains_to,
                                            shedno))
}
