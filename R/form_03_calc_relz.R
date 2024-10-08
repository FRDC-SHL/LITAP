# Relief calculations type 3

calc_relz <- function(db, idb, str_val, ridge_val, pond = NULL,
                      verbose) {

  if(verbose) message("  Calculating streams")
  streams <- db %>%
    dplyr::mutate(shedno = .data$fill_shed) %>%
    calc_stream3(str_val = str_val, verbose = verbose)

  if(verbose) message("  Calculating stream to pits")
  str2pits <- db %>%
    dplyr::mutate(shedno = .data$local_shed) %>%
    calc_pit3(pond = pond, verbose = verbose)

  if(verbose) message("  Calculating ridges") # Inverse
  ridges <- idb %>%
    dplyr::mutate(shedno = .data$inv_local_shed) %>%
    calc_stream3(str_val = ridge_val, verbose = verbose) %>%
    dplyr::rename("cr_row" = "st_row", "cr_col" = "st_col", "cr_elev" = "st_elev",
                  "z2cr" = "z2st", "n2cr" = "n2st") %>%
    dplyr::mutate(cr_elev = max(db$elev, na.rm = TRUE) - .data$cr_elev) #Convert back to orig elev

  if(verbose) message("  Calculating ridges to pits")

  ridge2pits <- idb %>%
    dplyr::mutate(shedno = .data$inv_local_shed) %>%
    calc_pit3(verbose = verbose) %>%
    dplyr::rename("peak_seqno" = "pit_seqno", "peak_row" = "pit_row", "peak_col" = "pit_col",
                  "peak_elev" = "pit_elev", "z2peak" = "z2pit", "n2peak" = "n2pit") %>%
    dplyr::mutate(peak_elev = max(db$elev, na.rm = TRUE) - .data$peak_elev)

  all <- dplyr::left_join(streams, str2pits, by = "seqno") %>%
    dplyr::left_join(ridges, by = "seqno") %>%
    dplyr::left_join(ridge2pits, by = "seqno") %>%
    dplyr::left_join(dplyr::select(db, "seqno", "elev", "row", "col", "buffer", "fill_shed"),
                     by = "seqno")

  all[is.na(all$elev), c("z2st", "z2cr")] <- NA

  if(verbose) message("  Calculating relief")

  calc_relief(all)
}

# To speed this up, consider breaking by watershed, or by flow-to-a-pit groups
# and running in purrr::map or something similar
calc_stream_sub <- function(db_shed, db, relz, str_val, verbose) {

}


calc_stream <- function(db, str_val, verbose) {
  num_dn <- 0

  st_row <- 0
  st_col <- 0
  st_elev <- 0

  relz <- data.frame(seqno = db$seqno,
                     st_row = as.numeric(NA), st_col = as.numeric(NA),
                     st_elev = as.numeric(NA),
                     z2st = as.numeric(0), n2st = as.numeric(NA))

  db_temp <- dplyr::select(db, "seqno", "row", "col", "elev", "upslope", "shedno") %>%
    dplyr::filter(!is.na(.data$elev)) %>%
    dplyr::arrange(.data$shedno, dplyr::desc(.data$elev), .data$upslope)

  seqno_order <- db_temp$seqno

  db_temp <- dplyr::arrange(db, .data$seqno)

  for(i in seqno_order){
    track <- trace_flow2(i, db_temp$drec)

    # Get first cell already visited
    visited <- which(relz$z2st[track] > 0)[1]

    # Get first channel cell
    channel <- which(db_temp$upslope[track] >= str_val)[1]

    # Get final cell
    end <- track[min(c(visited, channel, length(track)), na.rm = TRUE)]
    num_dn <- which(track == end)

    # Get new track
    track <- track[1:num_dn]

    # Update channel and visited
    visited <- which(relz$z2st[track] > 0)[1]
    channel <- which(db_temp$upslope[track] >= str_val)[1]

    # If we go to the end (i.e. no visited, no channels)
    # OR have a channel (and/or visited)
    if((is.na(visited) && is.na(channel)) || !is.na(channel)){

      relz$st_row[track] <- db_temp$row[end]
      relz$st_col[track] <- db_temp$col[end]
      relz$st_elev[track] <- db_temp$elev[end]

      relz$z2st[track] <- db_temp$elev[track] - db_temp$elev[end]
      relz$n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

    # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      relz$st_row[track] <- relz$st_row[end]
      relz$st_col[track] <- relz$st_col[end]
      relz$st_elev[track] <- relz$st_elev[end]

      relz$z2st[track] <- db_temp$elev[track] - relz$st_elev[end]
      relz$n2st[track] <- seq(num_dn + relz$n2st[end] - 1, by = -1, along.with = track)
    }
  }

  relz$z2st[relz$z2st < 0] <- 0

  relz
}

calc_stream2 <- function(db, str_val, verbose) {

  num_dn <- 0
  st_row <- rep(as.numeric(NA), nrow(db))
  st_col <- rep(as.numeric(NA), nrow(db))
  st_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(0, nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select("seqno", "row", "col", "elev", "upslope", "shedno") %>%
    dplyr::filter(!is.na(.data$elev)) %>%
    dplyr::arrange(.data$shedno, dplyr::desc(.data$elev), .data$upslope) %>%
    dplyr::pull(.data$seqno)

  db_temp <- dplyr::arrange(db, .data$seqno)
  db_upslope <- db_temp$upslope
  db_row <- db_temp$row
  db_col <- db_temp$col
  db_elev <- db_temp$elev
  db_drec <- db_temp$drec

  for(i in seqno_order){
    track <- trace_flow_fast(i, db_drec)

    # Get first cell already visited
    visited <- which(z2st[track] > 0)[1]

    # Get first channel cell
    channel <- which(db_upslope[track] >= str_val)[1]

    # Get final cell
    end <- track[min(c(visited, channel, length(track)), na.rm = TRUE)]
    num_dn <- which(track == end)

    # Get new track
    track <- track[1:num_dn]

    # Update channel and visited
    visited <- which(z2st[track] > 0)[1]
    channel <- which(db_upslope[track] >= str_val)[1]

    # If we go to the end (i.e. no visited, no channels)
    # OR have a channel (and/or visited)
    if((is.na(visited) && is.na(channel)) || !is.na(channel)){

      st_row[track] <- db_row[end]
      st_col[track] <- db_col[end]
      st_elev[track] <- db_elev[end]

      z2st[track] <- db_elev[track] - db_elev[end]
      n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

      # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      st_row[track] <- st_row[end]
      st_col[track] <- st_col[end]
      st_elev[track] <- st_elev[end]

      z2st[track] <- db_temp$elev[track] - st_elev[end]
      n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, st_row, st_col, st_elev, z2st, n2st)
}




calc_stream3 <- function(db, str_val, verbose) {

  st_row <- rep(as.numeric(NA), nrow(db))
  st_col <- rep(as.numeric(NA), nrow(db))
  st_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(0, nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select("seqno", "row", "col", "elev", "upslope", "shedno") %>%
    dplyr::filter(!is.na(.data$elev)) %>%
    dplyr::arrange(.data$shedno, dplyr::desc(.data$elev), .data$upslope) %>%
    dplyr::pull(.data$seqno)

  db_temp <- dplyr::arrange(db, .data$seqno)
  db_upslope <- db_temp$upslope
  db_row <- db_temp$row
  db_col <- db_temp$col
  db_elev <- db_temp$elev
  db_drec <- db_temp$drec

  n_cells <- 250
  seqno_track_groups <- seq(n_cells, length(seqno_order) + n_cells - 1, by = n_cells)

  for(a in seqno_track_groups) {
    seqno_sub <- seqno_order[(a-(n_cells-1)):a]
    # Remove extra cells on last batch
    if(a == seqno_track_groups[length(seqno_track_groups)]) {
      seqno_sub <- seqno_sub[!is.na(seqno_sub)]
    }
    track_group <- trace_flow_all(cells = seqno_sub, db_drec)

    for(b in 1:length(seqno_sub)){
      i <- seqno_sub[b]

      track <- track_group[,b]
      track <- unique(track)

      # Get first cell already visited
      visited <- which(z2st[track] > 0)[1]

      # Get first channel cell
      channel <- which(db_upslope[track] >= str_val)[1]

      # Get final cell
      end <- track[min(c(visited, channel, length(track)), na.rm = TRUE)]
      num_dn <- which(track == end)

      # Get new track
      track <- track[1:num_dn]

      # Update channel and visited
      visited <- which(z2st[track] > 0)[1]
      channel <- which(db_upslope[track] >= str_val)[1]

      # If we go to the end (i.e. no visited, no channels)
      # OR have a channel (and/or visited)
      if((is.na(visited) && is.na(channel)) || !is.na(channel)){

        st_row[track] <- db_row[end]
        st_col[track] <- db_col[end]
        st_elev[track] <- db_elev[end]

        z2st[track] <- db_elev[track] - db_elev[end]
        n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

        # If we go to the first cell already visited (i.e. cell is NOT channel cell)
      } else {

        st_row[track] <- st_row[end]
        st_col[track] <- st_col[end]
        st_elev[track] <- st_elev[end]

        z2st[track] <- db_elev[track] - st_elev[end]
        n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
      }
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, st_row, st_col, st_elev, z2st, n2st)
}


calc_stream4 <- function(db, str_val, verbose) {

  st_row <- rep(as.numeric(NA), nrow(db))
  st_col <- rep(as.numeric(NA), nrow(db))
  st_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(as.numeric(NA), nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select("seqno", "row", "col", "elev", "upslope", "shedno") %>%
    dplyr::filter(!is.na(.data$elev)) %>%
    dplyr::arrange(.data$shedno, dplyr::desc(.data$elev), .data$upslope) %>%
    dplyr::pull(.data$seqno)

  db_temp <- dplyr::arrange(db, .data$seqno)
  db_upslope <- db_temp$upslope
  db_row <- db_temp$row
  db_col <- db_temp$col
  db_elev <- db_temp$elev
  db_drec <- db_temp$drec

  for(i in seqno_order){
    ii <- i
    num_dn <- 0
    track <- vector()
    while(!done) {
      track <- c(track, ii)
      num_dn <- num_dn + 1
      v <- z2st[ii] > 0
      c <- db_upslope[ii] > 0
      e <- db_drec[ii] == ii
      done <- any(v, c, e, na.rm = TRUE)
    }

    end <- ii

    # If we go to the end (i.e. no visited, no channels)
    # OR have a channel (and/or visited)
    if(!v && !c || c){

      st_row[track] <- db_row[end]
      st_col[track] <- db_col[end]
      st_elev[track] <- db_elev[end]

      z2st[track] <- db_elev[track] - db_elev[end]
      n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

      # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      st_row[track] <- st_row[end]
      st_col[track] <- st_col[end]
      st_elev[track] <- st_elev[end]

      z2st[track] <- db_temp$elev[track] - st_elev[end]
      n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, st_row, st_col, st_elev, z2st, n2st)
}


calc_pit <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    temp <- db %>%
      dplyr::filter(ddir == 5) %>%
      dplyr::select("shedno", "pit_seqno" = "seqno",
                    "pit_row" = "row", "pit_col" = "col", "pit_elev" = "elev")
  } else {
    temp <- dplyr::select(pond, "shedno", "pit_seqno", "pit_row", "pit_col", 'pit_elev')
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, "seqno", "shedno", "elev"), ., by = "shedno") %>%
    dplyr::mutate(z2pit = .data$elev - .data$pit_elev,
                  z2pit = replace(.data$z2pit, .data$z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, .data$upslope, dplyr::desc(.data$elev)) %>%
    dplyr::filter(!is.na(.data$shedno), !is.na(.data$elev)) %>%
    dplyr::pull(.data$seqno)

  # While some not assigned, loop over them
  # if(verbose) {
  #   s <- sum(is.na(temp$n2pit[seqno_order]))
  #   pb <- progress::progress_bar$new(total = s)
  # }

  while(any(is.na(temp$n2pit[seqno_order]))) {
    # Get the next one which isn't filled
    i <- seqno_order[is.na(temp$n2pit[seqno_order])][1]
    track <- trace_flow2(i, db$drec)
    temp$n2pit[track] <- seq(length(track) - 1, along.with = track, by = -1)
  }

  dplyr::select(temp, -"shedno", -"elev")
}

calc_pit2 <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    temp <- db %>%
      dplyr::filter(.data$ddir == 5) %>%
      dplyr::select("shedno", "pit_seqno" = "seqno",
                    "pit_row" = "row", "pit_col" = "col", "pit_elev" = "elev")
  } else {
    temp <- dplyr::select(pond, "shedno", "pit_seqno", "pit_row", "pit_col", "pit_elev")
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, "seqno", "shedno", "elev"), ., by = "shedno") %>%
    dplyr::mutate(z2pit = .data$elev - .data$pit_elev,
                  z2pit = replace(.data$z2pit, .data$z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, .data$upslope, dplyr::desc(.data$elev)) %>%
    dplyr::filter(!is.na(.data$shedno), !is.na(.data$elev)) %>%
    dplyr::pull(.data$seqno)

  # For speed, pull out what is needed
  n2pit <- temp$n2pit

  # While some not assigned, loop over them
  # if(verbose) {
  #   s <- sum(is.na(n2pit[seqno_order]))
  #   pb <- progress::progress_bar$new(total = s)
  # }

  db_drec <- db$drec

  for(i in seqno_order) {
    if(is.na(n2pit[i])) { # Get the next one which isn't filled
      track <- trace_flow_fast(i, db_drec)
      n2pit[track] <- seq(length(track) - 1, along.with = track, by = -1)
    }
  }

  dplyr::mutate(temp, n2pit = !!n2pit) %>%
    dplyr::select(-"shedno", -"elev")
}

calc_pit3 <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    temp <- db %>%
      dplyr::filter(.data$ddir == 5) %>%
      dplyr::select("shedno", "pit_seqno" = "seqno",
                    "pit_row" = "row", "pit_col" = "col", "pit_elev" = "elev")
  } else {
    temp <- dplyr::select(pond, "shedno", "pit_seqno", "pit_row", "pit_col", "pit_elev")
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, "seqno", "shedno", "elev"), ., by = "shedno") %>%
    dplyr::mutate(z2pit = .data$elev - .data$pit_elev,
                  z2pit = replace(.data$z2pit, .data$z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, .data$upslope, dplyr::desc(.data$elev)) %>%
    dplyr::filter(!is.na(.data$shedno), !is.na(.data$elev)) %>%
    dplyr::pull(.data$seqno)

  # For speed, pull out what is needed
  n2pit <- temp$n2pit

  # While some not assigned, loop over them
  # if(verbose) {
  #   s <- sum(is.na(n2pit[seqno_order]))
  #   pb <- progress::progress_bar$new(total = s)
  # }

  db_drec <- db$drec

  n_cells <- 250
  seqno_track_groups <- seq(n_cells, length(seqno_order) + n_cells - 1, by = n_cells)

  for(a in seqno_track_groups) {
    seqno_sub <- seqno_order[(a-(n_cells-1)):a]
    # Remove extra cells on last batch
    if(a == seqno_track_groups[length(seqno_track_groups)]) {
      seqno_sub <- seqno_sub[!is.na(seqno_sub)]
    }
    track_group <- trace_flow_all(cells = seqno_sub, db_drec)

    for(b in 1:length(seqno_sub)){
      i <- seqno_sub[b]
      if(is.na(n2pit[i])) { # Get the next one which isn't filled
        track <- track_group[,b]
        track <- unique(track)
        n2pit[track] <- seq(length(track) - 1, along.with = track, by = -1)
      }
    }
  }

  dplyr::mutate(temp, n2pit = !!n2pit) %>%
    dplyr::select(-"shedno", -"elev")
}






# calcrelief3
calc_relief <- function(relz) {
  r <- relz %>%
    dplyr::mutate(min_elev = min(.data$elev, na.rm = TRUE),
                  max_elev = max(.data$elev, na.rm = TRUE),
                  elev_range = .data$max_elev - .data$min_elev) %>%
    dplyr::group_by(.data$fill_shed) %>%
    dplyr::mutate(max_elev_shed = max_na(.data$elev)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(zpit2peak = .data$z2pit + .data$z2peak,
                  zcr2st = .data$z2cr + .data$z2st,
                  zcr2pit = .data$z2cr + .data$z2pit,
                  z2top = .data$max_elev_shed - .data$elev,
                  ztop2pit = .data$max_elev_shed - .data$pit_elev,
                  ncr2st = .data$n2cr + .data$n2st,
                  pctz2top = trunc(100 - (.data$z2top / .data$ztop2pit) * 100),
                  pctz2st = trunc((.data$z2st / .data$zcr2st) * 100),
                  pctz2pit = trunc((.data$z2pit / .data$zpit2peak) * 100),
                  pctn2st = trunc((.data$n2st / .data$ncr2st) * 100),
                  pmin2max = trunc(((.data$elev - .data$min_elev) / .data$elev_range) * 100))

  r$pctz2top[r$ztop2pit == 0] <- 0
  r$pctz2st[r$zcr2st == 0] <- 0
  r$pctn2st[r$ncr2st == 0] <- 0
  r$pmin2max[r$elev_range == 0] <- 0

  r

}

