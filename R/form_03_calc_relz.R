# Relief calculations type 3

calc_relz <- function(db, idb, str_val = 10000, ridge_val = 10000, pond = NULL,
                      verbose = FALSE) {

  if(verbose) message("  Calculating streams")
  streams <- db %>%
    dplyr::mutate(shedno = fill_shed) %>%
    calc_stream3(str_val = str_val, verbose = verbose)

  if(verbose) message("  Calculating stream to pits")
  str2pits <- db %>%
    dplyr::mutate(shedno = local_shed) %>%
    calc_pit3(pond = pond, verbose = verbose)

  if(verbose) message("  Calculating ridges")
  ridges <- calc_stream3(idb, str_val = ridge_val, verbose = verbose) %>%
    dplyr::rename(cr_row = str_row, cr_col = str_col, cr_elev = str_elev,
                  z2cr = z2st, n2cr = n2st) %>%
    dplyr::mutate(cr_elev = max(db$elev, na.rm = TRUE) - cr_elev) #Convert back to orig elev

  if(verbose) message("  Calculating ridges to pits")

  ridge2pits <- calc_pit3(idb, verbose = verbose) %>%
    dplyr::rename(peak_seqno = pit_seqno, peak_row = pit_row, peak_col = pit_col,
                  peak_elev = pit_elev, z2peak = z2pit, n2peak = n2pit) %>%
    dplyr::mutate(peak_elev = max(db$elev, na.rm = TRUE) - peak_elev)

  all <- dplyr::left_join(streams, str2pits, by = "seqno") %>%
    dplyr::left_join(ridges, by = "seqno") %>%
    dplyr::left_join(ridge2pits, by = "seqno") %>%
    dplyr::left_join(dplyr::select(db, seqno, elev, row, col, buffer, fill_shed),
                     by = "seqno")

  all[is.na(all$elev), c("z2st", "z2cr")] <- NA

  if(verbose) message("  Calculating relief")

  calc_relief(all)
}

# To speed this up, consider breaking by watershed, or by flow-to-a-pit groups
# and running in purrr::map or something similar
calc_stream_sub <- function(db_shed, db, relz, str_val, verbose) {

}


calc_stream <- function(db, str_val = 10000, verbose = TRUE) {
  num_dn <- 0

  str_row <- 0
  str_col <- 0
  str_elev <- 0

  relz <- data.frame(seqno = db$seqno,
                     str_row = as.numeric(NA), str_col = as.numeric(NA),
                     str_elev = as.numeric(NA),
                     z2st = as.numeric(0), n2st = as.numeric(NA))

  db_temp <- dplyr::select(db, seqno, row, col, elev, upslope, shedno) %>%
    dplyr::filter(!is.na(elev)) %>%
    dplyr::arrange(shedno, dplyr::desc(elev), upslope)

  seqno_order <- db_temp$seqno

  db_temp <- dplyr::arrange(db, seqno)

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

      relz$str_row[track] <- db_temp$row[end]
      relz$str_col[track] <- db_temp$col[end]
      relz$str_elev[track] <- db_temp$elev[end]

      relz$z2st[track] <- db_temp$elev[track] - db_temp$elev[end]
      relz$n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

    # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      relz$str_row[track] <- relz$str_row[end]
      relz$str_col[track] <- relz$str_col[end]
      relz$str_elev[track] <- relz$str_elev[end]

      relz$z2st[track] <- db_temp$elev[track] - relz$str_elev[end]
      relz$n2st[track] <- seq(num_dn + relz$n2st[end] - 1, by = -1, along.with = track)
    }
  }

  relz$z2st[relz$z2st < 0] <- 0

  relz
}

calc_stream2 <- function(db, str_val = 10000, verbose = TRUE) {

  num_dn <- 0
  str_row <- rep(as.numeric(NA), nrow(db))
  str_col <- rep(as.numeric(NA), nrow(db))
  str_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(0, nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select(seqno, row, col, elev, upslope, shedno) %>%
    dplyr::filter(!is.na(elev)) %>%
    dplyr::arrange(shedno, dplyr::desc(elev), upslope) %>%
    dplyr::pull(seqno)

  db_temp <- dplyr::arrange(db, seqno)
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

      str_row[track] <- db_row[end]
      str_col[track] <- db_col[end]
      str_elev[track] <- db_elev[end]

      z2st[track] <- db_elev[track] - db_elev[end]
      n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

      # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      str_row[track] <- str_row[end]
      str_col[track] <- str_col[end]
      str_elev[track] <- str_elev[end]

      z2st[track] <- db_temp$elev[track] - str_elev[end]
      n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, str_row, str_col, str_elev, z2st, n2st)
}




calc_stream3 <- function(db, str_val = 10000, verbose = TRUE) {

  str_row <- rep(as.numeric(NA), nrow(db))
  str_col <- rep(as.numeric(NA), nrow(db))
  str_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(0, nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select(seqno, row, col, elev, upslope, shedno) %>%
    dplyr::filter(!is.na(elev)) %>%
    dplyr::arrange(shedno, dplyr::desc(elev), upslope) %>%
    dplyr::pull(seqno)

  db_temp <- dplyr::arrange(db, seqno)
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

        str_row[track] <- db_row[end]
        str_col[track] <- db_col[end]
        str_elev[track] <- db_elev[end]

        z2st[track] <- db_elev[track] - db_elev[end]
        n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

        # If we go to the first cell already visited (i.e. cell is NOT channel cell)
      } else {

        str_row[track] <- str_row[end]
        str_col[track] <- str_col[end]
        str_elev[track] <- str_elev[end]

        z2st[track] <- db_elev[track] - str_elev[end]
        n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
      }
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, str_row, str_col, str_elev, z2st, n2st)
}


calc_stream4 <- function(db, str_val = 10000, verbose = TRUE) {

  str_row <- rep(as.numeric(NA), nrow(db))
  str_col <- rep(as.numeric(NA), nrow(db))
  str_elev <- rep(as.numeric(NA), nrow(db))
  z2st <- rep(as.numeric(NA), nrow(db))
  n2st <- rep(as.numeric(NA), nrow(db))

  seqno_order <- db %>%
    dplyr::select(seqno, row, col, elev, upslope, shedno) %>%
    dplyr::filter(!is.na(elev)) %>%
    dplyr::arrange(shedno, dplyr::desc(elev), upslope) %>%
    dplyr::pull(seqno)

  db_temp <- dplyr::arrange(db, seqno)
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

      str_row[track] <- db_row[end]
      str_col[track] <- db_col[end]
      str_elev[track] <- db_elev[end]

      z2st[track] <- db_elev[track] - db_elev[end]
      n2st[track] <- seq(num_dn-1, by = -1, along.with = track)

      # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      str_row[track] <- str_row[end]
      str_col[track] <- str_col[end]
      str_elev[track] <- str_elev[end]

      z2st[track] <- db_temp$elev[track] - str_elev[end]
      n2st[track] <- seq(num_dn + n2st[end] - 1, by = -1, along.with = track)
    }
  }

  z2st[z2st < 0] <- 0

  data.frame(seqno = db_temp$seqno, str_row, str_col, str_elev, z2st, n2st)
}


calc_pit <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    browser()
    temp <- db %>%
      dplyr::filter(ddir == 5) %>%
      dplyr::select(shedno, pit_seqno = seqno,
                    pit_row = row, pit_col = col, pit_elev = elev)
  } else {
    temp <- pond %>%
      dplyr::select(shedno, pit_seqno, pit_row, pit_col, pit_elev)
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, seqno, shedno, elev), ., by = "shedno") %>%
    dplyr::mutate(z2pit = elev - pit_elev,
                  z2pit = replace(z2pit, z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, upslope, dplyr::desc(elev)) %>%
    dplyr::filter(!is.na(shedno), !is.na(elev)) %>%
    dplyr::pull(seqno)

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

  dplyr::select(temp, -shedno, -elev)
}

calc_pit2 <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    temp <- db %>%
      dplyr::filter(ddir == 5) %>%
      dplyr::select(shedno, pit_seqno = seqno,
                    pit_row = row, pit_col = col, pit_elev = elev)
  } else {
    temp <- pond %>%
      dplyr::select(shedno, pit_seqno, pit_row, pit_col, pit_elev)
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, seqno, shedno, elev), ., by = "shedno") %>%
    dplyr::mutate(z2pit = elev - pit_elev,
                  z2pit = replace(z2pit, z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, upslope, dplyr::desc(elev)) %>%
    dplyr::filter(!is.na(shedno), !is.na(elev)) %>%
    dplyr::pull(seqno)

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
    dplyr::select(-shedno, -elev)
}

calc_pit3 <- function(db, pond = NULL, verbose) {

  if(is.null(pond) || nrow(pond) == 0) {
    temp <- db %>%
      dplyr::filter(ddir == 5) %>%
      dplyr::select(shedno, pit_seqno = seqno,
                    pit_row = row, pit_col = col, pit_elev = elev)
  } else {
    temp <- pond %>%
      dplyr::select(shedno, pit_seqno, pit_row, pit_col, pit_elev)
  }

  temp <- temp %>%
    dplyr::full_join(dplyr::select(db, seqno, shedno, elev), ., by = "shedno") %>%
    dplyr::mutate(z2pit = elev - pit_elev,
                  z2pit = replace(z2pit, z2pit < 0, 0),
                  n2pit = as.numeric(NA))

  seqno_order <- dplyr::arrange(db, upslope, dplyr::desc(elev)) %>%
    dplyr::filter(!is.na(shedno), !is.na(elev)) %>%
    dplyr::pull(seqno)

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
    dplyr::select(-shedno, -elev)
}






# calcrelief3
calc_relief <- function(relz) {
  r <- relz %>%
    dplyr::mutate(min_elev = min(elev, na.rm = TRUE),
                  max_elev = max(elev, na.rm = TRUE),
                  elev_range = max_elev - min_elev) %>%
    dplyr::group_by(fill_shed) %>%
    dplyr::mutate(max_elev_shed = max_na(elev)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(zpit2peak = z2pit + z2peak,
                  zcr2st = z2cr + z2st,
                  zcr2pit = z2cr + z2pit,
                  z2top = max_elev_shed - elev,
                  ztop2pit = max_elev_shed - pit_elev,
                  ncr2st = n2cr + n2st,
                  pctz2top = trunc(100 - (z2top / ztop2pit) * 100),
                  pctz2st = trunc((z2st / zcr2st) * 100),
                  pctz2pit = trunc((z2pit / zpit2peak) * 100),
                  pctn2st = trunc((n2st / ncr2st) * 100),
                  pmin2max = trunc(((elev - min_elev) / elev_range) * 100))

  r$pctz2top[r$ztop2pit == 0] <- 0
  r$pctz2st[r$zcr2st == 0] <- 0
  r$pctn2st[r$ncr2st == 0] <- 0
  r$pmin2max[r$elev_range == 0] <- 0

  r

}

