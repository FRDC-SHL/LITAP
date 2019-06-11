# Relief calculations type 3

calc_relz <- function(db, idb, str_val = 10000, ridge_val = 10000, verbose = TRUE) {

  if(verbose) message("Calculating streams")
  temp1 <- db %>%
    dplyr::mutate(shedno = fill_shed) %>%
    calc_stream(str_val = str_val, verbose = verbose)

  temp2 <- calc_pit(db)

  if(verbose) message("Calculating ridges")
  temp3 <- calc_stream(idb, str_val = ridge_val, verbose = verbose) %>%
    dplyr::rename(cr_row = str_row, cr_col = str_col, cr_elev = str_elev,
                  z2cr = z2st, n2cr = n2st) %>%
    dplyr::mutate(cr_elev = max(db$elev, na.rm = TRUE) - cr_elev) #Convert back to orig elev

  temp4 <- calc_pit(idb) %>%
    dplyr::rename(peak_seqno = pit_seqno, peak_row = pit_row, peak_col = pit_col,
                  peak_elev = pit_elev, z2peak = z2pit, n2peak = n2pit) %>%
    dplyr::mutate(peak_elev = max(db$elev, na.rm = TRUE) - peak_elev)

  temp <- dplyr::left_join(temp1, temp2) %>%
    dplyr::left_join(temp3) %>%
    dplyr::left_join(temp4) %>%
    dplyr::left_join(dplyr::select(db, seqno, row, col, buffer, fill_shed))

  if(verbose) message("Calculating relief")
  calc_relief(temp)
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
                     str_row = 0, str_col = 0, str_elev = 0,
                     z2st = 0, n2st = 0)
  db_temp <- dplyr::arrange(db, shedno, dplyr::desc(elev), upslope)

  seqno_order <- db_temp$seqno

  db_temp <- dplyr::arrange(db, seqno)

  for(i in seqno_order){
    if(verbose) message(i)

    track <- trace_flow2(i, db_temp)

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

# use pond db
calc_pit <- function(db) {

  temp <- db %>%
    dplyr::filter(ldir == 5) %>%
    dplyr::select(shedno, pit_seqno = seqno, pit_row = row, pit_col = col, pit_elev = elev) %>%
    dplyr::full_join(dplyr::select(db, seqno, shedno, elev), ., by = "shedno") %>%
    dplyr::mutate(z2pit = elev - pit_elev,
                  z2pit = replace(z2pit, z2pit < 0, 0),
                  n2pit = dplyr::if_else(is.na(shedno), 0, as.numeric(NA)))

  seqno_order <- dplyr::arrange(db, upslope, dplyr::desc(elev)) %>%
    dplyr::pull(seqno)

  # While some not assigned, loop over them
  while(any(is.na(temp$n2pit))) {
    # Get the next one which isn't filled
    i <- seqno_order[which(is.na(temp$n2pit[seqno_order]))[1]]
    track <- trace_flow2(i, db)
    temp$n2pit[track] <- seq(length(track) - 1, along.with = track, by = -1)
  }
  temp
}

# calcrelief3
calc_relief <- function(relz) {
  relz %>%
    dplyr::mutate(min_elev = min(elev, na.rm = TRUE),
                  max_elev = max(elev, na.rm = TRUE),
                  elev_range = max_elev - min_elev) %>%
    dplyr::group_by(fill_shed) %>%
    dplyr::mutate(max_elev_shed = max(elev, na.rm = TRUE)) %>%
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
}

