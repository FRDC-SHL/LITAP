# Relief calculations type 3

calc_relz <- function(db, idb, verbose = TRUE) {

  temp1 <- calc_stream(db, verbose = verbose)
  temp2 <- calc_pit(db)
  temp3 <- calc_stream(idb) %>%
    dplyr::rename(cr_row = str_row, cr_col = str_col, cr_elev = str_elev,
                  z2cr = z2st, n2cr = n2st)
  temp4 <- calc_pit(idb) %>%
    dplyr::rename(peak_seqno = pit_seqno, peak_row = pit_row, peak_col = pit_col,
                  peak_elev = pit_elev, z2peak = z2pit, n2peak = n2pit)

  temp <- dplyr::left_join(temp1, temp2) %>%
    dplyr::left_join(temp3) %>%
    dplyr::left_join(temp4)

  calc_relief(temp)
}

# To speed this up, consider breaking by watershed, or by flow-to-a-pit groups
# and running in purrr::map or something similar

calc_stream <- function(db, str_val = 10000, verbose = TRUE) {
  num_dn <- 0

  str_row <- 0
  str_col <- 0
  str_elev <- 0

  temp <- data.frame(seqno = db$seqno,
                     str_row = 0, str_col = 0, str_elev = 0,
                     z2st = 0, n2st = 0)
  db_temp <- dplyr::arrange(db, shedno, dplyr::desc(elev), upslope)

  seqno_order <- db_temp$seqno

  db_temp <- dplyr::arrange(db, seqno)

  for(i in seqno_order){
    if(verbose) message(i)

    track <- trace_flow2(i, db_temp)

    # Get first cell already visited
    visited <- which(temp$z2st[track] > 0)[1]

    # Get first channel cell
    channel <- which(db_temp$upslope[track] > str_val)[1]

    # Get final cell
    end <- track[min(c(visited, channel, length(track)), na.rm = TRUE)]
    num_dn <- which(track == end)

    # Get new track
    track <- track[1:num_dn]

    # If we go to the end (i.e. no visited, no channels)
    # OR have a channel (and/or visited)
    if((is.na(visited) && is.na(channel)) || !is.na(channel)){

      temp$str_row[track] <- db_temp$row[end]
      temp$str_col[track] <- db_temp$col[end]
      temp$str_elev[track] <- db_temp$elev[end]

      temp$z2st[track] <- db_temp$elev[track] - db_temp$elev[end]
      temp$n2st[track] <- num_dn

    # If we go to the first cell already visited (i.e. cell is NOT channel cell)
    } else {

      temp$str_row[track] <- temp$str_row[end]
      temp$str_col[track] <- temp$str_col[end]
      temp$str_elev[track] <- temp$str_elev[end]

      temp$z2st[track] <- db_temp$elev[track] - temp$str_elev[end]
      temp$n2st[track] <- seq(num_dn + temp$n2st[end], by = -1, along.with = track)
    }
  }
  temp$n2st <- temp$n2st - 1
  temp$z2st[temp$z2st < 0] <- 0

  temp
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
    dplyr::group_by(shedno) %>%
    dplyr::mutate(max_elev_shed = max(elev, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(zpit2peak = z2pit + z2peak,
                  zcr2st = z2cr + z2st,
                  zcr2pit = z2cr + z2pit,
                  z2top = max_elev_shed - elev,
                  ztop2pit = max_elev_shed - pit_elev,
                  ncr2st = n2cr + n2st,
                  pctz2top = 100 - round((z2top / ztop2pit) * 100),
                  pctz2st = round((z2st / zcr2st) * 100),
                  pctz2pit = round((z2pit / zpit2peak) * 100),
                  pctN2st = round((n2st / ncr2st) * 100),
                  pmin2max = round(((elev - min_elev) / elev_range) * 100))

}

