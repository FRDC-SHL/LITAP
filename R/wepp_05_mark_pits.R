#' Mark pits
#'
#' Original FoxPro:
#' "Procedure to read in the locations of all pits in the DEM previously
#' computed and stored in the file ID#FILL and to then go to the recorded
#' pit centre location (identified by pitrec) and to mark the cell at that
#' location as being a pit cell.  If the cell is not located along a marked
#' channel then it is necessary to locate the first marked channel cell to
#' which the pit will drain as I have decided to link each pit cell to
#' the first downslope channel cell to which it will drain when full. This
#' approach allows me to compute depressional catchments that are located
#' both along marked stream channels and away from marked channels.  If a
#' pit is located away from a marked channel, it will be necessary to flow
#' down from the pit (along the integrated flow network) until the first
#' cell marked as a channel cell is encountered.  This cell can be identified
#' as the start cell for a channel segment that is downslope from the pit
#' and receives outflow from the pit (once it is full)."
#'
#' @noRd
mark_pits <- function(db, fill) {

  # Get watershed pits
  fill <- dplyr::group_by(fill, pit_seqno) %>%
    dplyr::slice(dplyr::n()) # Get last record of each pit

  # Mark all pit seeds types as pits
  db$seedtype[fill$pit_seqno] <- 5

  # Get all pit cells to start marking channels for
  channels <- db[fill$pit_seqno, ]


  # > -------------------------


  max_chan <- max(db$chan_no, na.rm = TRUE) + 1  # Update channel numbering

  # Working with pits where the drec != seqno
  ch1 <- channels %>%
    dplyr::filter(.data$chan_no == 0 & .data$drec != .data$seqno)

  if(nrow(ch1) > 0) {
    ch1 <- ch1 %>%
      dplyr::mutate(chan_no = max_chan:(max_chan + dplyr::n() - 1))

    db$seedtype[ch1$drec] <- 8
    db$chan_no[ch1$seqno] <- ch1$chan_no

    # Travel down and mark the rest of the cells
    cn <- get_pit_chan1(seqno = ch1$seqno, drec = db$drec,
                        chan_no = db$chan_no, seedtype = db$seedtype,
                        neighbours = dplyr::select(db, dplyr::contains("seqno_n")),
                        upslope = db$upslope, elev = db$elev)

    db[, c("chan_no", "seedtype", "drec")] <- cn
    max_chan <- max(db$chan_no, na.rm = TRUE) + 1  # Update channel numbering
  }

  # Working with pits where chan_no != 0 (ignore those with drec = seqno if already have channel)
  ch2 <- dplyr::filter(channels, .data$chan_no != 0 & .data$drec != .data$seqno)
  db$seedtype[ch2$drec] <- 8

  # Working with pits where the drec == seqno and no channels
  ch3 <- channels %>%
    dplyr::filter(.data$chan_no == 0 & .data$drec == .data$seqno)

  if(nrow(ch3) > 0) {
    ch3 <- ch3 %>%
      dplyr::rename("drec_orig" = "drec") %>%
      dplyr::left_join(dplyr::select(fill, "seqno" = "pit_seqno", "drec" = "out_seqno"),
                       by = "seqno") %>%
      dplyr::mutate(chan_no = max_chan:(max_chan + dplyr::n() - 1))

    db$chan_no[ch3$seqno] <- ch3$chan_no

    db2 <- db
    db2$chan_no[ch3$drec] <- dplyr::if_else(db$chan_no[ch3$drec] == 0, ch3$chan_no, db$chan_no[ch3$drec])

    # Travel down and mark the rest of the cells - NOTE STARTING FROM SECOND CELL!!!
    cn <- get_pit_chan2(seqno = ch3$drec, drec = db$drec,
                        chan_no = db$chan_no,
                        chan_no2 = db2$chan_no,
                        seedtype = db$seedtype,
                        neighbours = dplyr::select(db, dplyr::contains("seqno_n")),
                        upslope = db$upslope, elev = db$elev)

    db[, c("chan_no", "seedtype", "drec")] <- cn
  }

  db
}


get_pit_chan1 <- function(seqno, drec, chan_no, seedtype, neighbours, upslope, elev) {
  variable <- list(chan_no = chan_no,
                   seedtype = seedtype,
                   drec = drec)
  static <- list(neighbours = neighbours, upslope = upslope, elev = elev)

  # Because drec can be modified, can't use matrix format
  # if(length(seqno) > 500) trace <- trace_matrix
  trace <- trace_single

  variable <- trace(seqno = seqno, drec = drec, loop_func = mark_pit_chan1,
                    s = static, v = variable)

  data.frame(variable[c("chan_no", "seedtype", "drec")])
}

get_pit_chan2 <- function(seqno, drec, chan_no, chan_no2, seedtype, neighbours, upslope, elev) {
  variable <- list(chan_no = chan_no,
                   chan_no2 = chan_no2,
                   seedtype = seedtype,
                   drec = drec)
  static <- list(neighbours = neighbours, upslope = upslope, elev = elev)

  # Because drec can be modified, can't use matrix format
  # if(length(seqno) > 500) trace <- trace_matrix
  trace <- trace_single

  variable <- trace(seqno = seqno, drec = drec, loop_func = mark_pit_chan2,
                    s = static, v = variable)

  data.frame(variable[c("chan_no", "seedtype", "drec")])
}

mark_pit_chan <- function(t, s, v) {

  # Which channel number? If pit flowing to pourpoint, use second cell, otherwise first
  chan <- dplyr::if_else(v$chan_no[t[1]] == 0, v$chan_no[t[2]], v$chan_no[t[1]])

  # Find first marked cell in track
  first_marked <- which(v$chan_no[t[3:length(t)]] != 0 |
                          v$seedtype[t[3:length(t)]] %in% c(5, 6))
  first_marked <- first_marked[1] + 2 # Get first record and proper location in t

  # Mark un-marked channels with new channel
  v$chan_no[t[3:(first_marked - 1)]] <- chan

  # Fix last marked cell
  l <- t[first_marked]

  if(v$seedtype[l] == 0) {
    v$seedtype[l] <- 2
    aj <- above_junct(seqno = l,
                      drec = v$drec, seedtype = v$seedtype, chan_no = v$chan_no,
                      elev = s$elev, upslope = s$upslope, neighbours = s$neighbours)
    v$drec <- aj$drec
    v$seedtype <- aj$seedtype

  } else if(v$seedtype[l] == 1) {
    v$seedtype[l] <- 0
  } else if(v$seedtype[l] %in% c(5, 6)) {
    v$chan_no[l] <- chan
  } else if(v$seedtype[l] == 2) {
    aj <- above_junct(seqno = l,
                      drec = v$drec, seedtype = v$seedtype, chan_no = v$chan_no,
                      elev = s$elev, upslope = s$upslope, neighbours = s$neighbours)
    v$drec <- aj$drec
    v$seedtype <- aj$seedtype
  } #else if(v$seedtype[l] == 3) {
    #
    #} else if(v$seedtype[l] == 7) {
    #}

  v
}





mark_pit_chan1 <- function(t, s, v) {

  chan <- v$chan_no[t[1]]

  # Find first marked cell in track - Skip the first two as they should be 5 and 8
  first_marked <- which(v$chan_no[t[3:length(t)]] != 0 |
                          v$seedtype[t[3:length(t)]] %in% c(5, 6))

  if(length(first_marked) == 0) {
    first_marked <- length(t)
  } else {
    first_marked <- first_marked[1] + 2 # Get first record and proper location in t
  }

  # Mark un-marked channels with new channel
  v$chan_no[t[2:(first_marked - 1)]] <- chan

  # Adjust last marked cell
  l <- t[first_marked]

  if(v$seedtype[l] %in% c(0, 2)) {
    v$seedtype[l] <- 2
    aj <- above_junct(seqno = l,
                      drec = v$drec, seedtype = v$seedtype, chan_no = v$chan_no,
                      elev = s$elev, upslope = s$upslope, neighbours = s$neighbours)
    v$drec <- aj$drec
    v$seedtype <- aj$seedtype

  } else if(v$seedtype[l] == 1) {
    v$seedtype[l] <- 0
  } else if(v$seedtype[l] %in% c(5, 6)) {
    v$chan_no[l] <- chan
  }

  v
}

mark_pit_chan2 <- function(t, s, v) {

  chan <- v$chan_no2[t[1]]

  if(v$chan_no[t[1]] == 0) {
    v$seedtype[t[1]] <- 1
    v$chan_no[t[1]] <- chan
  }

  # Find first marked cell in track (except for the first one above)
  first_marked <- which(v$chan_no[t[-1]] != 0 | v$seedtype[t[-1]] %in% c(5, 6))
  if(length(first_marked) == 0) {
    first_marked <- length(t)
  } else {
    first_marked <- first_marked[1] + 1 # Get first record and proper location in t
  }

  # Mark un-marked channels with new channel
  v$chan_no[t[2:(first_marked - 1)]] <- chan

  # Adjust last marked cell
  l <- t[first_marked]

  if(v$seedtype[l] %in% c(0, 2)) {
    v$seedtype[l] <- 2
    aj <- above_junct(seqno = l,
                      drec = v$drec, seedtype = v$seedtype, chan_no = v$chan_no,
                      elev = s$elev, upslope = s$upslope, neighbours = s$neighbours)
    v$drec <- aj$drec
    v$seedtype <- aj$seedtype

  } else if(v$seedtype[l] == 1) {
    v$seedtype[l] <- 0
  } else if(v$seedtype[l] %in% c(5, 6)) {
    v$chan_no[l] <- chan
  }

  v
}
