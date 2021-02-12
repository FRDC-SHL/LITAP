remark_chan <- function(db, upslope_threshold) {
  chan <- db %>%
    dplyr::arrange(dplyr::desc(.data$elev)) %>%
    dplyr::filter(.data$seedtype == 1, .data$upslope > upslope_threshold)

  chan <- get_chan(seqno = chan$seqno, drec = db$drec,
                   upslope = db$upslope, elev = db$elev,
                   ddir = db$ddir,
                   upslope_threshold = upslope_threshold, fix_elev = FALSE)


  db <- dplyr::bind_cols(dplyr::select(db, -"chan_no", -"seedtype"),
                         dplyr::select(chan, "chan_no", "seedtype"))

  db <- check_flow2lower(db, upslope_threshold)

  # Check cells above junctions

  neighbours <- nb_values(db, max_cols = max(db$col), col = "seqno",
                          format = "wide") %>%
    dplyr::select(dplyr::contains("seqno_n"))

  check <- db$seqno[db$seedtype == 2 & !is.na(db$seedtype)]
  for(s in check) {
    aj <- above_junct(seqno = s, drec = db$drec, seedtype = db$seedtype,
                      chan_no = db$chan_no, elev = db$elev, upslope = db$upslope,
                      neighbours = neighbours)
    db$seedtype <- aj$seedtype
    db$drec <- aj$drec
  }

  db
}

check_flow2lower <- function(db, upslope_threshold) {
  # Now check type 2 junctions - look for neighbours with channels

  chan <- dplyr::filter(db, .data$seedtype == 2)

  if(nrow(chan) > 0) {
    chan <- chan %>%
      nb_values(db = db, max_cols = ncol(db),
                col = c("seqno", "chan_no", "drec"), db_sub = .) %>%
      dplyr::filter(.data$chan_no != 0, .data$drec_n == .data$seqno)

    # Now look for the neighbours of those neighbours for possibly lower flow points
    chan2 <- nb_values(db = db, max_cols = max(db$col),
                       col = c("seqno", "chan_no", "elev", "upslope"),
                       db_sub = db[chan$seqno_n,]) %>%
      dplyr::filter(.data$chan_no_n > 0, .data$upslope_n > upslope_threshold) %>%
      dplyr::group_by(seqno) %>%
      dplyr::filter(.data$elev_n == min(.data$elev_n)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    db$drec[chan2$seqno] <- chan2$seqno_n
    db$seedtype[unique(chan2$seqno_n)] <- 2
  }
  db
}


above_junct <- function(seqno, drec, seedtype, chan_no, elev, upslope, neighbours) {
  n <- unlist(neighbours[seqno,])[-5]

  upslope_cells <- (chan_no[n] > 0) & drec[n] == seqno
  upslope_max_cell <- n[upslope_cells][which.max(upslope[n[upslope_cells]])]

  # Must have 2 or more separate channel segments to join
  if(sum(upslope_cells) > 1) {
    # Get tributaries (not max and unassigned)
    ups <- n[upslope_cells]
    s_update <- ups[ups != upslope_max_cell & seedtype[ups] == 0]
    seedtype[s_update] <- 3

    # Get main channel upslope of junction cell
    if(seedtype[upslope_max_cell] == 0) {

      seedtype[upslope_max_cell] <- 7

      # Point neighbours of max upslope cell which no assigned towards the upslope
      # cell.
      n <- unlist(neighbours[upslope_max_cell,])[-5]
      move_drec <- n[chan_no[n] == 0 & elev[n] > elev[upslope_max_cell]]
      drec[move_drec] <- upslope_max_cell
    }
  }
  list(seedtype = seedtype, drec = drec)
}
