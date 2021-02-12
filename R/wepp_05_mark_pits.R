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
    dplyr::slice(dplyr::n())

  # Mark all pit seeds types as pits
  db$seedtype[fill$pit_seqno] <- 5

  # Get all pit cells to start marking channels for
  channels <- db[fill$pit_seqno, ]
  # The way it works out in original script
  channels <- dplyr::filter(channels, .data$chan_no == 0 |
                              .data$drec != .data$seqno) %>%
    dplyr::left_join(dplyr::select(fill, .data$pit_seqno, .data$out_seqno),
                     by = c("seqno" = "pit_seqno")) %>%
    # If no flow from pit, flow to pour point
    dplyr::mutate(drec_orig = .data$drec,
                  drec = replace(.data$drec, .data$drec == .data$seqno,
                                 .data$out_seqno[.data$drec == .data$seqno]))

  # If no flow from pit, flow to pour point (add to db)
  new_drec <- dplyr::filter(channels, drec_orig == seqno)
  db <- dplyr::mutate(db, drec_orig = drec)
  db$drec[new_drec$seqno] <- new_drec$drec
  db$drec_to_pour_point <- FALSE
  db$drec_to_pour_point[new_drec$seqno] <- TRUE

  # Add new chan_no
  max_chan <- max(db$chan_no, na.rm = TRUE) + 1
  max_chan <- max_chan:(max_chan + nrow(channels)-1)
  channels$chan_no <- max_chan
  db$chan_no[channels$seqno] <- max_chan

  # Second cells are marked as seedtype 8, unless going through pour point
  db$seedtype[channels$drec] <- dplyr::case_when(
    channels$drec_orig == channels$seqno ~ 1, TRUE ~ 8)

  # Process channels only if second cell doesn't end flow
  channels <- channels[db$chan_no[channels$drec] == 0, ]

  # Second cells are marked with chan_no of pit
  db$chan_no[channels$drec] <- channels$chan_no

  # Get neighbouring seqnos
  db <- nb_values(db, max_cols = max(db$col),
                  col = "seqno", format = "wide")

  # Travel down and mark the rest of the cells
  channels <- get_pit_chan(seqno = channels$seqno, drec = db$drec,
                           chan_no = db$chan_no, seedtype = db$seedtype,
                           neighbours = dplyr::select(db, dplyr::contains("seqno_n")),
                           upslope = db$upslope, elev = db$elev)

  dplyr::bind_cols(dplyr::select(db, -"chan_no", -"seedtype", -"drec"),
                   channels) %>%
    dplyr::mutate(drec = replace(drec, drec_to_pour_point,
                                 drec_orig[drec_to_pour_point])) %>%
    dplyr::select(-drec_to_pour_point, -drec_orig)
}


get_pit_chan <- function(seqno, drec, chan_no, seedtype, neighbours, upslope, elev) {
  variable <- list(chan_no = chan_no,
                   seedtype = seedtype,
                   drec = drec)
  static <- list(neighbours = neighbours, upslope = upslope, elev = elev)

  # Because drec can be modified, can't use matrix format
  # if(length(seqno) > 500) trace <- trace_matrix
  trace <- trace_single

  variable <- trace(seqno = seqno, drec = drec, loop_func = mark_pit_chan,
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
