#' Split channel segments
#'
#' From original FoxPro code:
#'
#' Procedure to compute the length of each unique channel segment between
#' channel start points (1) and end points (5,6) and major junction points (2),
#' tributary junction points (3,7) or pit centre points (5).  If a channel
#' segment is longer than the selected maximum allowable segment length
#' (currently set by the program at 200 m) then the segment is divided into
#' a number of segments of equal length.  The length of each new, shorter,
#' segment is 1/n of the total initial length of the original segment and
#' n is the number of times the initial segment is longer than 200 m)
#' The end result is insertion of a segment split point to break up any
#' channel segment into shorter segments no longer than 200 m.
#'
#'
#' @noRd

split_segments <- function(db, grid, chan_length) {

  segs <- db %>%
    dplyr::filter(.data$seedtype %in% c(1, 2, 8), .data$drec != .data$seqno) %>%
    dplyr::arrange(dplyr::desc(.data$elev)) %>%
    dplyr::select("seqno", "row", "col", "elev", "upslope", "chan_no", "drec", "seedtype")

  # Split into segments
  segs <- get_segments(seqno = segs$seqno, drec = db$drec,
                       seedtype = db$seedtype,
                       grid = grid, chan_length = chan_length,
                       seqno_all = db$seqno)

  dplyr::bind_cols(dplyr::select(db, -"seedtype"),
                   dplyr::select(segs, "seedtype"))

}

get_segments <- function(seqno, drec, seedtype, grid, chan_length, seqno_all) {
  variable <- list(seedtype = seedtype)
  static <- list(grid = grid, chan_length = chan_length,
                 seqno = seqno_all, drec = drec)

  if(length(seqno) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- trace(seqno = seqno, drec = drec, loop_func = split_segments_indiv,
                    s = static, v = variable)

  data.frame(variable[c("seedtype")])
}

split_segments_indiv <- function(t, s, v) {

  # Find first non-0 seedtype in track
  end <- which(v$seedtype[t[-1]] != 0)[1] + 1
  t <- t[1:(end-1)]

  # Get length
  seg_length <- length(t) * s$grid

  if(seg_length > s$chan_length) {
    n_segs <- as.integer(seg_length/s$chan_length) + 1
    new_seg_length <- as.integer(length(t)/n_segs)

    splits <- seq(1, length(t), new_seg_length)[2:n_segs]

    # Change seedtype to 4 at each junction
    # As long as not a pit that drains to self and next is 0, change next cell to 8
    for(i in splits) {
      v$seedtype[t][i] <- 4
      if(s$drec[t][i] != s$seqno[t][i] &&
         v$seedtype[t][i+1] == 0) v$seedtype[t][i+1] <- 8
    }
  }
  v
}
