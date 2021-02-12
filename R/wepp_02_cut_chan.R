#' Cut channels
#'
#'
#' Original FoxPro:
#'
#' "Procedure to burn or cut channels into the DEM.  The FlowMapR pit
#' removing procedures remove pits by resetting flow directions to flow
#' upslope from a pit center cell to the pour point at which the pit
#' overspills. This results in production of flow paths that flow uphill.
#' Such upslope flow paths confound efforts to number channel segments
#' sequentially from highest to lowest, as these procedures use the value
#' for the elevation of the last cell in each channel segment to order
#' channel segments from highest to lowest.  The last cell in an upslope
#' channel segment produced by pit removing will always be higher than
#' the elevation of the start cell at the pit center. This produces incorrect
#' sequencing for ordering channel segments.
#'
#' In order to remove this problem and order channel segments properly, this
#' procedure was added.  It follows each channel from its start to its finish
#' and identifies any points along the channel where the elevation of any
#' point is higher than the elevation of the previous cell.  It notes this
#' and continues along the flow path until a cell is reached that has a
#' value for elevation lower than that of the cell at which upward flow was
#' first detected.  It goes back to the point at which the flow path began
#' to flow upward and flows back along the path changing the value for
#' elevation for every cell along the path.  The new value is interpolated
#' using a linear interpolation between the highest elevation (the elevation
#' at the original pit center) and the lowest elevation (the elevation at
#' the first point that is lower in elevation than the original pit center."
#'
#' @noRd
#'
cut_chan <- function(db, upslope_threshold) {

  chan <- dplyr::filter(db, .data$seedtype == 1) %>%
    dplyr::arrange(dplyr::desc(.data$elev))

  variable <- list(seedtype = db$seedtype,
                   elev = db$elev)

  static <- list(upslope_threshold = upslope_threshold)

  if(length(db$seqno) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- trace(seqno = chan$seqno, drec = db$drec, loop_func = fix_elev,
                    s = static, v = variable)

  db$orig_elev <- db$elev
  db$elev <- variable$elev
  db$seedtype <- variable$seedtype

  db
}

fix_elev <- function(t, s, v) {

  t <- unique(t)

  v$seedtype[t[-1]][v$seedtype[t[-1]] == 1] <- 0

  new_elev <- v$elev[t]
  n <- length(new_elev)

  while(any(diffs <- new_elev > dplyr::lag(new_elev), na.rm = TRUE)) {
    start <- which(diffs > 0)[1] - 1
    end <- max(which(new_elev[start:n] > new_elev[start]) + start)
    len <- end - start - 1

    new_elev[(start + 1):(end-1)] <- new_elev[start] -
      (1:len * (new_elev[start] - new_elev[end])/len)
  }
  v$elev[t] <- new_elev
  v
}

