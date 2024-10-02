#' Compute channel lengths for WEPP
#'
#' From original FoxPro code:
#'
#' WEPP_LEN
#' "procedure to compute 3 measures of length to channel for use in the
#'  WEPP model.  Measures are N2ST, L2ST and Z2ST"
#'
#' go_stream
#'
#' "Procedure to go to each grid cell starting at the highest cell in each watershed (the peak cell)
#'  and to follow a flow path from each cell DOWNSLOPE from it to the first channel cell to which
#'  it is connected by downstream flow (where downstream flow is computed as flow from a grid
#'  cell to its LOWEST downslope neighbor).  As the algorithm traverses the DEM matrix it
#'  computes the following for every grid cell it encounters:
#'  st_row - the row number of the nearest channel cell
#' 	st_col  - the col number of the nearest channel cell
#'  st_elev - the elevation of the nearest channel cell
#'  Z2st = elevation of current cell - elevation of channel cell
#'  N2st = number of grid cells between current cell & crest"
#'
#' @noRd
#'

wepp_len <- function(db, grid) {

  db2 <- dplyr::arrange(db, dplyr::desc(.data$elev), .data$upslope)

  if(nrow(db2) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- list(n2st = rep(0, length = nrow(db)),
                   l2st = rep(0, length = nrow(db)),
                   z2st = rep(0, length = nrow(db)))

  static <- list(db = db, grid = grid)

  # trace flow for shed chan_side and no
  variable <- trace(seqno = db2$seqno, drec = db$drec, loop_func = get_length,
                    s = static, v = variable)

  db$n2st <- variable$n2st
  db$l2st <- variable$l2st
  db$z2st <- variable$z2st

  dplyr::mutate(db,
                n2st = replace(.data$n2st, .data$seqno == .data$drec, 0),
                l2st = replace(.data$l2st, .data$seqno == .data$drec, 0),
                z2st = replace(.data$z2st, .data$seqno == .data$drec, 0))
}

get_length <- function(t, s, v) {

  end <- which(!is.na(s$db$segment_no[t]))[1]

  if(!is.na(end) && end != 1 && v$n2st[t[1]] == 0) {

    t2 <- t[1:end]

    elev_end <- s$db$elev[t[end]]
    col_end <- s$db$col[t[end]]
    row_end  <- s$db$row[t[end]]

    if(sum(v$n2st[t2]) != 0) t3 <- t2[1:which(v$n2st[t2] != 0)[1]] else t3 <- t2

    v$n2st[t3] <- 1:(length(t3))
    v$l2st[t3] <- (sqrt((col_end - s$db$col[t3])^2 +
                          (row_end - s$db$row[t3])^2)) * s$grid
    v$z2st[t3] <- s$db$elev[t3] - elev_end
    v$l2st[t3] <- round(sqrt(v$l2st[t3]^2 + (v$z2st[t3] * s$grid)^2), 1)
  }
  v
}
