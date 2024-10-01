#' Force flow to channel
#'
#' From original FoxPro code:
#'
#' Procedure to try again to reset drecs to force flow into channels but
#' at the same time to ensure that flow from non-channel cells is always
#' into the lowest channel cell below it, except for cells that are
#' immediately upslope from a junction.
#' If a cell can flow into a choice of a type 3, 7 or 2 cell, it should
#' flow into a type 7
#'
#' `point_in`
#' Procedure to force all cells that are adjacent to and upslope of a main
#' channel junction, but are not already marked as belonging to a different
#' channel to point into (or flow into) this main channel junction cell. This
#' was added to prevent some flow paths from bleeding past a channel junction to
#' add their area to a downslope catchment. It allows me to revert to defining a
#' junction as the cell that is upslope of a cell that displays and increase in
#' upslope area of > the threshold value
#'
#' @noRd
flow_to_channels <- function(db) {

  check <- db %>%
    dplyr::filter(.data$chan_no > 0, .data$seedtype != 2) %>%
    nb_values(db = db, max_cols = max(db$col), col = "seqno", db_sub = .,
              format = "wide")

  elev <- check$elev
  seqno <- check$seqno
  for(i in c(1:4, 6:9)) {
    n <- db[check[[paste0("seqno_n", i)]],]
    n_chan_no <- n$chan_no
    n_elev <- n$elev

    change_drec <- !is.na(n_elev) & !is.na(n_chan_no) & n_chan_no == 0 & n_elev > elev
    db$drec[check[[paste0("seqno_n", i)]]][change_drec] <- seqno[change_drec]
  }

  db
}
