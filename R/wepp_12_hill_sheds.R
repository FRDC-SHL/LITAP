#' Compute and assign WEPP hillslopes
#'
#' From original FoxPro code:
#'
#' "Procedure to compute and assign every cell in a WEPP DBF file a value
#' to identify the WEPP hillslope watershed that the cell belongs to
#' Each WEPP hillslope shed is assigned an integer ID number equal to the
#' integer ID number of the channel segment that it drains to.
#' The procedure also computes and stores, for each grid cell, whether it
#' belongs to a flow path that flows into the WEPP channel segment from
#' the top (1), left (2) or right (3) side.
#' After all cells have been labelled with the segment number that they
#' drain to and the direction (top, left, right) that they drain from,
#' the procedure goes back through the database to renumber each hillslope
#' segment sequentially into the field hill_no"
#'
#' @noRd
#'
hill_sheds <- function(db) {
  db2 <- db %>%
    dplyr::select("seqno", "row", "col", "elev", "ddir", "drec", "chan_side", "segment_no") %>%
    dplyr::mutate(shed_no = 0, shed_side = 0, hill_no = 0) %>%
    dplyr::arrange(dplyr::desc(.data$elev))

  if(nrow(db2) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- list(shed_no = rep(0, length = nrow(db)),
                   shed_side = rep(0, length = nrow(db)),
                   n = 1)

  static <- list(segment_no = db$segment_no, chan_side = db$chan_side)

  # trace flow for shed chan_side and no
  variable <- trace(seqno = db2$seqno, drec = db$drec, loop_func = get_hills,
                    s = static, v = variable)

  db$shed_side <- variable$shed_side
  db$shed_no <- variable$shed_no

  # Remove assignment where segment_no != 0) (seems weird but clearly in original)
  db$shed_no[db$segment_no != 0] <- 0
  db$shed_side[db$segment_no != 0] <- 0

  # Label hills
  hill_index <- db %>%
    dplyr::filter(!(.data$shed_no == 0 & .data$shed_side == 0)) %>%
    dplyr::select("shed_no", "shed_side") %>%
    dplyr::arrange(.data$shed_no, .data$shed_side) %>%
    dplyr::distinct() %>%
    dplyr::mutate(hill_no = 1:dplyr::n())

  dplyr::left_join(db, hill_index, by = c("shed_no", "shed_side"))
}

# Get tracks to first
#   a) (not already labelled) segment_no
#   b) (already labelled) shed_no
#
# Data to keep:
# a) keep first segment_no as "shed_no", keep LAST "side" (where segment_no == 0, so previous cell) as "shed_side"
# b) If already labeled keep first shed_no as "shed_no", keep last  "shed_side" as "shed_side"


get_hills <- function(t, s, v) {
  new <- which(s$segment_no[t] != 0)[1]
  old <- which(v$shed_no[t] != 0)[1]

  if(new != 1 && !is.na(new) && ((!is.na(old) & new <= old) | is.na(old))) {
    v$shed_no[t[1:new]] <- s$segment_no[t][new]

    # Get the last chan side which matches
    chan_side <- s$chan_side[t][1:(new-1)]
    segment_no <- s$segment_no[t][1:(new-1)]
    chan_side <- rev(chan_side[chan_side != 0 & segment_no == 0])[1]
    v$shed_side[t[1:new]] <- chan_side
  }

  if(!is.na(old) && ((!is.na(new) & old < new) | is.na(new))) {
    v$shed_no[t[1:old]] <- v$shed_no[t][old]
    v$shed_side[t[1:old]] <- v$shed_side[t][old]
  }

  v$n <- v$n + 1
  v
}
