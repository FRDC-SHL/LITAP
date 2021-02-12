#' Re-compute local drain directions and add to DB and WEPP files
#'
#' Combines original redo_ddir and add_ddir functions from original FoxPro code:
#'
#' "Procedure to re-compute the local drain direction (ddir) again for every
#' cell in the WEPP file for which the DDIR has changed due to operations
#' related to defining channels and forcing flow into channels.
#' Re-computing DDIR ensures that this variable is not incorrect due to
#' changes.  An updated and correct value of DDIR is required for use in
#' the module that computes the direction (left, right, center) that
#' upslope channel segments enter a downslope segment from"
#'
#' "Procedure to add the local drain direction (ddir) value to the WEPP
#' Segment file (ID#SEGS) to identify the drainage direction of each start
#' cell in each segment.  The program only needs to know this for start
#' cells that are the first cells in a segment into which 2 or more upslope
#' segments drain.  Still, it proved easiest to just compute and store
#' this value for the start cell of every WEPP segment."
#'
#' @noRd



redo_ddir <- function(db, segs) {
  # Calc new ddirs
  db <- dplyr::mutate(db, orig_ddir = ddir)

  fix <- dplyr::filter(db, orig_ddir != drec) %>%
    dplyr::select(seqno, drec, ddir, row, col) %>%
    dplyr::mutate(down_row = db$row[drec],
                  down_col = db$col[drec],
                  row_diff = down_row - row,
                  col_diff = down_col - col,
                  row_diff = dplyr::case_when(row_diff == -1 ~ 1,
                                              row_diff ==  0 ~ 2,
                                              row_diff ==  1 ~ 3,
                                              TRUE ~ as.numeric(NA)),
                  col_diff = dplyr::case_when(col_diff == -1 ~ 1,
                                              col_diff ==  0 ~ 2,
                                              col_diff ==  1 ~ 3,
                                              TRUE ~ as.numeric(NA))) %>%
    assertr::verify(sum(is.na(row_diff)) == 0) %>%
    assertr::verify(sum(is.na(col_diff)) == 0) %>%
    dplyr::mutate(ddir = (3 - row_diff) * 3,
                  ddir = ddir + col_diff) %>%
    assertr::assert(assertr::in_set(1:9), ddir)

  db$ddir[fix$seqno] <- fix$ddir

  # Add ddirs to segments
  new_ddir <- dplyr::select(db, start_seqno = seqno, ddir)
  segs <- dplyr::left_join(segs, new_ddir, by = "start_seqno") %>%
    dplyr::rename("start_ddir" = "ddir")

  list(db = db, segs = segs)
}
