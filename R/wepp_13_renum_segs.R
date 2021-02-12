#' Renumber segments
#'
#' From original FoxPro code:
#'
#' "Procedure to renumber channel segments once the total number of WEPP
#' hillslopes is known.
#' Now we need to renumber the WEPP channel and impoundment entities so
#' that they begin numbering not at 1 as originally, but at Max_hills + 1
#' where max_hills = curr_hillno
#'
#' @noRd
#'
renum_segs <- function(db, segs) {

  max_hill <- max(db$hill_no, na.rm = TRUE)

  db <- dplyr::mutate(db,
                      dplyr::across(c("segment_no", "shed_no"), ~replace(., . == 0, NA)),
                      dplyr::across(c("segment_no", "shed_no"), ~ . + max_hill))

  segs <- dplyr::mutate(segs,
                        final_id = sort_order + max_hill,
                        dplyr::across(dplyr::matches("_seg|_imp"),
                                      ~dplyr::if_else(. == 0, 0L, as.integer(. + max_hill))))
#
#   # Rename chan_no to final id in db
#   db <- dplyr::left_join(db, unique(dplyr::select(segs, "final_id", "sort_order")),
#                          by = c("chan_no" = "sort_order")) %>%
#     dplyr::select(-"chan_no") %>%
#     dplyr::rename(chan_no = "final_id")


  list(db = db, segs = segs)
}
