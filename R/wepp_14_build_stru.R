#' Build WEPP structure file
#'
#' From original FoxPro code:
#'
#' "Procedure to create and fill in the structure file required for WEPP"
#'
#' @noRd
#'
#'
build_stru <- function(db, segs) {

  struct <- db %>%
    dplyr::arrange(.data$shed_no, .data$shed_side) %>%
    dplyr::select("hill_no", "shed_side", "final_id" = "shed_no") %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(.data$shed_side), !is.na(.data$hill_no)) %>%
    dplyr::mutate(hill_type = dplyr::case_when(
      .data$shed_side == 1 ~ "top_hill",
      .data$shed_side == 2 ~ "right_hill",
      .data$shed_side == 3 ~ "left_hill")) %>%
    dplyr::select(-"shed_side") %>%
    tidyr::pivot_wider(names_from = "hill_type", values_from = "hill_no")

  segs <- dplyr::left_join(segs, struct, by = "final_id")

  struct <- dplyr::select(segs, "final_id", "impound",
                          dplyr::matches("_seg|_imp")) %>%
    dplyr::left_join(struct, by = "final_id") %>%
    dplyr::rename(element_no = "final_id",
                  left_chan = "left_seg",
                  right_chan = "right_seg",
                  top_chan = "center_seg") %>%
    dplyr::mutate(ele_type = dplyr::if_else(.data$impound == 1, 3, 2),
                  comments = paste("Element No:", .data$element_no,
                                   dplyr::if_else(.data$impound == 1, "impoundment", "channel")),
                  left_chan = replace(.data$left_chan,
                                      .data$left_chan == .data$left_imp,
                                      0),
                  left_imp = replace(.data$left_imp,
                                     .data$left_chan != .data$left_imp,
                                     0),
                  right_chan = replace(.data$right_chan,
                                       .data$right_chan == .data$right_imp,
                                       0),
                  right_imp = replace(.data$right_imp,
                                      .data$right_chan != .data$right_imp,
                                      0),
                  top_chan = replace(.data$top_chan,
                                     .data$top_chan == .data$top_imp,
                                     0),
                  top_imp = replace(.data$top_imp,
                                    .data$top_chan != .data$top_imp,
                                    0)) %>%
    dplyr::select(-"impound", -"down_seg") %>%
    dplyr::select("element_no", "ele_type", "left_hill", "right_hill", "top_hill",
                  "left_chan", "right_chan", "top_chan",
                  "left_imp", "right_imp", "top_imp", "comments") %>%
    dplyr::arrange(.data$element_no, .data$ele_type)

  list(segs = segs, struct = struct)
}
