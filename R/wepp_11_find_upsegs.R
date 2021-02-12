#' Find Upslope segments that drain into each channel segment
#'
#' From original FoxPro code:
#'
#' "Procedure to identify for each channel segment the identity and location
#' of the upslope segment(s) that drain into it.
#' If more than 1 upslope segment drains into a given lower segment then
#' it is necessary to identify which of the upslope segments enters the
#' lower segment from the left, centre or right side.
#' If only 2 segments enter: Then assign them to left and right sides only
#' If 3 segments enter: Then assign each to a left, center or right.
#' If more than 3 segments enter, we have a problem and need to fix it."
#'
#' @noRd
#'
find_upsegs <- function(segs) {

  s_keep <- dplyr::select(segs, "sort_order", "initial_id", "len_cells", "len_meters")

  s <- segs %>%
    dplyr::select("start_seqno", "start_row", "start_col", "start_type", "start_elev",
                  "end_seqno", "end_row", "end_col", "end_type", "end_elev",
                  "impound", "drain_seqno",
                  "start_ddir", "sort_order", "down_seg") %>%
    dplyr::arrange(.data$down_seg, .data$sort_order) %>%
    dplyr::filter(.data$down_seg != 0) %>%
    dplyr::group_by(.data$down_seg) %>%
    dplyr::mutate(not_equal = .data$down_seg != .data$sort_order,
                  num_down = sum(.data$not_equal),
                  upside = .data$sort_order,
                  upside = replace(.data$upside, !.data$not_equal, as.numeric(NA))) %>%
    dplyr::ungroup() %>%
    dplyr::select("down_seg", "sort_order", dplyr::everything())

  # Deal with only 1 segment

  if(any(s$num_down == 1)) {
    s_index1 <- s %>%
      dplyr::filter(.data$num_down == 1) %>%
      dplyr::group_by(.data$down_seg) %>%
      dplyr::summarize(down_seg = down_seg[1],
                       center_seg = sort_order[1], # (upnum)
                       center_seqno = end_seqno[1],       # (uprec)
                       center_imp = dplyr::if_else(.data$impound[1],
                                                   .data$down_seg,
                                                   0L))
  } else s_index1 <- data.frame()

  if(any(s$num_down > 1)) {
    # Deal with multiple segments
    sindex2 <- s %>%
      dplyr::select(down_seg, sort_order, end_type, end_row, end_col, num_down) %>%
      dplyr::filter(.data$num_down > 1) %>%
      dplyr::group_by(down_seg) %>%
      dplyr::mutate(n = 1:dplyr::n(),
                    segs = list(s),
                    start_ddir = purrr::map2(down_seg, segs,
                                             ~.y[.y$sort_order == .x,
                                                 c("start_ddir", "start_row", "start_col")])) %>%
      dplyr::select(-segs) %>%
      tidyr::unnest(start_ddir) %>%
      dplyr::mutate(cr = end_row - start_row + 2,
                    cc = end_col - start_col + 2,
                    search_win = dplyr::case_when(end_type == 5 & n == 1 ~ 7, # (1,1)
                                                  end_type == 5 & n == 2 ~ 9, # (1,3)
                                                  end_type == 5 & n == 3 ~ 3, # (3,3)
                                                  end_type == 5 ~ 0,
                                                  start_ddir %in% 1:9 ~ start_ddir,
                                                  TRUE ~ 0)) %>%
      dplyr::filter(search_win != 0)

    ups <- sindex2 %>%
      dplyr::select(down_seg, sort_order, search_win, start_ddir) %>%
      tidyr::nest(data = c(search_win, sort_order)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ups = purrr::map2(data, start_ddir, get_order)) %>%
      tidyr::unnest(ups) %>%
      dplyr::group_by(down_seg) %>%
      dplyr::mutate(ups = dplyr::if_else(ups == "center" & !any(ups == "left"), "left", ups)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-down_seg) %>%
      dplyr::mutate(side_seqno = purrr::map(
                      sort_order,
                      ~s[s$sort_order == ., c("down_seg", "end_seqno", "impound")])) %>%
      tidyr::unnest(side_seqno) %>%
      dplyr::mutate(impound = dplyr::if_else(impound, sort_order, 0L))

    ups <- ups %>%
      dplyr::select(side_seg = sort_order, side_seqno = end_seqno, side_imp = impound, ups, down_seg) %>%
      tidyr::pivot_longer(cols = c("side_seg", "side_seqno", "side_imp")) %>%
      dplyr::mutate(name = stringr::str_replace(name, "side", ups)) %>%
      dplyr::select(-ups) %>%
      tidyr::pivot_wider(names_from = "name", values_from = "value")
  } else ups <- dplyr::tibble(left_seg = 0, right_seg = 0, center_seg = 0,
                              left_seqno = 0, right_seqno = 0, center_seqno = 0,
                              left_imp = 0, right_imp = 0, center_imp = 0,
                              .rows = 0)

  ups <- dplyr::bind_rows(s_index1, ups) %>%
    dplyr::rename(top_imp = "center_imp")

  dplyr::left_join(s, ups, by = c("sort_order" = "down_seg")) %>%
    dplyr::mutate(dplyr::across(.fns = ~tidyr::replace_na(., 0))) %>%
    dplyr::left_join(s_keep, by = "sort_order")

}


get_order <- function(data, start_ddir) {
  search_order <- rep(c(1, 4, 7, 8, 9, 6, 3, 2), 2)
  search_order <- list(`1` = search_order[2:9],
                       `2` = search_order[1:8],
                       `3` = search_order[8:15],
                       `4` = search_order[3:10],
                       `5` = search_order[1:8],
                       `6` = search_order[7:14],
                       `7` = search_order[4:11],
                       `8` = search_order[5:12],
                       `9` = search_order[6:13])

  o <- search_order[[start_ddir]]
  data$search_win <- factor(data$search_win, levels = o)
  data <- data[order(data$search_win), ]
  data$ups <- c("right", "center", "left")[1:nrow(data)]
  data$search_win <- as.numeric(as.character(data$search_win))
  data
}
