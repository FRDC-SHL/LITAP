#' Compute slope and aspect for WEPP
#'
#' From original FoxPro code:
#'
#' "Procedure to process the ID#WEPP file after all channel segments and
#'  their morphometric attributes have been computed to create and store
#'  statistical descriptions of notional channel profiles as required
#'  for input into the WEPP model."
#'
#' @noRd
#'

chan_stats <- function(db, segs, grid) {

  chan <- segs %>%
    dplyr::select(chan_no = "final_id", "start_seqno", "start_row", "start_col",
                  "start_elev", "start_ddir", "end_seqno", "end_row", "end_col", "end_elev",
                  "len_cells") %>%
    dplyr::mutate(x_diff = abs(.data$start_col - .data$end_col) * grid,
                  y_diff = abs(.data$start_row - .data$end_row) * grid,
                  start2endl = sqrt(.data$x_diff^2 + .data$y_diff^2),
                  # If len_cells > 1:
                  gen_slope = (.data$start_elev - .data$end_elev) / (.data$len_cells * grid),
                  delta_n = dplyr::if_else(.data$len_cells < 21, 1, .data$len_cells/18))

  chan1 <- up_cells(chan, db) %>%
    dplyr::select(chan_no, num_points, mean_slope, gen_slope, aspect,
                  profile = prof_string)

  chan2 <- dplyr::filter(chan, .data$len_cells > 1) %>%
    tidyr::nest(data = c(start_seqno, end_seqno, delta_n, len_cells)) %>%
    dplyr::select(chan_no, gen_slope, data) %>%
    dplyr::mutate(data = purrr::map(data, ~get_slope(db, .))) %>%
    tidyr::unnest(data) %>%
    dplyr::select(chan_no, num_points, mean_slope, gen_slope, aspect = circ_aspect,
                  profile = prof_string)

  dplyr::bind_rows(chan1, chan2) %>%
    dplyr::mutate(chan_len = dplyr::if_else(num_points == 2, grid,
                                            .data$num_points * grid))
}

up_cells <- function(chan, db) {
  chan1 <- dplyr::filter(chan, len_cells == 1)

  up_seqs <- chan1 %>%
    dplyr::select("start_seqno") %>%
    dplyr::distinct()

  up_seqs <- db[up_seqs$start_seqno, ] %>%
    dplyr::select("seqno", "drec", "elev","slope_pct", "aspect") %>%
    nb_values(db = db, col = c("drec", "elev"), format = "wide",
              max_cols = max(db$col), db_sub = .) %>%
    dplyr::mutate(drec_n1 = drec_n1 == drec, drec_n2 = drec_n2 == drec,
                  drec_n3 = drec_n3 == drec, drec_n4 = drec_n4 == drec,
                  drec_n6 = drec_n6 == drec, drec_n7 = drec_n7 == drec,
                  drec_n8 = drec_n8 == drec, drec_n9 = drec_n9 == drec,
                  elev_n1 = elev_n1 > elev, elev_n2 = elev_n2 > elev,
                  elev_n3 = elev_n3 > elev, elev_n4 = elev_n4 > elev,
                  elev_n6 = elev_n6 > elev, elev_n7 = elev_n7 > elev,
                  elev_n8 = elev_n8 > elev, elev_n9 = elev_n9 > elev) %>%
    dplyr::mutate(up_cell1 = drec_n1 & elev_n1,
                  up_cell2 = drec_n2 & elev_n2,
                  up_cell3 = drec_n3 & elev_n3,
                  up_cell4 = drec_n4 & elev_n4,
                  up_cell6 = drec_n6 & elev_n6,
                  up_cell7 = drec_n7 & elev_n7,
                  up_cell8 = drec_n8 & elev_n8,
                  up_cell9 = drec_n9 & elev_n9) %>%
    dplyr::select(-contains("_n"))

  up_seqs <- nb_values(db, max_cols = max(db$col), col = "upslope",
                       db_sub = up_seqs, format = "wide") %>%
    tidyr::pivot_longer(cols = dplyr::matches("[0-9]{1}$"),
                        names_to = c("type", "n"),
                        values_to = "value", names_sep = -1) %>%
    tidyr::pivot_wider(names_from = type, values_from = value) %>%
    dplyr::group_by(seqno) %>%
    dplyr::arrange(seqno, dplyr::desc(up_cell), dplyr::desc(upslope_n)) %>%
    dplyr::slice(1)

  up_seqs[, c("up_slope", "up_aspect", "up_ddir")] <-
    db[up_seqs$seqno, c("slope_pct", "aspect", "ddir")]

  dplyr::select(chan1, "start_seqno", "chan_no") %>%
    dplyr::left_join(up_seqs, by = c("start_seqno" = "seqno")) %>%
    dplyr::mutate(total_slope = .data$slope_pct + .data$up_slope,
                  mean_slope = .data$total_slope / 2 / 100,
                  gen_slope = .data$mean_slope,
                  up_slope = .data$up_slope / 100,
                  circ_aspect = .data$aspect,
                  prof_string = glue::glue("0.0,{up_slope} 1.0{slope_pct / 100}"),
                  num_points = 2)
}

get_slope <- function(db, chan) {

  t <- trace_flow_fast(chan$start_seqno, db$drec)
  t <- t[1:(which(t == chan$end_seqno))]

  n <- seq_len(chan$len_cells)
  sample_no <- ceiling(c(0.5, cumsum(rep(chan$delta_n, chan$len_cells))))

  n <- n[n %in% sample_no]

  rel_dist <- (n - 1) / (chan$len_cells - 1) # 0 to 1
  str <- glue::glue("{round(rel_dist, 3)}, {round(db$slope_pct[t[n]]/100, 3)}") %>%
    glue::glue_collapse(sep = " ")

  db[t, ] %>%
    dplyr::summarize(total_slope = sum(.data$slope_pct),
                     num_points = length(n),
                     mean_slope = .data$total_slope / .data$num_points / 100,
                     prof_string = str,
                     x_vector = sum(sin(.data$aspect * pi/180)),
                     y_vector = sum(cos(.data$aspect * pi/180)),
                     xovery = x_vector / y_vector,
                     temp_aspect = atan(xovery) * 180 / pi,
                     circ_aspect = dplyr::case_when(.data$y_vector <= 0 ~ 180 + .data$temp_aspect,
                                                    .data$x_vector >= 0 ~ .data$temp_aspect,
                                                    .data$x_vector <= 0 ~ 360 + .data$temp_aspect,
                                                    TRUE ~ as.numeric(NA)))
}
