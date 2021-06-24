# FormMapR: Procedure to compute wetness index as per Quinn et all., 1991

calc_weti <- function(db, grid, verbose) {

  l1 <- grid * 0.5    # orthogonal
  l2 <- grid * 0.354  # diagonal  (hypothenus = sqrt(0.5^2 + 0.5^2) / 2)
  l_sqr <- grid * grid  # let's use cell area
  orthogonal <- grid
  diagonal <- grid * sqrt(2)
  qarea1 <- 1
  qarea2 <- l_sqr

  max_s <- nrow(db)

  if(verbose) message("  Setting up cell flow")

  db_n <- db %>%
    dplyr::arrange(dplyr::desc(elev), upslope, seqno) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::select("seqno", "elev", "drec", "order") %>%
    dplyr::arrange(seqno) %>%
    nb_values(max_cols = max(db$col), col = c("elev", "seqno", "order"),
              format = "wide") %>%
    tidyr::nest(
      n1 = c("seqno", "elev", "drec", "order", tidyselect::contains("n1")),
      n2 = c("seqno", "elev", "drec", "order", tidyselect::contains("n2")),
      n3 = c("seqno", "elev", "drec", "order", tidyselect::contains("n3")),
      n4 = c("seqno", "elev", "drec", "order", tidyselect::contains("n4")),
      n5 = c("seqno", "elev", "drec", "order", tidyselect::contains("n5")),
      n6 = c("seqno", "elev", "drec", "order", tidyselect::contains("n6")),
      n7 = c("seqno", "elev", "drec", "order", tidyselect::contains("n7")),
      n8 = c("seqno", "elev", "drec", "order", tidyselect::contains("n8")),
      n9 = c("seqno", "elev", "drec", "order", tidyselect::contains("n9"))) %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::mutate(
      n = stringr::str_remove(name, "n"),
      value = purrr::map(value,
                         ~dplyr::rename_all(., ~stringr::str_remove(., "[0-9]{1}"))),
      value = purrr::map2(
        value, n,
        ~dplyr::mutate(.x,
                       n = .y,
                       diag = n %in% c(1, 3, 7, 9),
                       deltax = dplyr::if_else(.data$diag, diagonal, orthogonal),
                       ql = dplyr::if_else(.data$diag, l2, l1))),
      value = purrr::map(
        value,
        ~dplyr::mutate(.,
                       status = dplyr::case_when(
                         elev_n > elev ~ "higher",  # Neighbour higher
                         elev_n < elev ~ "lower",   # Neighbour lower
                         TRUE ~ "no_flow"),         # Equal, no flow
                       status_drec = drec == seqno_n & drec != seqno, # Equal, but flows
                       elev_diff = elev_n - elev,
                       tan_f = (elev - elev_n) / deltax,   # From focal cell perspective
                       tan2_f = tan_f * ql))) %>%
    dplyr::select(value) %>%
    tidyr::unnest(cols = "value")

  if(verbose) message("  Addressing special flow cases")

  # Which cells flow to drec (not by elev)
  db_drec <- db_n %>%
    dplyr::group_by(.data$seqno) %>%
    dplyr::filter(all(.data$status != "lower") & .data$status_drec) %>%
    dplyr::ungroup()

  # Which should be evaluated BEFORE their drain points?
  db_first <- dplyr::filter(db_drec, .data$order < .data$order_n)

  db_n$status[db_n$seqno %in% db_first$seqno & db_n$status_drec] <- "lower_drec"
  db_n$match <- paste0(db_n$seqno, "_", db_n$seqno_n)
  db_first$match <- paste0(db_first$drec, "_", db_first$seqno)
  db_n$status[db_n$match %in% db_first$match] <- "higher_drec"

  db_after <- db_drec$seqno[!db_drec$seqno %in% db_first$seqno]

  # Only cells which have higher neighbours
  db_n_sub <- db_n %>%
    dplyr::select("seqno", "seqno_n", "order", "order_n", "diag",
                  "elev_diff", "status", "tan2_f") %>%
    dplyr::filter(!is.na(.data$elev_diff),
                  .data$status %in% c("higher", "higher_drec"))

  db_c <- db_n %>%
    dplyr::filter(!is.na(.data$elev_diff)) %>%
    dplyr::group_by(.data$seqno) %>%
    dplyr::summarize(
      drec = .data$drec[1],
      in_t = sum(.data$status %in% c("higher", "higher_drec"),
                 na.rm = TRUE),  # How many higher cells?
      out_t = sum(.data$status %in% c("lower", "lower_drec"),
                  na.rm = TRUE),  # How many lower cells?
      qarea1 = !!qarea1, # starting area
      qarea2 = !!qarea2, # starting area
      cell_t = 0,
      count_d = 0,
      count_o = 0,
      elev_sum = 0,
      sumtanbl = sum(.data$tan2_f[.data$status == "lower"], na.rm = TRUE),
      qc1 = NA,
      qc2 = NA)

  if(verbose) message("  Trace wetness")
  while(any(db_c$in_t >= 0 & db_c$out_t !=0)) {
    db_c <- trace_wetness(db_n_sub, db_c)
  }

  if(verbose) message("  Fix special cases")

  # For cells which have no lower cells and were not already evaluated
  # (i.e. sumtanbl == 0) push to drec
  db_flat <- db_c %>%
    dplyr::filter(.data$seqno %in% !!db_after) %>%
    dplyr::select("drec", "qarea_flat1" = "qarea1", "qarea_flat2" = "qarea2")

  db_c_temp <- dplyr::inner_join(
    dplyr::select(db_c, "seqno", "qarea1", "qarea2"),
    db_flat,
    by = c("seqno" = "drec")) %>%
    dplyr::group_by(.data$seqno, .data$qarea1, .data$qarea2) %>%
    dplyr::summarize(qarea_flat1 = sum(.data$qarea_flat1),
                     qarea_flat2 = sum(.data$qarea_flat2)) %>%
    dplyr::mutate(qarea1 = .data$qarea1 + .data$qarea_flat1,
                  qarea2 = .data$qarea2 + .data$qarea_flat2) %>%
    dplyr::select(-"qarea_flat1", -"qarea_flat2")

  db_c$qarea1[db_c$seqno %in% db_c_temp$seqno] <- db_c_temp$qarea1
  db_c$qarea2[db_c$seqno %in% db_c_temp$seqno] <- db_c_temp$qarea2

  db_c$qc1[is.na(db_c$qc1)] <- db_c$qarea1[is.na(db_c$qc1)]/0.0001
  db_c$qc2[is.na(db_c$qc2)] <- db_c$qarea2[is.na(db_c$qc2)]/0.0001

  # Shouldn't this be all +1?
  db_weti <- db_c %>%
    dplyr::mutate(
      qweti1 = dplyr::if_else(.data$qc1 > 1, log(.data$qc1), log(1 + .data$qc1)),
      qweti2 = dplyr::if_else(.data$qc2 > 1, log(.data$qc2), log(1 + .data$qc2)),
      qarea1 = round(.data$qarea1, 2),
      qarea2 = round(.data$qarea2, 2),
      qweti1 = round(.data$qweti1, 2),
      qweti2 = round(.data$qweti2, 2))

  dplyr::full_join(db_weti, db, by = c("seqno", "drec")) %>%
    dplyr::arrange(seqno)
}


trace_wetness <- function(db_n_sub, db_c) {
  # Get cells with only lower cells
  temp <- db_c[db_c$in_t == 0 & db_c$out_t > 0,]

  # Get the lower cells that these upper cells flow into, add effects of this link,
  # plus all previous linkages from this upper cell
  temp_n <- db_n_sub[db_n_sub$seqno_n %in% temp$seqno, ]


  temp_n <- temp_n %>%
    dplyr::left_join(temp, by = c("seqno_n" = "seqno")) %>%
    dplyr::mutate(qc1 = dplyr::if_else(.data$status == "higher",
                                      .data$qarea1 / .data$sumtanbl,
                                      .data$qarea1 / 0.0001),
                  qc2 = dplyr::if_else(.data$status == "higher",
                                       .data$qarea2 / .data$sumtanbl,
                                       .data$qarea2 / 0.0001),
                  new_qa1 = dplyr::if_else(status == "higher",
                                          # -1 because calc from lower cell's perspective
                                          .data$qc1 * -1 * .data$tan2_f,
                                          .data$qarea1),
                  new_qa2 = dplyr::if_else(status == "higher",
                                           # -1 because calc from lower cell's perspective
                                           .data$qc2 * -1 * .data$tan2_f,
                                           .data$qarea2),
                  elev_diff = replace(elev_diff, status != "higher", 0),
                  cell_t = replace(cell_t, status != "higher", 0))

  temp_qc <- temp_n %>%
    dplyr::group_by(seqno_n) %>%
    dplyr::arrange(order_n) %>%
    dplyr::summarize(qc1 = dplyr::last(qc1),
                     qc2 = dplyr::last(qc2)) %>% # get last qc for each higher cell
    dplyr::arrange(seqno_n)

  temp_n <- temp_n %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(qarea1 = sum(new_qa1),
                     qarea2 = sum(new_qa2),
                     this_round = sum(status %in% c("higher", "higher_drec")),
                     d = sum(diag) + sum(count_d),
                     o = sum(!diag) + sum(count_o),
                     t = d + o,
                     elev_sum = sum(elev_diff) + sum(elev_sum))

  temp_seqno <- temp$seqno
  temp_n_seqno <- temp_n$seqno
  temp_n_t <- temp_n$t
  temp_n_d <- temp_n$d
  temp_n_o <- temp_n$o
  temp_n_elev_sum <- temp_n$elev_sum
  temp_n_qarea1 <- temp_n$qarea1
  temp_n_qarea2 <- temp_n$qarea2
  temp_n_this_round <- temp_n$this_round

  temp_qc_seqno1 <- temp_qc$seqno_n
  temp_qc_seqno2 <- temp_qc$seqno_n
  temp_qc_qc1 <- temp_qc$qc1
  temp_qc_qc2 <- temp_qc$qc2

  # Add these cell totals to all the previous totals already calculated for this lower cell
  seqno <- db_c$seqno
  cell_t <- db_c$cell_t
  count_d <- db_c$count_d
  count_o <- db_c$count_o
  elev_sum <- db_c$elev_sum
  qarea1 <- db_c$qarea1
  qarea2 <- db_c$qarea2
  qc1 <- db_c$qc1
  qc2 <- db_c$qc2

  in_t <- db_c$in_t

  i <- seqno %in% temp_n_seqno

  cell_t[i] <- cell_t[i] + temp_n_t
  count_d[i] <- count_d[i] + temp_n_d
  count_o[i] <- count_o[i] + temp_n_o
  elev_sum[i] <- elev_sum[i] + temp_n_elev_sum
  qarea1[i] <- qarea1[i] + temp_n_qarea1
  qarea2[i] <- qarea2[i] + temp_n_qarea2

  qc1[seqno %in% temp_qc_seqno1] <- temp_qc_qc1
  qc2[seqno %in% temp_qc_seqno2] <- temp_qc_qc2

  # Resolve the linkages (i.e. one less upper cell flowing into these lower cells)
  in_t[seqno %in% temp_seqno] <- in_t[seqno %in% temp_seqno] - 1
  in_t[i] <- in_t[i] - temp_n_this_round

  data.frame(seqno, drec = db_c$drec, cell_t,
             count_d, count_o, elev_sum,
             qarea1 = qarea1, qarea2 = qarea2,
             qc1 = qc1, qc2 = qc2,
             in_t, out_t = db_c$out_t,
             sumtanbl = db_c$sumtanbl)
}

