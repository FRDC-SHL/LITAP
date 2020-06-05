# FormMapR: Procedure to compute wetness index as per Quinn et all., 1991

calc_weti <- function(db, grid = 5, verbose = FALSE) {

  l1 <- grid * 0.5    # orthogonal
  l2 <- grid * 0.354  # diagonal  (hypothenus = sqrt(0.5^2 + 0.5^2) / 2)
  l_sqr <- grid * grid  # let's use cell area
  orthogonal <- grid
  diagonal <- grid * sqrt(2)
  qarea <- l_sqr

  max_s <- nrow(db)

  db_n <- db %>%
    dplyr::arrange(dplyr::desc(elev), upslope, seqno) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::select(seqno, elev, drec, order) %>%
    dplyr::arrange(seqno) %>%
    nb_values(max_cols = max(db$col), col = c("elev", "seqno", "order")) %>%
    dplyr::mutate(diag = n %in% c(1, 3, 7, 9),
                  deltax = dplyr::if_else(diag, diagonal, orthogonal),
                  ql = dplyr::if_else(diag, l2, l1),
                  status = dplyr::case_when(elev_n > elev ~ "higher",  # Neighbour higher
                                            elev_n < elev ~ "lower",   # Neighbour lower
                                            TRUE ~ "no_flow"),         # Equal, no flow
                  status_drec = drec == seqno_n & drec != seqno, # Equal, but flows
                  elev_diff = elev_n - elev,
                  tan_f = (elev - elev_n) / deltax,   # From focal cell perspective
                  tan2_f = tan_f * ql)

  # Which cells flow to drec (not by elev)
  db_drec <- db_n %>%
    dplyr::group_by(seqno) %>%
    dplyr::filter(all(status != "lower") & status_drec) %>%
    dplyr::ungroup()

  # Which should be evaluated BEFORE their drain points?
  db_first <- dplyr::filter(db_drec, order < order_n)

  db_n$status[db_n$seqno %in% db_first$seqno & db_n$status_drec] <- "lower_drec"
  for(i in 1:nrow(db_first)) {
    db_n$status[db_n$seqno == db_first$drec[i] &
                  db_n$seqno_n == db_first$seqno[i]] <- "higher_drec"
  }

  db_after <- db_drec$seqno[!db_drec$seqno %in% db_first$seqno]

  # Only cells which have higher neighbours
  db_n_sub <- db_n %>%
    dplyr::select(seqno, seqno_n, order, order_n, diag, elev_diff, status, tan2_f) %>%
    dplyr::filter(!is.na(elev_diff), stringr::str_detect(status, "higher"))

  db_c <- db_n %>%
    dplyr::filter(!is.na(elev_diff)) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(drec = drec[1],
                     in_t = sum(stringr::str_detect(status, "higher"),
                                na.rm = TRUE),  # How many higher cells?
                     out_t = sum(stringr::str_detect(status, "lower"),
                                 na.rm = TRUE),  # How many lower cells?
                     qarea = qarea,
                     cell_t = 0,
                     count_d = 0,
                     count_o = 0,
                     elev_sum = 0,
                     sumtanbl = sum(tan2_f[status == "lower"], na.rm = TRUE),
                     qc = NA)

  while(any(db_c$in_t >= 0 & db_c$out_t !=0)) {
    db_c <- trace_wetness(db_n_sub, db_c)
  }

  # For cells which have no lower cells and were not already evaluated
  # (i.e. sumtanbl == 0) push to drec
  db_flat <- db_c %>%
    dplyr::filter(seqno %in% db_after) %>%
    dplyr::select(drec, qarea_flat = qarea)

  db_c_temp <- dplyr::inner_join(dplyr::select(db_c, seqno, qarea),
                                 db_flat, by = c("seqno" = "drec")) %>%
    dplyr::group_by(seqno, qarea) %>%
    dplyr::summarize(qarea_flat = sum(qarea_flat)) %>%
    dplyr::mutate(qarea = qarea + qarea_flat) %>%
    dplyr::select(-qarea_flat)

  db_c$qarea[db_c$seqno %in% db_c_temp$seqno] <- db_c_temp$qarea

  db_c$qc[is.na(db_c$qc)] <- db_c$qarea[is.na(db_c$qc)]/0.0001

  # Shouldn't this be all +1?
  db_weti <- db_c %>%
    dplyr::mutate(qweti = dplyr::if_else(qc > 1, log(qc), log(1 + qc)),
                  qweti = round(qweti, 3))

  dplyr::full_join(db_weti, db, by = c("seqno", "drec")) %>%
    dplyr::arrange(seqno)
}

calc_weti2 <- function(db, grid = 5, verbose = FALSE) {

  l1 <- grid * 0.5    # orthogonal
  l2 <- grid * 0.354  # diagonal  (hypothenus = sqrt(0.5^2 + 0.5^2) / 2)
  l_sqr <- grid * grid  # let's use cell area
  orthogonal <- grid
  diagonal <- grid * sqrt(2)
  qarea <- l_sqr

  max_s <- nrow(db)

  if(verbose) message("  Setting up cell flow")

  db_n <- db %>%
    dplyr::arrange(dplyr::desc(elev), upslope, seqno) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::select("seqno", "elev", "drec", "order") %>%
    dplyr::arrange(seqno) %>%
    nb_values(max_cols = max(db$col), col = c("elev", "seqno", "order"), format = "wide") %>%
    tidyr::nest(n1 = c("seqno", "elev", "drec", "order", tidyselect::contains("n1")),
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
      value = purrr::map(value, ~dplyr::rename_all(., ~stringr::str_remove(., "[0-9]{1}"))),
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
                       status = dplyr::case_when(elev_n > elev ~ "higher",  # Neighbour higher
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
    dplyr::group_by(seqno) %>%
    dplyr::filter(all(status != "lower") & status_drec) %>%
    dplyr::ungroup()

  # Which should be evaluated BEFORE their drain points?
  db_first <- dplyr::filter(db_drec, order < order_n)

  db_n$status[db_n$seqno %in% db_first$seqno & db_n$status_drec] <- "lower_drec"
  for(i in 1:nrow(db_first)) {
    db_n$status[db_n$seqno == db_first$drec[i] &
                  db_n$seqno_n == db_first$seqno[i]] <- "higher_drec"
  }


  db_after <- db_drec$seqno[!db_drec$seqno %in% db_first$seqno]

  # Only cells which have higher neighbours
  db_n_sub <- db_n %>%
    dplyr::select(seqno, seqno_n, order, order_n, diag, elev_diff, status, tan2_f) %>%
    dplyr::filter(!is.na(elev_diff), status %in% c("higher", "higher_drec"))

  db_c <- db_n %>%
    dplyr::filter(!is.na(elev_diff)) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(drec = drec[1],
                     in_t = sum(status %in% c("higher", "higher_drec"),
                                na.rm = TRUE),  # How many higher cells?
                     out_t = sum(status %in% c("lower", "lower_drec"),
                                 na.rm = TRUE),  # How many lower cells?
                     qarea = qarea,
                     cell_t = 0,
                     count_d = 0,
                     count_o = 0,
                     elev_sum = 0,
                     sumtanbl = sum(tan2_f[status == "lower"], na.rm = TRUE),
                     qc = NA)

  if(verbose) message("  Trace wetness")
  while(any(db_c$in_t >= 0 & db_c$out_t !=0)) {
    db_c <- trace_wetness2(db_n_sub, db_c)
  }

  if(verbose) message("  Fix special cases")
  # For cells which have no lower cells and were not already evaluated
  # (i.e. sumtanbl == 0) push to drec
  db_flat <- db_c %>%
    dplyr::filter(seqno %in% db_after) %>%
    dplyr::select(drec, qarea_flat = qarea)

  db_c_temp <- dplyr::inner_join(dplyr::select(db_c, seqno, qarea),
                                 db_flat, by = c("seqno" = "drec")) %>%
    dplyr::group_by(seqno, qarea) %>%
    dplyr::summarize(qarea_flat = sum(qarea_flat)) %>%
    dplyr::mutate(qarea = qarea + qarea_flat) %>%
    dplyr::select(-qarea_flat)

  db_c$qarea[db_c$seqno %in% db_c_temp$seqno] <- db_c_temp$qarea

  db_c$qc[is.na(db_c$qc)] <- db_c$qarea[is.na(db_c$qc)]/0.0001

  # Shouldn't this be all +1?
  db_weti <- db_c %>%
    dplyr::mutate(qweti = dplyr::if_else(qc > 1, log(qc), log(1 + qc)),
                  qarea = round(qarea, 2),
                  qweti = round(qweti, 2))

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
    dplyr::mutate(qc = dplyr::if_else(.data$status == "higher",
                                      .data$qarea / .data$sumtanbl,
                                      .data$qarea / 0.0001),
                  new_qa = dplyr::if_else(status == "higher",
                                          .data$qc * -1 * .data$tan2_f,     # -1 because calc from lower cell's perspective
                                          .data$qarea),
                  elev_diff = replace(elev_diff, status != "higher", 0),
                  cell_t = replace(cell_t, status != "higher", 0))

  temp_qc <- temp_n %>%
    dplyr::group_by(seqno_n) %>%
    dplyr::arrange(order_n) %>%
    dplyr::summarize(qc = dplyr::last(qc)) %>% # get last qc for each higher cell
    dplyr::arrange(seqno_n)

  temp_n <- temp_n %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(qarea = sum(new_qa),
                     this_round = sum(status %in% c("higher", "higher_drec")),
                     d = sum(diag) + sum(count_d),
                     o = sum(!diag) + sum(count_o),
                     t = d + o,
                     elev_sum = sum(elev_diff) + sum(elev_sum))

  # Add these cell totals to all the previous totals already calculated for this lower cell
  db_c[db_c$seqno %in% temp_n$seqno, c("cell_t", "count_d", "count_o", "elev_sum", "qarea")] <-
    db_c[db_c$seqno %in% temp_n$seqno, c("cell_t", "count_d", "count_o", "elev_sum", "qarea")]  +
    temp_n[, c("t", "d", "o", "elev_sum", "qarea")]

  db_c$qc[db_c$seqno %in% temp_qc$seqno_n] <- temp_qc$qc

  # Resolve the linkages (i.e. one less upper cell flowing into these lower cells)
  db_c$in_t[db_c$seqno %in% temp$seqno] <-  db_c$in_t[db_c$seqno %in% temp$seqno] - 1
  db_c$in_t[db_c$seqno %in% temp_n$seqno] <- db_c$in_t[db_c$seqno %in% temp_n$seqno] - temp_n$this_round

  db_c
}

trace_wetness2 <- function(db_n_sub, db_c) {
  # Get cells with only lower cells
  temp <- db_c[db_c$in_t == 0 & db_c$out_t > 0,]

  # Get the lower cells that these upper cells flow into, add effects of this link,
  # plus all previous linkages from this upper cell
  temp_n <- db_n_sub[db_n_sub$seqno_n %in% temp$seqno, ]


  temp_n <- temp_n %>%
    dplyr::left_join(temp, by = c("seqno_n" = "seqno")) %>%
    dplyr::mutate(qc = dplyr::if_else(.data$status == "higher",
                                      .data$qarea / .data$sumtanbl,
                                      .data$qarea / 0.0001),
                  new_qa = dplyr::if_else(status == "higher",
                                          .data$qc * -1 * .data$tan2_f,     # -1 because calc from lower cell's perspective
                                          .data$qarea),
                  elev_diff = replace(elev_diff, status != "higher", 0),
                  cell_t = replace(cell_t, status != "higher", 0))

  temp_qc <- temp_n %>%
    dplyr::group_by(seqno_n) %>%
    dplyr::arrange(order_n) %>%
    dplyr::summarize(qc = dplyr::last(qc)) %>% # get last qc for each higher cell
    dplyr::arrange(seqno_n)

  temp_n <- temp_n %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(qarea = sum(new_qa),
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
  temp_n_qarea <- temp_n$qarea
  temp_n_this_round <- temp_n$this_round

  temp_qc_seqno <- temp_qc$seqno_n
  temp_qc_qc <- temp_qc$qc

  # Add these cell totals to all the previous totals already calculated for this lower cell
  seqno <- db_c$seqno
  cell_t <- db_c$cell_t
  count_d <- db_c$count_d
  count_o <- db_c$count_o
  elev_sum <- db_c$elev_sum
  qarea <- db_c$qarea
  qc <- db_c$qc

  in_t <- db_c$in_t

  i <- seqno %in% temp_n_seqno

  cell_t[i] <- cell_t[i] + temp_n_t
  count_d[i] <- count_d[i] + temp_n_d
  count_o[i] <- count_o[i] + temp_n_o
  elev_sum[i] <- elev_sum[i] + temp_n_elev_sum
  qarea[i] <- qarea[i] + temp_n_qarea

  qc[seqno %in% temp_qc_seqno] <- temp_qc_qc

  # Resolve the linkages (i.e. one less upper cell flowing into these lower cells)
  in_t[seqno %in% temp_seqno] <- in_t[seqno %in% temp_seqno] - 1
  in_t[i] <- in_t[i] - temp_n_this_round

  if(146 %in% temp_n$seqno) browser()

  data.frame(seqno, drec = db_c$drec, cell_t,
             count_d, count_o, elev_sum,
             qarea = qarea,
             qc = qc, in_t, out_t = db_c$out_t,
             sumtanbl = db_c$sumtanbl)
}

