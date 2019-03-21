# FormMapR: Procedure to compute wetness index as per Quinn et all., 1991

calc_weti <- function(db, grid = 5, verbose = TRUE) {

  l1 <- grid * 0.5    # orthogonal
  l2 <- grid * 0.354  # diagonal  (hypothenus = sqrt(0.5^2 + 0.5^2) / 2)
  l_sqr <- grid * grid  # let's use cell area
  orthogonal <- grid
  diagonal <- grid * sqrt(2)
  qarea <- l_sqr

  max_s <- nrow(db)

  db_n <- db %>%
    dplyr::select(seqno, elev, drec) %>%
    nb_values(max_cols = max(db$col), col = c("elev", "seqno")) %>%
    dplyr::mutate(diag = n %in% c(1, 3, 7, 9),
                  deltax = dplyr::if_else(diag, diagonal, orthogonal),
                  ql = dplyr::if_else(diag, l2, l1),
                  status = dplyr::case_when(elev_n > elev ~ "higher",  # Neighbour is higher
                                            elev_n < elev ~ "lower",   # Neighbour is lower
                                            drec == seqno_n ~ "lower", # Neighbour is equal, but flows into
                                            TRUE ~ "no_flow"),          # No flow between focal and neighbour
                  elev_diff = elev_n - elev,
                  tan_f = (elev - elev_n) / deltax,   # From focal cell perspective
                  tan2_f = tan_f * ql)

  # Only cells which have higher neighbours
  db_n_sub <- db_n %>%
    dplyr::select(seqno, seqno_n, diag, elev_diff, status, tan2_f) %>%
    dplyr::filter(!is.na(elev_diff), status == "higher")

  db_c <- db_n %>%
    dplyr::filter(!is.na(elev_diff)) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(drec = drec[1],
                     in_t = sum(status == "higher", na.rm = TRUE),  # How many higher cells?
                     out_t = sum(status == "lower", na.rm = TRUE),  # How many lower cells?
                     qarea = qarea,
                     cell_t = 0,
                     count_d = 0,
                     count_o = 0,
                     elev_sum = 0,
                     sumtanbl = sum(tan2_f[status == "lower"], na.rm = TRUE))

  i <- 0
  while(any(db_c$in_t >= 0 & db_c$out_t !=0)) {
    i <- i + 1

    db_c <- trace_wetness(db_n_sub, db_c)

    if(verbose) message("Round ", i, " - ", nrow(db_c[db_c$in_t > -1,]), " remaining")
  }

  db_weti <- db_c %>%
    dplyr::mutate(qweti = dplyr::if_else(sumtanbl == 0, qarea / 0.0001, qarea / sumtanbl),
                  qweti = dplyr::if_else(qweti > 1, log(qweti), log(1 + qweti)))

  dplyr::full_join(db_weti, db, by = c("seqno", "drec")) %>%
    dplyr::arrange(seqno)
}

trace_wetness <- function(db_n_sub, db_c) {
  # Get cells with only lower cells
  temp <- db_c[db_c$in_t == 0 & db_c$out_t > 0,] %>%
    dplyr::mutate(qc = .data$qarea / .data$sumtanbl)

  # Get the lower cells that these upper cells flow into, add effects of this link,
  # plus all previous linkages from this upper cell
  temp_n <- db_n_sub[db_n_sub$seqno_n %in% temp$seqno, ]

  temp_n <- temp_n %>%
    dplyr::left_join(temp, by = c("seqno_n" = "seqno")) %>%
    dplyr::mutate(new_qa = qc * -1 * tan2_f) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(qarea = sum(new_qa),
                     this_round = sum(status == "higher"),
                     d = sum(diag) + sum(count_d),
                     o = sum(!diag) + sum(count_o),
                     t = d + o,
                     elev_sum = sum(elev_sum) + sum(elev_diff))

  # Add these cell totals to all the previous totals already calculated for this lower cell
  db_c[db_c$seqno %in% temp_n$seqno, c("cell_t", "count_d", "count_o", "elev_sum", "qarea")] <-
    db_c[db_c$seqno %in% temp_n$seqno, c("cell_t", "count_d", "count_o", "elev_sum", "qarea")]  +
    temp_n[, c("t", "d", "o", "elev_sum", "qarea")]

  # Resolve the linkages (i.e. one less upper cell flowing into these lower cells)
  db_c$in_t[db_c$seqno %in% temp$seqno] <-  db_c$in_t[db_c$seqno %in% temp$seqno] - 1
  db_c$in_t[db_c$seqno %in% temp_n$seqno] <- db_c$in_t[db_c$seqno %in% temp_n$seqno] - temp_n$this_round

  db_c
}
