
# From Sheet SlpCal, A41:U42
ix_avgs <- function(pnts, pnts_no_edge) {

  # Note that this means that j1 DOESNT get filtered to omit edge points... should it?
  #  - I think this is because we're joining later by j drec -> j1, so we want
  #    bits in the edges...
  j1 <- dplyr::mutate(pnts, seqno2 = .data$seqno)

  j <- dplyr::rename(pnts_no_edge, "seqno1" = "seqno")

  dplyr::bind_cols(
    ix_avg(j, j1, LE = 1),
    ix_avg(j, j1, LE = 2),
    ix_avg(j, j1, LE = 3),
    ix_avg(j, j1, LE = 4),
    ix_avg(j, j1, LE = 5))

}

ix_avg <- function(j, j1, LE) {

  if(LE %in% c(1,2)) {
    j <- dplyr::filter(j, .data$max_facet > .env$LE)
    j1 <-dplyr:: filter(j1, .data$max_facet <= .env$LE)
  } else if(LE == 4) {
    j <- dplyr::filter(j, .data$max_facet < .env$LE)
    j1 <- dplyr::filter(j1, .data$max_facet >= .env$LE)
  } else if(LE == 5) {
    j <- dplyr::filter(j, .data$max_facet < .env$LE)
    j1 <- dplyr::filter(j1, .data$max_facet >= .env$LE,
                        .data$l2str > 0)
  }

  #qryFuzcLE5_Bdrix1_100104
  if(LE %in% c(1,2)) by <- c("inv_drec" = "seqno") else by <- c("drec" = "seqno")

  t1 <- dplyr::left_join(j, j1, by = by, suffix = c("1", "2")) |>
    dplyr::rename("l2cr1" = "l2div1",
                  "l2cr2" = "l2div2") |>
    dplyr::mutate(cr_rowd = .data$cr_row1 - .data$cr_row2,
                  cr_cold = .data$cr_col1 - .data$cr_col2,
                  st_rowd = .data$st_row1 - .data$st_row2,
                  st_cold = .data$st_col1 - .data$st_col2,
                  bdr_z2peak = (.data$z2peak1 + .data$z2peak2)/2,
                  bdr_z2pit  = (.data$z2pit1  + .data$z2pit2)/2,
                  bdr_l2peak = (.data$l2peak1 + .data$l2peak2)/2,
                  bdr_l2pit  = (.data$l2pit1  + .data$l2pit2)/2,
                  bdr_z2cr   = (.data$z2cr1   + .data$z2cr2)/2,
                  bdr_z2str  = (.data$z2st1   + .data$z2st2)/2,
                  bdr_l2cr   = (.data$l2cr1    + .data$l2cr2)/2,
                  bdr_l2str  = (.data$l2str1  + .data$l2str2)/2)

  #qryFuzcLE5_Bdrix1_Pct_100104
  t2 <- t1 |>
    dplyr::mutate(
      cs_z_pct = .data$bdr_z2cr / (.data$bdr_z2cr + .data$bdr_z2str),
      cs_l_pct = .data$bdr_l2cr / (.data$bdr_l2cr + .data$bdr_l2str),
      pp_z_pct = .data$bdr_z2peak / (.data$bdr_z2peak + .data$bdr_z2pit),
      pp_l_pct = .data$bdr_l2peak / (.data$bdr_l2peak + .data$bdr_l2pit)) |>

    dplyr::filter(.data$cr_rowd == 0, .data$cr_cold == 0,
                  .data$st_rowd == 0, .data$st_cold == 0)

  if(LE %in% c(1,2)) {
    t3 <- t2 |>
      dplyr::summarize("ix{LE}_z2peak_avg" := mean(.data$bdr_z2peak),
                       "ix{LE}_l2peak_avg" := mean(.data$bdr_l2peak),
                       "ix{LE}_z2cr_avg" := mean(.data$bdr_z2cr),
                       "ix{LE}_l2cr_avg" := mean(.data$bdr_l2cr),
                       "ix{LE}_cnts" := length(.data$seqno1))
  } else {
    t3 <- t2 |>
      dplyr::summarize("ix{LE}_z2pit_avg" := mean(.data$bdr_z2pit),
                       "ix{LE}_l2pit_avg" := mean(.data$bdr_l2pit),
                       "ix{LE}_z2str_avg" := mean(.data$bdr_z2str),
                       "ix{LE}_l2str_avg" := mean(.data$bdr_l2str),
                       "ix{LE}_cnts" := length(.data$seqno1))

  }

  t3
}

le5_avg <- function(pnts_no_edge) {
  pnts_no_edge |>
    dplyr::filter(.data$max_facet != 0) |>
    dplyr::select("seqno", "max_facet", "z2cr", "l2div", "z2st", "l2str",
                  "slope_pct", "aspect", "prof", "plan", "qarea1", "qweti1") |>
    dplyr::group_by(.data$max_facet) |>
    dplyr::summarize(counts = dplyr::n(), dplyr::across(-"seqno", mean))
}

pnts_count <- function(allpeak, allpit, allcrest, allstream, pnts_no_edge) {
  type_count(allcrest, pnts_no_edge, "crest") |>
    dplyr::left_join(type_count(allstream, pnts_no_edge, "stream"), by = "max_facet") |>
    dplyr::left_join(type_count(allpeak, pnts_no_edge, "peak"), by = "max_facet") |>
    dplyr::left_join(type_count(allpit, pnts_no_edge, "pit"), by = "max_facet") |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("_cnt"), ~tidyr::replace_na(.x, 0)))

}

type_count <- function(allXXX, pnts_no_edge, type) {
  allXXX |>
    dplyr::select("seqno", "max_facet") |>
    dplyr::semi_join(pnts_no_edge, by = "seqno")  |> # Omit edges
    dplyr::group_by(max_facet) |>
    dplyr::summarize("{type}_cnt" := dplyr::n())
}


omit_edges <- function(x, edge_row = NULL, edge_col = NULL, nrow = NULL, ncol = NULL, meta = NULL) {

  if(!is.null(meta)) {
    edge_row <- meta$edge_row
    edge_col <- meta$edge_col
    nrow <- meta$nrows
    ncol <- meta$ncols
  }

  if(is.null(nrow)) nrow <- max(x$rows)
  if(is.null(ncol)) ncol <- max(x$cols)

  dplyr::filter(
    x,
    .data$row > (.env$edge_row), .data$row < (.env$nrow - .env$edge_row),
    .data$col > (.env$edge_col), .data$col < (.env$ncol - .env$edge_col))
}

keep_edges <- function(x, edge_row, edge_col, nrow = NULL, ncol = NULL) {

  if(is.null(nrow)) nrow <- max(x$rows)
  if(is.null(ncol)) ncol <- max(x$cols)

  dplyr::filter(
    x,
    (.data$row <= (.env$edge_row) | .data$row >= (.env$nrow - .env$edge_row)) |
    (.data$col <= (.env$edge_col) | .data$col >= (.env$ncol - .env$edge_col)))
}


# SlpCal - B56:I98
mid_calc <- function(pnts, pnts_no_edge, cnts, avg, seg_cal) {

  ix <- ix_avgs(pnts, pnts_no_edge) # Bdr-Cal

  rel_cr_cnts1 <- (cnts$crest_cnt[1] / sum(cnts$crest_cnt[1:2]))   # H48
  rel_st_cnts5 <- (cnts$stream_cnt[5] / sum(cnts$stream_cnt[4:5])) # H52

  # Cst
  cst <- dplyr::tibble(
    zc2s = ix$ix1_z2cr_avg * rel_cr_cnts1, # C2S - Zc2s
    lc2s = ix$ix1_l2cr_avg * rel_cr_cnts1, # C2S - Lc2s
    zp2p = ix$ix1_z2peak_avg,              # P2P - Zp2p
    lp2p = ix$ix1_l2peak_avg               # P2P - Lp2p
  )

  # Ups
  ups <- dplyr::tibble(
    zc2s = ix$ix2_z2cr_avg - cst$zc2s,   # C2S - Zc2s
    lc2s = ix$ix2_l2cr_avg - cst$lc2s,   # C2S - Lc2s
    zp2p = ix$ix2_z2peak_avg - cst$zp2p, # P2P - Zp2p
    lp2p = ix$ix2_l2peak_avg - cst$lp2p  # P2P - Lp2p
  )

  # Dep
  dep <- dplyr::tibble(
    zc2s = ix$ix5_z2str_avg * rel_st_cnts5,
    lc2s = ix$ix5_l2str_avg * rel_st_cnts5,
    zp2p = ix$ix5_z2pit_avg,
    lp2p = ix$ix5_l2pit_avg)

  # Low
  low <- dplyr::tibble(
    zc2s = ix$ix4_z2str_avg - dep$zc2s,
    lc2s = ix$ix4_l2str_avg - dep$lc2s,
    zp2p = ix$ix4_z2pit_avg - dep$zp2p,
    lp2p = ix$ix4_l2pit_avg - dep$lp2p)

  # Mid
  mid <- dplyr::tibble(
    zc2s = avg$zcr2st - cst$zc2s - ups$zc2s - low$zc2s - dep$zc2s,
    lc2s = avg$lstr2div - cst$lc2s - ups$lc2s - low$lc2s - dep$lc2s,
    zp2p = avg$slope3 - cst$zp2p - ups$zp2p - low$zp2p - dep$zp2p,
    lp2p = avg$slope4 - cst$lp2p - ups$lp2p - low$lp2p - dep$lp2p)

  all <- dplyr::bind_rows(cst, ups, mid, low, dep) |>
    dplyr::bind_cols(dplyr::select(seg_cal, "counts", "prof")) |>
    dplyr::mutate(type = factor(c("cst", "ups", "mid", "low", "dep"),
                                levels = c("cst", "ups", "mid", "low", "dep"))) |>
    dplyr::relocate(type)

  all <- dplyr::mutate(
    all,
    cur = tan(.data$prof * pi / 180),
    cal1 = .data$cur * .data$lc2s/2/100,
    cal2 = cumsum(.data$lc2s),
    cal2 = .data$cal2 + dplyr::lag(.data$cal2, default = 0),
    cal3 = .data$cal2 * .data$cal1 / avg$lstr2div / 2)

  slp_ratio <- avg$zcr2st / avg$lstr2div  # C72
  cal3_sum <- sum(all$cal3) + slp_ratio  # I72

  all <- dplyr::arrange(all, dplyr::desc(type)) |>
    dplyr::mutate(sg = .env$cal3_sum * 100,
                  z = 0,
                  midpntz = .data$lc2s * .data$sg / 2 / 100 + .data$z)

  for(i in 1:5) {
    all <- dplyr::mutate(
      all,
      sg = dplyr::lag(.data$sg - .data$cur * .data$lc2s/2, default = .env$cal3_sum * 100),
      z = dplyr::lag(.data$midpntz, default = 0) +
        dplyr::lag(.data$lc2s, default = 0) / 2 * (.data$sg/100),
      midpntz = .data$lc2s * .data$sg / 2 / 100 + .data$z)
  }

  # Now do seg_h and seg_sg's...

  all <- dplyr::arrange(all, type)

  z <- all$sg[1] - all$cur[1] * all$lc2s[1]/2   # C74
  z <- all$midpntz[1] + all$lc2s[1]/2 * (z/100) # C76

  all <- dplyr::mutate(
    all,
    segh = dplyr::lag(.data$z, default = .env$z) - .data$z,
    segsg = .data$segh/.data$lc2s * 100)

  # Back to C2S/P2P PL and PZ...

  all <- dplyr::mutate(
    all,
    pa = .data$counts / sum(.data$counts),
    pz_c2s = .data$segh / .env$z,
    pl_c2s = .data$lc2s / avg$lstr2div,
    pz_p2p = .data$zp2p / avg$slope3,
    pl_p2p = .data$lp2p / avg$slope4)

  all <- dplyr::mutate(
    all,
    dplyr::across(
      .cols = c("l" = "pl_c2s", "a" = "pa"),
      ~ dplyr::case_when(
        type == "ups" ~ sum(.x[type %in% c("cst", "ups")]),
        type == "mid" ~ .x,
        type == "low" ~ sum(.x[type %in% c("low", "dep")])),
      .names = "{.col}_f"))

  k <- 2/sqrt(10 - 4 * sqrt(2))  # D100

  all[all$type == "mid", c("l_f", "a_f")] <- NA_real_

  all <- dplyr::mutate(
    all,
    a = 2 * .data$l_f -
      2 * .env$k *.data$l_f -
      2 * .env$k^2 * .data$a_f +
      4 * .env$k * .data$a_f -
      2 * .data$a_f,
    b = pi * .data$l_f^2 -
      4 * .data$l_f +
      2 * .env$k * .data$l_f -
      4 * .env$k * .data$a_f +
      4 * .data$a_f,
    c = 2 * .data$l_f - 2 * .data$a_f,
    alpha = - b / 2 / a - sqrt(b^2 / 4 / a^2 - c / a),
    alpha = dplyr::if_else(
      type == "mid",
      (a_f[type == "ups"] * alpha[type == "ups"] + a_f[type == "low"] * alpha[type == "low"]) /
        (a_f[type == "ups"] + a_f[type == "low"]),
      alpha),
    tci = dplyr::case_when(alpha < 0 ~ 0,
                           alpha > 100 ~ 100,
                           TRUE ~ alpha * 100))


  dplyr::mutate(
    all,
    l_final = pl_c2s * avg$lstr2div,
    z_final = pz_c2s * avg$zcr2st,
    s_final = z_final/l_final * 100,
    pa_final = pa * 100,
    pl_final = l_final / sum(l_final) * 100,
    pz_final = z_final / sum(z_final) * 100,
    wi = seg_cal$qweti1)
}

ls_factor_all <- function(pnts, edge_row, edge_col, nrows, ncols) {

  omit_edges(pnts, edge_row, edge_col, nrows, ncols) |>
    dplyr::select(elev, zcr2st, lstr2div) |>
    dplyr::mutate(
      s = zcr2st / lstr2div,
      sin_theta = sin(zcr2st / sqrt(zcr2st^2 + lstr2div^2)),
      betta = (sin_theta / 0.0896) / (3 * sin_theta^0.8 + 0.56),
      m = betta / (betta + 1),
      l_factor = (lstr2div * 3.28084 / 72.6)^m,
      s_factor = dplyr::if_else(s < 0.09, 10.8 * sin_theta + 0.03, 16.8 * sin_theta - 0.5),
      ls_factor = l_factor * s_factor) |>
    tidyr::drop_na("elev", "zcr2st", "lstr2div") |>
    dplyr::filter(.data$elev != 0,
                  .data$zcr2st != 0,
                  .data$lstr2div != 0) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      s_sin_theta = .data$s / sqrt(1 + .data$s^2),
      s_s_factor = dplyr::if_else(.data$s < 0.09,
                                  10.8 * .data$s_sin_theta + 0.03,
                                  16.8 * .data$s_sin_theta - 0.5),
      s_l_factor = .data$ls_factor / .data$s_s_factor,  # PROBLEM
      s_betta = .data$s_sin_theta / 0.0896 / (3 * .data$s_sin_theta^0.8 + 0.56),
      s_m = .data$s_betta / (1 + .data$s_betta),
      s_len = exp(log(.data$s_l_factor) / .data$s_m) * 72.6 / 3.28084,  # PROBLEM
      s_z = .data$s_len * .data$s) |>  # PROBLEM
    stats() |>
    percentiles_format()
}

ls_factor <- function(pnts_no_edge) {

  pnts_no_edge |>
    dplyr::select(elev, zcr2st, lstr2div) |>
    dplyr::mutate(
      s = zcr2st / lstr2div,
      sin_theta = sin(zcr2st / sqrt(zcr2st^2 + lstr2div^2)),
      betta = (sin_theta / 0.0896) / (3 * sin_theta^0.8 + 0.56),
      m = betta / (betta + 1),
      l_factor = (lstr2div * 3.28084 / 72.6)^m,
      s_factor = dplyr::if_else(s < 0.09, 10.8 * sin_theta + 0.03, 16.8 * sin_theta - 0.5),
      ls_factor = l_factor * s_factor) |>
    tidyr::drop_na("elev", "zcr2st", "lstr2div") |>
    dplyr::filter(.data$elev != 0,
                  .data$zcr2st != 0,
                  .data$lstr2div != 0) |>
    dplyr::as_tibble() |>
    stats() |>
    percentiles_format() |>
    dplyr::mutate(
      s_sin_theta = .data$s / sqrt(1 + .data$s^2),
      s_s_factor = dplyr::if_else(.data$s < 0.09,
                                  10.8 * .data$s_sin_theta + 0.03,
                                  16.8 * .data$s_sin_theta - 0.5),
      s_l_factor = .data$ls_factor / .data$s_s_factor,  # PROBLEM
      s_betta = .data$s_sin_theta / 0.0896 / (3 * .data$s_sin_theta^0.8 + 0.56),
      s_m = .data$s_betta / (1 + .data$s_betta),
      s_len = exp(log(.data$s_l_factor) / .data$s_m) * 72.6 / 3.28084,
      s_z = .data$s_len * .data$s)
}

avg_topo <- function(topo, lsf) {
  dplyr::filter(topo, .data$name == "avg") |>
    dplyr::mutate(zcr2st = lsf$zcr2st[2],
                  lstr2div = lsf$lstr2div[2]) |>
    # TODO: Ask Li, Sheng if these are constants
    dplyr::mutate(slope3 = 6.87521764069166,
                  slope4 = 309.311743656845)
}

ws_density <- function(pnts, allpit, meta) {

  WDqry01 <- dplyr::count(pnts, .data$pit_row, .data$pit_col, name = "pit_pnt_cnts") |>
    dplyr::filter(!(.data$pit_row == 0 & .data$pit_col == 0))

  # Fetch only pits on exact edges
  WDqry03 <- pnts |>
    dplyr::filter(row %in% c(1, meta$nrows) | col %in% c(1, meta$ncols)) |>
    dplyr::select("pit_row", "pit_col") |>  # WDqry02
    dplyr::filter(!(.data$pit_row == 0 & .data$pit_col == 0))

  # Get pit counts without pits on exact edge
  WDqry06 <- allpit |>
    # WDqry05 - Remove pits on exact edges
    dplyr::select("seqno", "row", "col") |>
    dplyr::anti_join(WDqry03, by = c("row" = "pit_row", "col" = "pit_col")) |>
    # WDqry06
    dplyr::inner_join(WDqry01, by = c("col" = "pit_col", "row" = "pit_row"))

  # Get pit counts without pits in edge buffer
  WDqry011 <- allpit |>
    omit_edges(meta = meta) |>
    dplyr::left_join(WDqry01, by = c("row" = "pit_row", "col" = "pit_col")) |>
    dplyr::select("row", "col", "pit_pnt_cnts")

  method <- "remove_edge_pits"
  if(nrow(WDqry06) > 0) {
    temp <- WDqry06              # From 500_WDqry
  } else if(nrow(WDqry011) > 0){
    temp <- WDqry011             # From 5001_WDqry
  } else {
    method <- "no_edge_pits"
  }

  if(method == "remove_edge_pits") {
    pit_wo_edge <- temp |>
      dplyr::summarize(pit_cts_10 = quantile(pit_pnt_cnts, 0.1),
                       pit_cts_25 = quantile(pit_pnt_cnts, 0.25),
                       pit_cts_50 = quantile(pit_pnt_cnts, 0.50),
                       pit_cts_75 = quantile(pit_pnt_cnts, 0.75),
                       pit_cts_90 = quantile(pit_pnt_cnts, 0.90)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0)))

    # 510_WDqry
    WDqry08 <- WDqry03 |>  # Get small edge sheds
      dplyr::inner_join(WDqry01, by = c("pit_row", "pit_col")) |>  # WDqry04
      dplyr::filter(.data$pit_pnt_cnts < pit_wo_edge$pit_cts_10)

    WDqry09 <- WDqry01 |>  # Omit small edge sheds
      dplyr::anti_join(WDqry08, by = c("pit_row", "pit_col"))

    real_pit_avg_area <- mean(WDqry09$pit_pnt_cnts)  # From 510_WDqry

  } else if(method == "no_edge_pits") {
    real_pit_avg_area <- mean(WDqry01$pit_pnt_cnts)
  }

  real_pit_avg_area

}

ws_drainage <- function(pnts, pit, allpit, meta) {
  dplyr::select(allpit, "seqno", "row", "col", "pit_row", "pit_col") |>
    dplyr::left_join(pit, by = c("pit_row", "pit_col")) |>
    dplyr::select("seqno", "row", "col", "shed_area") |>
    keep_edges(edge_row = meta$edge_row_ws, edge_col = meta$edge_col_ws,
               meta$nrows, meta$ncols) |>
    dplyr::pull(.data$shed_area) |>
    sum()
}
