ix_avgs <- function(pnts, facet, edge_row, edge_col, nrows, ncols) {

  tbl_le_sum <- dplyr::select(facet, "seqno", "max_facet")

  j <- dplyr::left_join(tbl_le_sum, pnts, by = "seqno")

  # TODO: Note that this means that j1 DOESNT get filtered... should it?
  #  - I think this is because we're joining later by j drec -> j1, so we want
  #    bits in the edges... correct?
  j1 <- dplyr::mutate(j, seqno2 = .data$seqno)

  j <- omit_edges(j, edge_row, edge_col, nrow = nrows, ncol = ncols) |>
    dplyr::rename("seqno1" = "seqno")

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

le5_avg <- function(pnts, le) {
  dplyr::inner_join(le, pnts, by = c("row", "col", "seqno")) |>
    dplyr::filter(max_facet != 0) |>
    dplyr::select("seqno", "max_facet", "z2cr", "l2div", "z2st", "l2str",
                  "slope_pct", "aspect", "prof", "plan", "qarea1", "qweti1") |>
    dplyr::group_by(max_facet) |>
    dplyr::summarize(counts = n(), dplyr::across(-"seqno", mean))
}

pnts_count <- function(pnts, le) {
  type_count(pnts, le, "crest") |>
    dplyr::left_join(type_count(pnts, le, "stream"), by = "max_facet") |>
    dplyr::left_join(type_count(pnts, le, "peak"), by = "max_facet") |>
    dplyr::left_join(type_count(pnts, le, "pit"), by = "max_facet") |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("_cnt"), ~tidyr::replace_na(.x, 0)))

}

type_count <- function(pnts, le, type) {
  all_summary(pnts, type) |>
    dplyr::select(seqno) |>
    dplyr::inner_join(le, by = "seqno")  |>
    dplyr::group_by(max_facet) |>
    dplyr::summarize("{type}_cnt" := dplyr::n())
}


omit_edges <- function(x, edge_row, edge_col, nrow = NULL, ncol = NULL) {

  if(is.null(nrow)) nrow <- max(x$rows)
  if(is.null(ncol)) ncol <- max(x$cols)

  dplyr::filter(
    x,
    .data$row > (.env$edge_row), .data$row < (.env$nrow - .env$edge_row),
    .data$col > (.env$edge_col), .data$col < (.env$ncol - .env$edge_col))
}


mid_calc <- function(ix, cnts, avg, avg_le) {

  browser()
  rel_cr_cnts1 <- (cnts$crest_cnt[1] / sum(cnts$crest_cnt[1:2]))   # H48
  rel_st_cnts5 <- (cnts$stream_cnt[5] / sum(cnts$stream_cnt[4:5])) # H52

  # Cst
  cst <- data.frame(zc2s = ix$ix1_z2cr_avg * rel_cr_cnts1, # C2S - Zc2s
                    lc2s = ix$ix1_l2cr_avg * rel_cr_cnts1, # C2S - Lc2s
                    zp2p = ix$ix1_z2peak_avg,              # P2P - Zp2p
                    lp2p = ix$ix1_l2peak_avg               # P2P - Lp2p
  )

  # Ups
  ups <- data.frame(zc2s = ix$ix2_z2cr_avg - cst$zc2s,   # C2S - Zc2s
                    lc2s = ix$ix2_l2cr_avg - cst$lc2s,   # C2S - Lc2s
                    zp2p = ix$ix2_z2peak_avg - cst$zp2p, # P2P - Zp2p
                    lp2p = ix$ix2_l2peak_avg - cst$lp2p, # P2P - Lp2p
                    cur = tan(avg_le$prof[2] * pi / 180))

  # Dep
  dep <- data.frame(
    zc2s = ix$ix5_z2str_avg * rel_st_cnts5,
    lc2s = ix$ix5_l2str_avg * rel_st_cnts5,
    zp2p = ix$ix5_z2pit_avg,
    lp2p = ix$ix5_l2pit_avg,
    cur = tan(avg_le$prof[5] * pi / 180))

  # Low
  low <- data.frame(
    zc2s = ix$ix4_z2str_avg - dep$zc2s,
    lc2s = ix$ix4_l2str_avg - dep$lc2s,
    zp2p = ix$ix4_z2pit_avg - dep$zp2p,
    lp2p = ix$ix4_l2pit_avg - dep$lp2p,
    cur = tan(avg_le$prof[4] * pi / 180))

  # Mid
  mid <- data.frame(
    zc2s = avg$zcr2st - cst$zc2s - ups$zc2s - low$zc2s - dep$zc2s,
    lc2s = avg$lstr2div - cst$lc2s - ups$lc2s - low$lc2s - dep$lc2s,
    #zp2p = ???? - cst$zp2p - ups$zp2p - low$zp2p - deps$zp2p,
    #zc2s = ???? - cst$lp2p - ups$lp2p - low$lp2p - deps$lp2p
    cur = tan(avg_le$prof[3] * pi / 180))

  scal_cst <- dplyr::tibble(
    cur = tan(avg_le$prof[1] * pi / 180),
    cal1 = cur * cst$lc2s/2/100,
    cal2 = cst$lc2s,
    cal3 = cal2 * cal1 / avg$lstr2div/2)

  scal_ups <- dplyr::tibble(
    cur = tan(avg_le$prof[2] * pi / 180),
    cal1 = cur * ups$lc2s/2/100,
    cal2 = 2 * cst$lc2s + ups$lc2s,
    cal3 = cal2 * cal1 / avg$lstr2div / 2)

  scal_mid <- dplyr::tibble(
    cur = tan(avg_le$prof[3] * pi / 180),
    cal1 = cur * mid$lc2s/2/100,
    cal2 = scal_ups$cal2 + ups$lc2s + mid$lc2s,
    cal3 = cal2 * cal1 / avg$lstr2div / 2
  )

  scal_low <- dplyr::tibble(
    cur = tan(avg_le$prof[4] * pi / 180),
    cal1 = cur * low$lc2s/2/100,
    cal2 = scal_mid$cal2 + mid$lc2s + low$lc2s,
    cal3 = cal2 * cal1 / avg$lstr2div / 2
  )

  scal_dep <- dplyr::tibble(
    cur = tan(avg_le$prof[5] * pi / 180),
    cal1 = cur * dep$lc2s/2/100,
    cal2 = scal_low$cal2 + low$lc2s + dep$lc2s,
    cal3 = cal2 * cal1 / avg$lstr2div / 2
  )

  scal <- dplyr::bind_rows(scal_cst, scal_ups, scal_mid, scal_low, scal_dep) |>
    dplyr::mutate(type = c("cst", "ups", "mid", "low", "dep"))

  slp_ratio <- avg$zcr2st / avg$lstr2div  # C72
  cal3_sum <- sum(scal$cal3) + slp_ratio  # I72

  sg_dep <- dplyr::tibble(
    sg = cal3_sum * 100,
    midpntz = sg * dep$lc2s/2/100,
    z = 0)
    #seg_h = sg_low$z - z,
    #seg_sg = seg_h/dep$lc2s*100)

  sg_low <- dplyr::tibble(
    sg = sg_dep$sg - scal_dep$cur*dep$lc2s/2,
    z = sg_dep$midpntz + dep$lc2s/2 * (sg/100),
    midpntz = low$lc2s * sg/2/100 + z)

  # Start HERE! ------------
  sg_mid <- dplyr::tibble()

  sg_ups <- dplyr::tibble()

  sg_cst <- dplyr::tibble()

  # Now do seg_h and seg_sg's...



  #cst$sg <- ups$sg - ups$cur*ups$lc2s/2 # NEED ups$sg FRIST

  # SiteSum row 17
  cbind(
    Cst_L = cst$lc2s,  # C2S P_l (%)
    Ups_L = ups$lc2s,  # C2S P_l (%)
    Mid_L = mid$lc2s,  # C2S P_l (%)
    Low_L = low$lc2s,
    Dep_L = dep$lc2s)

  # SiteSum row 18





}
