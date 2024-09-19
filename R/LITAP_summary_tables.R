#' Create summary watersheed tables in Excel
#'
#' Using output from the flow_mapper(), form_mapper() and facet_maper() runs,
#' summarizes into various tables in Excel worksheet.
#'
#' @param file Character. Name of output file (.xlsx)
#' @param min_x Numeric. Override the starting x coordinate in the original
#'   data (in meters)
#' @param min_y Numeric. Override the starting y coordinate in the original
#'   data (in meters)
#'
#' @inheritParams args
#'
#' @examples
#'
#' summary_tables()
#'
#'
summary_tables <- function(folder) {

  #TODO: Figure out edge row stuff based on different number of edge rows
  #TODO: Q - X/Y are UTMs? I.e. meters?

  # Checks
  check_folder(folder)

  # Load data
  testing <- FALSE
  if(testing) {
    message("Using test data for summary_tables()")
    t <- test_files()
    pnts <- t$pnts
    facet <- t$facet
    topo <- t$topo
    pit <- t$pit
  } else {
    pnts <- all_points(folder)
    facet <- get_previous(folder, step = "fuzc", where = "facet")
    topo <- readxl::read_excel(file.path(folder, "topographic_derivatives.xlsx"))
  }

  # Calculate all point data frames



  # Calculate topographic summary
  if(check_facets(facet)) topo_summary(folder, file = "topo_summary.xlsx")#, min_x, min_y)

}


topo_summary <- function(folder, file = "topo_summary.xlsx") {

  # Load data
  testing <- TRUE
  if(testing) {
    t <- test_files()
    pnts <- t$pnts
    facet <- t$facet
    topo <- t$topo
    pit <- t$pit
  } else {
    # Checks
    check_folder(folder)

    pnts <- all_points(folder)
    facet <- get_previous(folder, step = "fuzc", where = "facet")
    topo <- readxl::read_excel(file.path(folder, "topo_derivatives.xlsx"))
  }

  check_facets(facet, fun = stop)

  log <- readr::read_lines(list.files(folder, "facet.log", full.names = TRUE))
  date <- stringr::str_subset(log, "Run started") |>
    stringr::str_extract("[0-9-]+ [0-9:]+")
  edge_row <- get_edge(log, "row")
  edge_col <- get_edge(log, "col")
  edge_row_ws <- as.integer(edge_row / 3) + 1
  edge_col_ws <- as.integer(edge_col / 3) + 1

  nrows <- max(pnts$row)
  ncols <- max(pnts$col)

  grid <- (max(pnts$x) - min(pnts$x) + 1) / ncols
  if(grid != (max(pnts$y) - min(pnts$y) + 1) / nrows) {
    stop("Inconsistent grid size. Grid must be square", call. = FALSE)
  }

  min_x <- min(pnts$x, na.rm = TRUE)
  max_x <- max(pnts$x, na.rm = TRUE)
  min_y <- min(pnts$y, na.rm = TRUE)
  max_y <- max(pnts$y, na.rm = TRUE)

  ## T1: Metadata ------------------------------
  meta <- dplyr::tibble(Run = folder,
                        `facet_mapper() Run` = date,
                        `Summary Table Creation` = Sys.Date(),
                        `LITAP version` = packageVersion("LITAP"),
                        Cols = ncols,
                        Rows = nrows,
                        `Points (n)` = nrow(pnts),
                        `Cell Size` = grid,
                        `Edge Rows` = edge_row,
                        `Edge Cols` = edge_col,
                        `Edge Rows (WS)` = edge_row_ws,
                        `Edge Cols (WS)` = edge_col_ws,
                        `Min X` = min_x,
                        `Max X` = max_x,
                        `Min Y` = min_y,
                        `Max Y` = max_y,
                        `Min Z` = min(pnts$elev, na.rm = TRUE),
                        `Max Z` = max(pnts$elev, na.rm = TRUE))


  le <- dplyr::select(facet, "row", "col", "seqno", "max_facet") |>
    omit_edges(edge_row = edge_row, edge_col = edge_col,
               nrow = nrows, ncol = ncols)

  # Seg-Cal
  seg_cal <- le5_avg(pnts, le)

  # Bdr-Cal
  ix <- ix_avgs(pnts, facet, edge_row, edge_col, nrows, ncols)

  # PntCounts
  cnts <- pnts_count(pnts, le)

  # LS factor Calculations
  lsf <- ls_factor(pnts, edge_row, edge_col, nrows, ncols)

  avg <- dplyr::filter(topo, .data$name == "avg") |>
    dplyr::mutate(zcr2st = lsf$zcr2st[2],
                  lstr2div = lsf$lstr2div[2]) |>
    # TODO: Ask Li, Sheng if these are constants
    dplyr::mutate(slope3 = 6.87521764069166,
                  slope4 = 309.311743656845)

  # Average calcs
  slp_cal <- mid_calc(ix, cnts, avg, seg_cal)


  density <- ws_density(pnts, edge_row, edge_col, nrows, ncols)
  edge_drainage <- ws_drainage(pnts, pit, edge_row_ws, edge_col_ws, nrows, ncols)

  ## T2: Topographic derivatives of each slope segment on the representative hillslope ---------
  x1 <- dplyr::select(
    slp_cal, "type",
    "L (m)" = "l_final", "Z (m)" = "z_final", "S (%)" = "s_final",
    "P_A (%)" = "pa_final", "P_L (%)" = "pl_final", "P_Z (%)" = "pz_final",
    "WI" = "wi") |>
    dplyr::mutate(type = toupper(type)) |>
    tidyr::pivot_longer(-type) |>
    dplyr::group_by(name) |>
    dplyr::mutate(Sum = sum(.data$value)) |>
    tidyr::pivot_wider(names_from = "type", values_from = "value") |>
    dplyr::relocate("Sum", .after = dplyr::last_col()) |>
    dplyr::mutate(Avg = NA_real_)
  x1$Sum[3:7] <- NA_real_
  x1$Avg[3] <- x1$Sum[2] / x1$Sum[1] * 100
  x1$Avg[7] <- avg$qweti1

  ## T3: Area-weighted average values of selected topographic derivatives for each slope segment ----
  x2 <- seg_cal |>
    dplyr::select("SG (%)" = "slope_pct", "Aspect (degree)" = "aspect",
                  "PrCurv (degree / 100m)" = "prof", "PlCurv (degree / 100m)" = "plan",
                  "Qarea" = "qarea1", "Qweti" = "qweti1") |>
    dplyr::mutate(type = toupper(c("cst", "ups", "mid", "low", "dep"))) |>
    tidyr::pivot_longer(-"type") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value") |>
    dplyr::bind_cols(
      dplyr::select(avg, "slope", "aspect", "prof", "plan", "qarea1", "qweti1") |>
        tidyr::pivot_longer(cols = dplyr::everything()) |>
        dplyr::select("Avg" = "value")) |>
    dplyr::mutate(` ` = "") |>
    dplyr::relocate(` `, .before = "Avg")

  ## T4: Statistics of selected topographic derivatives (area-weighted) -------
  x3 <- topo |>
    dplyr::select("name",
                  "SG" = "slope", "CV_pr" = "prof", "CV_pl" = "plan",
                  "WI" = "qweti1", "Lp2p" = "lpit2peak", "Zp2p" = "zpit2peak",
                  "Ld2c" = "lstr2div", "Zd2c" = "zcr2st") |>
    dplyr::left_join(dplyr::select(lsf, "name", "Sd2c" = "s"), by = "name") |>
    dplyr::mutate(Sd2c = .data$Sd2c * 100) |>
    dplyr::filter(name %in% c("avg", "sd",
                              "min", "1%", "5%", "10%", "25%", "50%",
                              "75%", "90%", "95%", "99%", "max")) |>
    tidyr::pivot_longer(-"name", names_to = "Parameter") |>
    tidyr::pivot_wider(names_from = "name", values_from = "value") |>
    dplyr::mutate(Unit = c("%", "° (100 m) -1", "° (100 m) -1", "",
                           "m", "m", "m", "m", "%")) |>
    dplyr::relocate("Unit", .after = "Parameter") |>
    dplyr::rename("Average" = "avg", "SD" = "sd") |>
    dplyr::rename_with(tools::toTitleCase)

  ## T5: Indexes, parameters and ratios characterizing different aspects of the landscape topography -----
  x4 <- dplyr::tibble(
    Z_max = topo$zpit2peak[topo$name == "max"],
    D_ws = 1000000 / .env$density / grid^2,
    A_ws = 100 / D_ws,
    P_osd = .env$edge_drainage / nrow(pnts) * 100,
    RL_w = avg$lpit2peak / avg$lstr2div,
    RZ_w = avg$zpit2peak / avg$zcr2st,
    RZ_l = Z_max / avg$zpit2peak,
    Dr = sum(cnts$crest_cnt) / .env$grid / .env$nrows / .env$ncols * 10000,
    Dc = sum(cnts$stream_cnt) / .env$grid / .env$nrows / .env$ncols * 10000,
    TCI_UP = slp_cal$tci[slp_cal$type == "ups"],
    TCI_LOW = slp_cal$tci[slp_cal$type == "low"],
    TCI = slp_cal$tci[slp_cal$type == "mid"]) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Parameter",
                        values_to = "value") |>
    dplyr::mutate(Unit = c("m", "/ 100 ha", "ha", "%", "", "", "", "m/ha", "m/ha", "%", "%", "%")) |>
    dplyr::relocate(Unit, .after = "Parameter")

  ## T6: Location (X) and relief (Z) of points (at the top of the slope and the end of each segment) along the modal hillslopes ----
  x5 <- slp_cal |>
    dplyr::select("type", "pl_c2s") |>
    tidyr::pivot_wider(names_from = "type", values_from = "pl_c2s") |>
    dplyr::bind_cols(
      lsf |>
        dplyr::select("name", "s_len") |>
        dplyr::filter(.data$name %in% c("10%", "20%", "25%", "30%", "40%",
                                        "50%", "60%", "70%", "75%", "80%", "90%")))
  # Add in the avg values from lstr2div
  x5 <- dplyr::slice(x5, 1) |>
    dplyr::mutate(s_len = lsf$lstr2div[2], name = "avg") |>
    dplyr::add_row(x5) |>
    dplyr::mutate(
      top = 0,
      cst = .data$top + .data$cst * .data$s_len,
      ups = .data$cst + .data$ups * .data$s_len,
      mid = .data$ups + .data$mid * .data$s_len,
      low = .data$mid + .data$low * .data$s_len,
      dep = .data$low + .data$dep * .data$s_len) |>
    dplyr::select(-"s_len") |>
    dplyr::rename_with(\(x) paste0(toupper(x), "_X")) |>
    dplyr::rename("Stat" = "NAME_X") |>
    dplyr::relocate("Stat", "Top_X" = "TOP_X") |>
    dplyr::mutate(Stat = dplyr::case_when(.data$Stat == "avg" ~ "Avg",
                                          .data$Stat == "50%" ~ "50% (Median)",
                                          TRUE ~ .data$Stat))

  x6 <- slp_cal |>
    dplyr::select("type", "pz_c2s") |>
    tidyr::pivot_wider(names_from = "type", values_from = "pz_c2s") |>
    dplyr::bind_cols(lsf |>
                       dplyr::select("name", "s_z") |>
                       dplyr::filter(.data$name %in% c("10%", "20%", "25%", "30%", "40%",
                                                       "50%", "60%", "70%", "75%", "80%", "90%")))

  # Add in the avg values from zcr2st
  x6 <- dplyr::slice(x6, 1) |>
    dplyr::mutate(s_z = lsf$zcr2st[2], name = "avg") |>
    dplyr::add_row(x6) |>
    dplyr::mutate(
      top = .data$s_z,
      top = dplyr::if_else(name == "avg",
                           lsf$zcr2st[lsf$name == "avg"],
                           .data$top),
      cst = .data$top - .data$cst * .data$s_z,
      ups = .data$cst - .data$ups * .data$s_z,
      mid = .data$ups - .data$mid * .data$s_z,
      low = .data$mid - .data$low * .data$s_z,
      dep = .data$low - .data$dep * .data$s_z) |>
    dplyr::select(-"s_z") |>
    dplyr::rename_with(\(x) paste0(toupper(x), "_Z")) |>
    dplyr::rename("Stat" = "NAME_Z") |>
    dplyr::relocate("Stat", "Top_Z" = "TOP_Z") |>
    dplyr::mutate(Stat = dplyr::case_when(.data$Stat == "avg" ~ "Avg",
                                          .data$Stat == "50%" ~ "50% (Median)",
                                          TRUE ~ .data$Stat))

  # Combine x5 and x6 into one table
  x5 <- dplyr::left_join(x5, x6, by = "Stat")

  create_excel(file.path(folder, file), meta, x1, x2, x3, x4, x5)
}



get_edge <- function(x, type) {
  type <- paste0("edge_", type)
  stringr::str_subset(x, type) |>
    stringr::str_extract(paste0("(?<=", type, " = )[0-9]{1,}")) |>
    as.numeric()
}




test_files <- function(folder = "~/Dropbox/LITAP files/LandMapR_BR3Raw_20210427/LandMapR_Files/",
                       id = "31M",
                       avg_file = "BR3_1m_20210427.xlsx",
                       grid = 1) {

  d <- paste0(folder, "/", id)

  suppressMessages({

    topo <- readxl::read_excel(file.path(folder, "..", avg_file),
                              sheet = "PercentileAccu", skip = 1) |>
      janitor::clean_names() |>
      dplyr::mutate(name = tolower(name),
                    name = dplyr::case_when(name == "stdev" ~ "sd",
                                            name == "median" ~ "50%",
                                            name == "average" ~ "avg",
                                            TRUE ~ name)) |>
      dplyr::rename(qarea1 = a_qarea, qweti1 = a_qweti)

    pnts <- foreign::read.dbf(paste0(d, "idem.dbf")) |>
      janitor::clean_names() |>
      dplyr::select(seqno = seq_no, row, col, idrec = drec) |>
      dplyr::left_join(foreign::read.dbf(paste0(d, "dem.dbf")) |>
                         janitor::clean_names() |>
                         dplyr::select(seqno = seq_no, drec, elev),
                       by = "seqno") |>
      dplyr::mutate(x = col * grid,
                    y = rev(row) * grid,
                    elev = round(elev, 3),
                    elev = dplyr::na_if(elev, -9999)) |>
      dplyr::rename(inv_drec = idrec)

    facet <- readr::read_tsv(paste0(d, "fuzc.txt")) |>
      janitor::clean_names() |>
      dplyr::left_join(dplyr::select(pnts, seqno, row, col), by = "seqno")

    form1 <- readr::read_tsv(paste0(d, "Relz.txt")) |>
      janitor::clean_names() |>
      dplyr::left_join(dplyr::select(pnts, seqno, row, col), by = "seqno") |>
      dplyr::mutate(l2pit = sqrt((row - pit_row)^2 + (col - pit_col)^2) * grid,
                    l2peak = sqrt((row - pk_row)^2 + (col - pk_col)^2) * grid,
                    l2str = sqrt((row - st_row)^2 + (col - st_col)^2) * grid,
                    l2div = sqrt((row - cr_row)^2 + (col - cr_col)^2) * grid,
                    lpit2peak = l2pit + l2peak,
                    lstr2div = l2str + l2div,
                    ppit2peakl = dplyr::if_else(lpit2peak <= 0, 0, l2pit / lpit2peak * 100),
                    pstr2divl = dplyr::if_else(lstr2div <= 0, 0, l2str/lstr2div * 100)) |>
      dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~round(.x, 3))) |>
      dplyr::select(-row, -col) |>
      dplyr::rename(peak_row = pk_row, peak_col = pk_col, peak_elev = pk_elev) |>
      dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~dplyr::na_if(.x, -9999)))

    form2 <- readr::read_tsv(paste0(d, "Form.txt")) |>
      janitor::clean_names() |>
      dplyr::rename(slope_pct = slope, qarea1 = qarea, qweti1 = qweti)


    pnts <- dplyr::left_join(pnts, form1, by = "seqno") |>
      dplyr::left_join(form2, by = "seqno")

    pit <- foreign::read.dbf(paste0(d, "pit.dbf")) |>
      janitor::clean_names()
  })

  list("facet" = facet, "pnts" = pnts, "topo" = topo, "pit" = pit)
}
