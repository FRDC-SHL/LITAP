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

  # Output File paths
  summary_file <- file.path(folder, paste0(basename(folder), "_topo_summary.xlsx"))
  allpoints_file <- file.path(folder, paste0(basename(folder), "_all_points.xlsx"))
  allpeak_file <- file.path(folder, paste0(basename(folder), "_all_peaks.xlsx"))
  allpit_file <- file.path(folder, paste0(basename(folder), "_all_pits.xlsx"))
  allcrest_file <- file.path(folder, paste0(basename(folder), "_all_crests.xlsx"))
  allstream_file <- file.path(folder, paste0(basename(folder), "_all_streams.xlsx"))

  # Testing
  testing <- folder == "testing"

  # Checks
  if(!testing) check_folder(folder)

  stats_pit <- all_stats(folder, "pit")
  stats_inverted <- all_stats(folder, "inverted")

  allpoints <- all_points(folder)

  allpeak <- all_peak(points = allpoints, stats = stats_inverted)
  allpit <- all_pit(points = allpoints, stats = stats_pit)
  allcrest <- all_crest(points = allpoints, stats = stats_inverted)
  allstream <- all_stream(points = allpoints, stats = stats_pit)

  # Calculate topographic summary - Only if have the correct facets
  if(testing || check_facets(folder)) {
    topo_summary(folder, allpoints, allpeak, allpit, allcrest, allstream, out_file = summary_file)
  }

  openxlsx::write.xlsx(allpeak, allpeak_file)
  openxlsx::write.xlsx(allpit, allpit_file)
  openxlsx::write.xlsx(allcrest, allcrest_file)
  openxlsx::write.xlsx(allstream, allstream_file)
}


topo_summary <- function(folder, allpoints, allpeak, allpit, allcrest, allstream,
                         out_file = "topographic_summary.xlsx") {

  # Load data
  testing <- folder == "testing"

  if(testing) {

    facet <- test_facet()
    topo <- test_topo()
    stats <- test_stats()

    log <- test_log()

  } else {

    facet <- get_previous(folder, where = "facet", step = "fuzc")
    topo <- readxl::read_excel(file.path(folder, "topographic_derivatives.xlsx"))
    stats <- get_previous(folder, where = "flow", step = "pit", type = "stats")

    log <- readr::read_lines(list.files(folder, "facet.log", full.names = TRUE))
  }

  # T1: Metadata ------------------------------
  meta <- tbl_meta(facet, log)
  tbl_meta <- meta$tbl_meta
  meta <- meta$meta

  # Prepare data -----------------------------------
  pnts <- allpoints

  nrows <- max(facet$row)
  ncols <- max(facet$col)

  grid <- (max(facet$x) - min(facet$x) + 1) / ncols
  if(grid != (max(facet$y) - min(facet$y) + 1) / nrows) {
    stop("Inconsistent grid size. Grid must be square", call. = FALSE)
  }

  min_x <- min(facet$x, na.rm = TRUE)
  max_x <- max(facet$x, na.rm = TRUE)
  min_y <- min(facet$y, na.rm = TRUE)
  max_y <- max(facet$y, na.rm = TRUE)

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

  # Seg-Cal ---- SlpCal - A32:M38
  seg_cal <- le5_avg(pnts, le)

  # Bdr-Cal
  ix <- ix_avgs(pnts, edge_row, edge_col, nrows, ncols)

  # PntCounts
  cnts <- pnts_count(allpeak, allpit, allcrest, allstream, le)

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


  density <- ws_density(pnts, allpit, edge_row, edge_col, nrows, ncols)
  edge_drainage <- ws_drainage(pnts, stats, allpit, edge_row_ws, edge_col_ws, nrows, ncols)

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

  create_excel(out_file, meta, x1, x2, x3, x4, x5)
}


get_edge <- function(x, type) {
  type <- paste0("edge_", type)
  stringr::str_subset(x, type) |>
    stringr::str_extract(paste0("(?<=", type, " = )[0-9]{1,}")) |>
    as.numeric()
}

get_detail <- function(x, type, pattern = ".+") {
  stringr::str_subset(x, type) |>
    stringr::str_extract(paste0("(?<=", type, ")", pattern))
}
