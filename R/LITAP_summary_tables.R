#' Create summary watershed tables in Excel
#'
#' Using output from the `flow_mapper()`, `form_mapper()` and `facet_mapper()` runs,
#' summarizes into various tables in Excel worksheet. Based on technique from
#' Sheng et al. (2011).
#'
#' @inheritParams args
#'
#' @references
#' Sheng Li, David A. Lobb, Brian G. McConkey, R. A. MacMillan, Alan Moulin,
#' and Walter R. Fraser. 2011. Extracting topographic characteristics of landforms
#' typical of Canadian agricultural landscapes for agri-environmental modeling.
#' I. Methodology. Canadian Journal of Soil Science 91(2), 251-266.
#' <https://doi.org/10.4141/CJSS10080>.
#'
#' @examples
#' \dontrun{summary_tables()}
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
  openxlsx::write.xlsx(allpoints, allpoints_file)
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

    log <- readr::read_lines(list.files(folder, "facet.log", full.names = TRUE), progress = FALSE)
  }

  # T1: Metadata ------------------------------
  meta <- tbl_meta(facet, log)
  tbl_meta <- meta$tbl_meta
  meta <- meta$meta

  # Prepare data -----------------------------------
  pnts <- allpoints
  pnts_no_edge <- omit_edges(pnts, meta = meta)

  ## Sub calculations --------------------------
  seg_cal <- le5_avg(pnts_no_edge)  # Seg-Cal ---- SlpCal - A32:M38
  lsf <- ls_factor(pnts_no_edge)    # LS factor Calculations
  avg <- avg_topo(topo, lsf)
  cnts <- pnts_count(allpeak, allpit, allcrest, allstream, pnts_no_edge) # PntCounts
  slp_cal <- mid_calc(pnts, pnts_no_edge, cnts, avg, seg_cal)            # Average calcs
  density <- ws_density(pnts, allpit, meta)
  edge_drainage <- ws_drainage(pnts, stats, allpit, meta)

  ## T2: Topographic derivatives of each slope segment on the representative hillslope ---------
  tbl_x1 <- tbl_derivatives(slp_cal, avg)

  ## T3: Area-weighted average values of selected topographic derivatives for each slope segment ----
  tbl_x2 <- tbl_avg(seg_cal, avg)

  ## T4: Statistics of selected topographic derivatives (area-weighted) -------
  tbl_x3 <- tbl_stats(topo, lsf)

  ## T5: Indexes, parameters and ratios characterizing different aspects of the landscape topography -----
  tbl_x4 <- tbl_indices(topo, meta, avg, cnts, slp_cal, density, edge_drainage)

  ## T6: Location (X) and relief (Z) of points (at the top of the slope and the end of each segment) along the modal hillslopes ----
  tbl_x5 <- tbl_locs(slp_cal, lsf)

  # Write to excel file -----------------------------------------
  create_excel(out_file, tbl_meta, tbl_x1, tbl_x2, tbl_x3, tbl_x4, tbl_x5)
}

tbl_meta <- function(facet, log) {

  meta <- dplyr::summarize(
    facet,
    run = get_detail(log, "Input folder = "), # Run
    run_date = get_detail(log, "Run started: ", pattern = "[0-9-]+ [0-9:]+"),   # `facet_mapper() Run`
    summary_date = Sys.Date(),             # `Summary Table Creation`
    version = utils::packageVersion("LITAP"),   # `LITAP version`
    ncols = max(col),                      # Cols
    nrows = max(row),                      # Rows
    n = dplyr::n(),                        # `Points (n)`
    n_missing = sum(!is.na(elev)),         # `Points missing (n)`
    grid = calc_grid(facet),               # `Cell Size`
    edge_row = get_edge(log, "row"),       # `Edge Rows`
    edge_col = get_edge(log, "col"),       # `Edge Cols`
    edge_row_ws = as.integer(.data$edge_row / 3) + 1, #`Edge Rows (WS)`
    edge_col_ws = as.integer(.data$edge_col / 3) + 1, #`Edge Cols (WS)`
    min_x = min(.data$x, na.rm = TRUE),          # `Min X`
    max_x = max(.data$x, na.rm = TRUE),          # `Max X`
    min_y = min(.data$y, na.rm = TRUE),          # `Min Y`
    max_y = max(.data$y, na.rm = TRUE),          # `Max Y`
    min_z = min(.data$elev, na.rm = TRUE),       # `Min Z`
    max_z = max(.data$elev, na.rm = TRUE))       # `Max Z`

  nms <- c(
    "Run" = "run", "facet_mapper() Run" = "run_date",
    "Summary Table Creation" = "summary_date", "LITAP version" = "version",
    "Cols" = "ncols", "Rows" = "nrows",
    "Points (n)" = "n", "Points Not Missing (n)" = "n_missing",
    "Cell Size" = "grid", "Edge Rows" = "edge_row", "Edge Cols" = "edge_col",
    "Edge Rows (WS)" = "edge_row_ws", "Edge Cols (WS)" = "edge_col_ws",
    "Min X" = "min_x", "Max X" = "max_x", "Min Y" = "min_y", "Max Y" = "max_y",
    "Min Z" = "min_z", "Max Z" = "max_z")

  tbl_meta <- dplyr::rename(meta, dplyr::all_of(nms))

  list(meta = meta, tbl_meta = tbl_meta)
}

tbl_derivatives <- function(slp_cal, avg) {
  x1 <- dplyr::select(
    slp_cal, "type",
    "L (m)" = "l_final", "Z (m)" = "z_final", "S (%)" = "s_final",
    "P_A (%)" = "pa_final", "P_L (%)" = "pl_final", "P_Z (%)" = "pz_final",
    "WI" = "wi")  %>%
    dplyr::mutate(type = toupper(.data$type))  %>%
    tidyr::pivot_longer(-"type")  %>%
    dplyr::group_by(.data$name)  %>%
    dplyr::mutate(Sum = sum(.data$value))  %>%
    tidyr::pivot_wider(names_from = "type", values_from = "value")  %>%
    dplyr::relocate("Sum", .after = dplyr::last_col())  %>%
    dplyr::mutate(Avg = NA_real_)
  x1$Sum[3:7] <- NA_real_
  x1$Avg[3] <- x1$Sum[2] / x1$Sum[1] * 100
  x1$Avg[7] <- avg$qweti1

  x1
}

tbl_avg <- function(seg_cal, avg) {
  seg_cal  %>%
    dplyr::select("SG (%)" = "slope_pct", "Aspect (degree)" = "aspect",
                  "PrCurv (degree / 100m)" = "prof", "PlCurv (degree / 100m)" = "plan",
                  "Qarea" = "qarea1", "Qweti" = "qweti1")  %>%
    dplyr::mutate(type = toupper(c("cst", "ups", "mid", "low", "dep")))  %>%
    tidyr::pivot_longer(-"type")  %>%
    tidyr::pivot_wider(names_from = "type", values_from = "value")  %>%
    dplyr::bind_cols(
      dplyr::select(avg, "slope", "aspect", "prof", "plan", "qarea1", "qweti1")  %>%
        tidyr::pivot_longer(cols = dplyr::everything())  %>%
        dplyr::select("Avg" = "value"))  %>%
    dplyr::mutate(` ` = "")  %>%
    dplyr::relocate(" ", .before = "Avg")
}

tbl_stats <- function(topo, lsf) {
  topo  %>%
    dplyr::select("name",
                  "SG" = "slope", "CV_pr" = "prof", "CV_pl" = "plan",
                  "WI" = "qweti1", "Lp2p" = "lpit2peak", "Zp2p" = "zpit2peak",
                  "Ld2c" = "lstr2div", "Zd2c" = "zcr2st")  %>%
    dplyr::left_join(dplyr::select(lsf, "name", "Sd2c" = "s"), by = "name")  %>%
    dplyr::mutate(Sd2c = .data$Sd2c * 100)  %>%
    dplyr::filter(.data$name %in% c("avg", "sd",
                              "min", "1%", "5%", "10%", "25%", "50%",
                              "75%", "90%", "95%", "99%", "max"))  %>%
    tidyr::pivot_longer(-"name", names_to = "Parameter")  %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value")  %>%
    dplyr::mutate(Unit = c("%", "° (100 m) -1", "° (100 m) -1", "",
                           "m", "m", "m", "m", "%"))  %>%
    dplyr::relocate("Unit", .after = "Parameter")  %>%
    dplyr::rename("Average" = "avg", "SD" = "sd")  %>%
    dplyr::rename_with(tools::toTitleCase)
}


tbl_indices <- function(topo, meta, avg, cnts, slp_cal, density, edge_drainage) {
  dplyr::tibble(
    Z_max = topo$zpit2peak[topo$name == "max"],
    D_ws = 1000000 / .env$density / meta$grid^2,
    A_ws = 100 / .data$D_ws,
    P_osd = .env$edge_drainage / meta$n * 100,
    RL_w = avg$lpit2peak / avg$lstr2div,
    RZ_w = avg$zpit2peak / avg$zcr2st,
    RZ_l = .data$Z_max / avg$zpit2peak,
    Dr = sum(cnts$crest_cnt) / meta$grid / meta$nrows / meta$ncols * 10000,
    Dc = sum(cnts$stream_cnt) / meta$grid / meta$nrows / meta$ncols * 10000,
    TCI_UP = slp_cal$tci[slp_cal$type == "ups"],
    TCI_LOW = slp_cal$tci[slp_cal$type == "low"],
    TCI = slp_cal$tci[slp_cal$type == "mid"])  %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Parameter",
                        values_to = "value")  %>%
    dplyr::mutate(Unit = c("m", "/ 100 ha", "ha", "%", "", "", "", "m/ha", "m/ha", "%", "%", "%"))  %>%
    dplyr::relocate("Unit", .after = "Parameter")
}

tbl_locs <- function(slp_cal, lsf) {

  x5 <- slp_cal  %>%
    dplyr::select("type", "pl_c2s")  %>%
    tidyr::pivot_wider(names_from = "type", values_from = "pl_c2s")  %>%
    dplyr::bind_cols(
      lsf  %>%
        dplyr::select("name", "s_len")  %>%
        dplyr::filter(.data$name %in% c("10%", "20%", "25%", "30%", "40%",
                                        "50%", "60%", "70%", "75%", "80%", "90%")))
  # Add in the avg values from lstr2div
  x5 <- dplyr::slice(x5, 1)  %>%
    dplyr::mutate(s_len = lsf$lstr2div[2], name = "avg")  %>%
    dplyr::add_row(x5)  %>%
    dplyr::mutate(
      top = 0,
      cst = .data$top + .data$cst * .data$s_len,
      ups = .data$cst + .data$ups * .data$s_len,
      mid = .data$ups + .data$mid * .data$s_len,
      low = .data$mid + .data$low * .data$s_len,
      dep = .data$low + .data$dep * .data$s_len)  %>%
    dplyr::select(-"s_len")  %>%
    dplyr::rename_with(\(x) paste0(toupper(x), "_X"))  %>%
    dplyr::rename("Stat" = "NAME_X")  %>%
    dplyr::relocate("Stat", "Top_X" = "TOP_X")  %>%
    dplyr::mutate(Stat = dplyr::case_when(.data$Stat == "avg" ~ "Avg",
                                          .data$Stat == "50%" ~ "50% (Median)",
                                          TRUE ~ .data$Stat))

  x6 <- slp_cal  %>%
    dplyr::select("type", "pz_c2s")  %>%
    tidyr::pivot_wider(names_from = "type", values_from = "pz_c2s")  %>%
    dplyr::bind_cols(lsf  %>%
                       dplyr::select("name", "s_z")  %>%
                       dplyr::filter(.data$name %in% c("10%", "20%", "25%", "30%", "40%",
                                                       "50%", "60%", "70%", "75%", "80%", "90%")))

  # Add in the avg values from zcr2st
  x6 <- dplyr::slice(x6, 1)  %>%
    dplyr::mutate(s_z = lsf$zcr2st[2], name = "avg")  %>%
    dplyr::add_row(x6)  %>%
    dplyr::mutate(
      top = .data$s_z,
      top = dplyr::if_else(.data$name == "avg",
                           lsf$zcr2st[lsf$name == "avg"],
                           .data$top),
      cst = .data$top - .data$cst * .data$s_z,
      ups = .data$cst - .data$ups * .data$s_z,
      mid = .data$ups - .data$mid * .data$s_z,
      low = .data$mid - .data$low * .data$s_z,
      dep = .data$low - .data$dep * .data$s_z)  %>%
    dplyr::select(-"s_z")  %>%
    dplyr::rename_with(\(x) paste0(toupper(x), "_Z"))  %>%
    dplyr::rename("Stat" = "NAME_Z")  %>%
    dplyr::relocate("Stat", "Top_Z" = "TOP_Z")  %>%
    dplyr::mutate(Stat = dplyr::case_when(.data$Stat == "avg" ~ "Avg",
                                          .data$Stat == "50%" ~ "50% (Median)",
                                          TRUE ~ .data$Stat))

  # Combine x5 and x6 into one table
  dplyr::left_join(x5, x6, by = "Stat")
}


get_edge <- function(x, type) {
  type <- paste0("edge_", type, " = ")
  get_detail(x, type, pattern = "[0-9]{1,}")  %>%
    as.numeric()
}

get_detail <- function(x, type, pattern = ".+") {
  stringr::str_subset(x, type)  %>%
    stringr::str_extract(paste0("(?<=", type, ")", pattern))
}
