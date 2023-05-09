#' @examples
#'
#' summary_tables()
#'
summary_tables <- function(folder, testing = TRUE) {

  # TESTING
  if(testing) {
    t <- test_files()
    pnts <- t$pnts
    facet <- t$facet
    avg <- t$avg
  } else {
    pnts <- all_points(folder)
    facet <- get_previous(folder, step = "fuzc", where = "facet")
    avg <- readxl::read_excel(file.path(folder, "topographic_derivatives.xlsx")) |>
      dplyr::filter(name == "avg")
  }

  log <- readr::read_lines(list.files(folder, "facet.log", full.names = TRUE))
  edge_row <- get_edge(log, "row")
  edge_col <- get_edge(log, "col")
  nrows <- max(pnts$row)
  ncols <- max(pnts$col)

  le <- dplyr::select(facet, "row", "col", "seqno", "max_facet") |>
    omit_edges(edge_row = edge_row, edge_col = edge_col,
               nrow = nrows, ncol = ncols)

  # Seg-Cal
  avg_le <- le5_avg(pnts, le)

  # Bdr-Cal
  ix <- ix_avgs(pnts, facet, edge_row, edge_col, nrows, ncols)

  # PntCounts
  cnts <- pnts_count(pnts, le)

  # Average calcs
  x <- mid_calc(ix, cnts, avg, avg_le)

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

    #TODO: Calculate averages based on different number of edge rows

    avg <- readxl::read_excel(file.path(folder, "..", "BR3_1m_20210427.xlsx"),
                              sheet = "PercentileAccu", skip = 1) |>
      janitor::clean_names() |>
      dplyr::filter(name == "Average") |>
      dplyr::rename(slope_pct = slope) |>
      dplyr::select(lstr2div, zcr2st) |>
      dplyr::mutate(lstr2div = 88.8773188000762,
                    zcr2st = 4.12803403202703) # TODO: Overwriting right now but fix as above

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

    facet <- read_tsv(paste0(d, "fuzc.txt")) |>
      janitor::clean_names() |>
      dplyr::left_join(dplyr::select(pnts, seqno, row, col), by = "seqno")

    form1 <- read_tsv(paste0(d, "Relz.txt")) |>
      janitor::clean_names() |>
      dplyr::left_join(dplyr::select(pnts, seqno, row, col), by = "seqno") |>
      dplyr::mutate(l2pit = sqrt((row - pit_row)^2 + (col - pit_col)^2) * cellsize,
                    l2peak = sqrt((row - pk_row)^2 + (col - pk_col)^2) * cellsize,
                    l2str = sqrt((row - st_row)^2 + (col - st_col)^2) * cellsize,
                    l2div = sqrt((row - cr_row)^2 + (col - cr_col)^2) * cellsize,
                    lpit2peak = l2pit + l2peak,
                    lstr2div = l2str + l2div,
                    ppit2peakl = dplyr::if_else(lpit2peak <= 0, 0, l2pit / lpit2peak * 100),
                    pstr2divl = dplyr::if_else(lstr2div <= 0, 0, l2str/lstr2div * 100)) |>
      dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~round(.x, 3))) |>
      dplyr::select(-row, -col) |>
      dplyr::rename(peak_row = pk_row, peak_col = pk_col, peak_elev = pk_elev) |>
      dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~dplyr::na_if(.x, -9999)))

    form2 <- read_tsv(paste0(d, "Form.txt")) |>
      janitor::clean_names() |>
      dplyr::rename(slope_pct = slope, qarea1 = qarea, qweti1 = qweti)


    pnts <- dplyr::left_join(pnts, form1, by = "seqno") |>
      dplyr::left_join(form2, by = "seqno")

    # pnts <- readr::read_csv(file.path(folder, "Pnt_AllData.txt")) |>
    #   janitor::clean_names() |>
    #   dplyr::rename(seqno = seq_no) |>
    #   dplyr::rename(inv_drec = i_drec, peak_row = pk_row, peak_col = pk_col, peak_elev = pk_elev,
    #                 slope_pct = slope, qarea1 = a_qarea, qweti1 = a_qweti)
  })

  #waldo::compare(form1$l2pit, form2$l2pit, tolerance = 0.1)



  list("facet" = facet, "pnts" = pnts, "avg" = avg)
}
