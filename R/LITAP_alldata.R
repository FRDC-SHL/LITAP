all_peak <- function(folder = NULL, points = NULL, stats = NULL) {
  all_summary(folder, points, stats, type = "peak") |>
    dplyr::rename("pit_elev_2" = "pit_elev.x",
                  "pit_elev" = "pit_elev.y")
}

all_stream <- function(folder = NULL, points = NULL, stats = NULL) {
  all_summary(folder, points, stats, type = "stream") |>
    dplyr::select(-"pit_elev.y") |>
    dplyr::rename("pit_elev" = "pit_elev.x")
}

all_pit <- function(folder = NULL, points = NULL, stats = NULL) {
  all_summary(folder, points, stats, type = "pit") |>
    dplyr::select(-"pit_elev.y") |>
    dplyr::rename("pit_elev" = "pit_elev.x")
}

all_crest <- function(folder = NULL, points = NULL, stats = NULL) {
  all_summary(folder, points, stats, type = "crest") |>
    dplyr::select(-"pit_elev.y") |>
    dplyr::rename("pit_elev" = "pit_elev.x")
}

all_summary <- function(folder = NULL, points = NULL, stats = NULL, type) {

  # All Points
  if(is.null(points)) points <- all_points(folder)

  # Stats file
  if(is.null(stats)) {
    stats <- all_stats(folder, type = dplyr::if_else(type %in% c("peak"), "inverted", "pit"))
  }

  t <- c("crest" = "cr", "stream" = "st", "pit" = "pit", "peak" = "peak")

  type_cols <- paste0(t[type], c("_row", "_col")) |>
    setNames(c("row", "col"))

  seq <- points |>
    dplyr::select(dplyr::all_of(type_cols)) |>
    dplyr::distinct() |>
    tidyr::drop_na()

  pnts <- dplyr::select(points, "seqno", "x", "y", "row", "col")

  pnts_type <- points |>
    dplyr::semi_join(seq, by = c("row", "col"))

  for(i in t[names(t) != type]) {
    pnts_type <- link_points(pnts_type, pnts, by = i)
  }

  joinby <- setNames(c("pit_seqno", "pit_row", "pit_col"), nm = c("seqno", "row", "col"))

  pnts_type |>
    dplyr::left_join(stats, by = joinby) |>
    dplyr::mutate(pnt_mark = .env$type)
}

link_points <- function(x, y, by) {
  by_cols <- paste0(by, c("_col", "_row"))
  y <- dplyr::rename_with(y, ~paste0(by, "_", .x))
  dplyr::inner_join(x, y, by = by_cols)
}

all_points <- function(folder) {

  testing <- folder == "testing"
  if(testing) {
    message("Using test data for all_points()")
    flow <- test_points()
    inv <- test_inv()
    length <- test_length()
    form <- test_form()
    facet <- test_facet()
  } else {
    flow <- get_previous(folder, where = "flow", step = "fill")
    facet <- get_previous(folder, where = "facet", step = "fuzc")
    inv <- get_previous(folder, where = "flow", step = "inverted")
    length <- get_previous(folder, where = "form", step = "length")
    form <- get_previous(folder, where = "form", step = "form")
  }
  grid <- calc_grid(flow)

  flow <- dplyr::select(flow, -dplyr::any_of("missing"))
  facet <- dplyr::select(facet, "seqno", "max_facet", "max_value", "max_facet_name")

  inv <- inv %>%
    dplyr::select(dplyr::any_of(
      c("seqno", "ddir", "drec", "upslope", "upslope_m", "edge",
        "inv_initial_shed", "inv_local_shed", "edge_map"))) %>%
    dplyr::rename_with(.cols = -c("seqno", dplyr::contains("inv_")),
                       ~paste0("inv_", .))

  common <- c("x", "y", "row", "col", "elev", "drec", "upslope")
  length <- dplyr::select(length, -dplyr::any_of(common))
  form <- dplyr::select(form, -dplyr::any_of(common))

  all <- flow %>%
    dplyr::left_join(inv, by = "seqno") %>%
    dplyr::left_join(length, by = "seqno") %>%
    dplyr::left_join(form, by = "seqno") %>%
    dplyr::left_join(facet, by = "seqno") %>%
    dplyr::filter(!is.na(elev))

  # Adjustments (cf 100_qryAllpnts_AllData_090214)
  all %>%
    dplyr::mutate(
      l2pit =  sqrt((.data$row - .data$pit_row)^2 + (.data$col - .data$pit_col)^2) * .env$grid,
      l2peak = sqrt((.data$row - .data$peak_row)^2 + (.data$col - .data$peak_col)^2) * .env$grid,
      l2str =  sqrt((.data$row - .data$st_row)^2 + (.data$col - .data$st_col)^2) * .env$grid,
      l2div =  sqrt((.data$row - .data$cr_row)^2 + (.data$col - .data$cr_col)^2) * .env$grid,
      lpit2peak = .data$l2pit + .data$l2peak,
      lstr2div = .data$l2str + .data$l2div,
      ppit2peakl = dplyr::if_else(.data$lpit2peak <= 0, 0, .data$l2pit / .data$lpit2peak * 100),
      pstr2divl = dplyr::if_else(.data$lstr2div <= 0, 0, .data$l2str/.data$lstr2div * 100))
}


all_stats <- function(folder, type) {

  testing <- folder == "testing"
  if(testing) {
    message("Using test (stats) data for all_points()")
    if(type == "pit") stats <- test_stats()
    if(type == "inverted") stats <- test_stats_inv()
  } else {
    stats <- get_previous(folder, where = "flow", step = type, type = "stats")
  }

  stats |>
    dplyr::rename_with(\(x) stringr::str_replace(x, "(shedno|edge)", paste0(type, "_\\1")))
}
