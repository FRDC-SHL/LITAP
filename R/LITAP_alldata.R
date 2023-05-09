all_peak <- function(points) {
  all_summary(points, type = "pit")
}

all_stream <- function(points) {
  all_summary(points, type = "stream")
}

all_pit <- function(points) {
  all_summary(points, type = "pit")
}

all_crest <- function(points) {
  all_summary(points, type = "crest")
}

all_summary <- function(source, type) {

  if(is.character(source)) {
    all_pnts <- all_points(source)
  } else {
    all_pnts <- source
  }
  t <- c("crest" = "cr", "stream" = "st", "pit" = "pit", "peak" = "peak")

  type_cols <- paste0(t[type], c("_row", "_col")) |>
    setNames(c("row", "col"))

  seq <- all_pnts |>
    dplyr::select(dplyr::all_of(type_cols)) |>
    dplyr::distinct() |>
    tidyr::drop_na()

  pnts <- dplyr::select(all_pnts, "seqno", "x", "y", "row", "col")

  pnts_type <- all_pnts |>
    dplyr::semi_join(seq, by = c("row", "col"))

  for(i in t[names(t) != type]) {
    pnts_type <- link_points(pnts_type, pnts, by = i)
  }

  pnts_type
}

link_points <- function(x, y, by) {
  by_cols <- paste0(by, c("_col", "_row"))
  y <- dplyr::rename_with(y, ~paste0(by, "_", .x))
  dplyr::inner_join(x, y, by = by_cols)
}

all_points <- function(folder) {

  if(!dir.exists(folder)) stop("Cannot find 'folder'", call. = FALSE)

  flow <- get_previous(folder, step = "fill", where = "flow")
  inv <- get_previous(folder, step = "inverted", where = "flow") %>%
    dplyr::select("seqno", "ddir", "drec", "upslope", "upslope_m",
                  "inv_initial_shed", "inv_local_shed", "edge_map") %>%
    dplyr::rename_with(.cols = -c("seqno", dplyr::contains("inv_")),
                       ~paste0("inv_", .))

  length <- get_previous(folder, step = "length", where = "form")
  weti <- get_previous(folder, step = "weti", where = "form")

  flow %>%
    dplyr::left_join(inv, by = "seqno") %>%
    dplyr::left_join(length,
                     by = c("seqno", "x", "y", "row", "col", "elev")) %>%
    dplyr::left_join(weti,
                     by = c("seqno", "x", "y", "row", "col",
                            "elev", "drec", "upslope"))
}
