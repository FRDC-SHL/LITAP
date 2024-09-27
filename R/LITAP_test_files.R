
# Load test dem (points) files for summary tables
test_points <- function() {
  test_files("dem.dbf") |>
    foreign::read.dbf() |>
    janitor::clean_names() |>
    dplyr::rename(seqno = seq_no, upslope = up_slope,
                  local_shed = shed_no, fill_shed = shed_now, parea = p_area,
                  edge_map = edge) |>
    dplyr::mutate(x = col * test_meta()$grid + test_meta()$min_x - 1,
                  y = rev(row) * test_meta()$grid + test_meta()$min_y - 1,
                  elev = round(elev, 3),
                  elev = dplyr::na_if(elev, -9999),
                  seqno = as.integer(seqno),
                  row = as.double(row),
                  col = as.double(col)) |>
    dplyr::as_tibble()
}

# Load test idem (inverted points) files for summary tables
test_inv <- function() {

  pnts <- test_points()

  test_files("idem.dbf") |>
    foreign::read.dbf() |>
    janitor::clean_names() |>
    dplyr::rename(seqno = seq_no, upslope = up_slope,
                  inv_initial_shed = shed_no,
                  inv_local_shed = shed_now,
                  edge_map = edge) |>
    dplyr::left_join(dplyr::select(pnts, seqno, x, y), by = "seqno") |>
    dplyr::mutate(elev = round(elev, 3),
                  elev = dplyr::na_if(elev, -9999),
                  seqno = as.integer(seqno),
                  row = as.double(row),
                  col = as.double(col))
}

# Load test topographic summary files for summary tables
test_topo <- function() {
  test_files("../BR3_1m_20210427.xlsx", id = NULL) |>
    readxl::read_excel(sheet = "PercentileAccu", skip = 1, progress = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(name = tolower(name),
                  name = dplyr::case_when(name == "stdev" ~ "sd",
                                          name == "median" ~ "50%",
                                          name == "average" ~ "avg",
                                          TRUE ~ name)) |>
    dplyr::rename(qarea1 = a_qarea, qweti1 = a_qweti)
}

# Load test fuzc files for summary tables
test_facet <- function() {

  pnts <- test_points()

  test_files("fuzc.txt") |>
    readr::read_tsv(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::rename(max_value = fac4) |>
    dplyr::mutate(max_facet_name = "") |>
    # Add rows/cols which not in original (but are in LITAP output)
    dplyr::left_join(dplyr::select(pnts, elev, seqno, row, col, x, y), by = "seqno")
}

# Load test relz files for summary tables
test_length <- function() {
  pnts <- test_points()

  test_files("Relz.txt") |>
    readr::read_tsv(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    # Add rows/cols which not in original (but are in LITAP output)
    dplyr::left_join(dplyr::select(pnts, seqno, row, col), by = "seqno") |>
    dplyr::rename(peak_row = pk_row, peak_col = pk_col, peak_elev = pk_elev) |>
    # Fix digits and missing
    dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~round(.x, 3))) |>
    dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~dplyr::na_if(.x, -9999)))
}

# Load test form files for summary tables
test_form <- function() {
  test_files("Form.txt") |>
    readr::read_tsv(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::rename(slope_pct = slope, qarea1 = qarea, qweti1 = qweti) |>
    # Fix digits and missing
    dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~round(.x, 3))) |>
    dplyr::mutate(dplyr::across(dplyr::contains('elev'), ~dplyr::na_if(.x, -9999)))
}

# Load test stats (inverted) files for summary tables
test_stats_inv <- function() {
  test_files("ipit.dbf") |>
    foreign::read.dbf() |>
    janitor::clean_names() |>
    dplyr::rename("shedno" = "shed_no", "edge_pit" = "edge", "pit_seqno" = "pit_rec") |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("elev"),
                    \(x) round(x, 3) |> dplyr::na_if(-9999)))
}

# Load test stats (pit) files for summary tables
test_stats <- function() {
  test_files("pit.dbf") |>
    foreign::read.dbf() |>
    janitor::clean_names() |>
    dplyr::rename("shedno" = "shed_no", "edge_pit" = edge, "pit_seqno" = "pit_rec") |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("elev"),
                    \(x) round(x, 3) |> dplyr::na_if(-9999)))
}

# Load test file parameters for summary tables
test_meta <- function() {
  list(
    folder = "~/Dropbox/LITAP files/LandMapR_BR3Raw_20210427/LandMapR_Files/",
    id = "31M",
    min_x = 2415575, min_y = 7493199,
    edge_row = 15, edge_col = 9,
    edge_row_ws = 6, edge_col_ws = 4,
    grid = 1)
}

test_log <- function() {
  c("Run options:",
    "",
    "  Input folder = /home/steffi/Projects/Business/LandmapR/Runs - LITAP/BR3",
    "  arule file =  Derived (see topographic_derivatives.xlsx)",
    "  crule file = /home/steffi/Dropbox/LITAP files/LandMapR_BR3Raw_20210427/LandMapR_Files/C7rule.dbf",
    "  edge_row = 15 (5%)",
    "  edge_col = 9 (5%)",
    "  Procedure = lsm",
    "",
    "",
    "Run started: 2023-05-09 14:54:37.231962",
    "",
    "Started calculating fuzzy attributes at: 2023-05-09 14:54:37.232566",
    "  Total time: 0",
    "Started calculating classes at: 2023-05-09 14:54:37.489439",
    "  Total time: 0.02",
    "",
    "Total run time: 0.02 min")
}

# Grab test file path
test_files <- function(file, id) {

  if(missing(id)) id <- test_meta()$id
  if(!is.null(id)) {
    d <- paste0(test_meta()$folder, "/", id, file)
  } else d <- file.path(test_meta()$folder, file)
  d
}
