#' Load and prep elevation data
#'
#' This function is used by the `mapper` functions to load input files and
#' prepare them for analysis. It can also be used to load input files
#' independently for plotting with \code{\link{flow_plot}} and/or
#' trouble-shooting.
#'
#' @param file Character. The location of the file containing elevation data.
#'   See details for accepted file types
#' @param nrow Numeric. Number of rows in dem file (required for dbf files with
#'   a single column, but can be automatically assessed from files with x and y
#'   coordinates.
#' @param ncol Numeric. Number of columns in dem file (required for dbf files
#'   with a single column, but can be automatically assessed from files with x
#'   and y coordinates.
#' @param missing_value Vector. The number or character string specifying
#'   missing data.
#' @param rlim Vector. Two numbers specifying the start and end of a subset of
#'   rows to extract
#' @param clim Vector. Two numbers specifying the start and end of a subset of
#'   columns to extract
#' @param edge Logical. Whether to add an edge (buffer) around the data.
#'
#' @inheritParams args
#'
#' @return Returns a data frame containing elevation data in a format suitable
#'   for analysis
#'
#' @details All x/y data must be in a format such that greater values indicate
#'   East or North respectively.
#'
#' This function uses file extensions to guess the file type to be loaded.
#'
#' \strong{dBase files:}
#' These files are loaded via the \code{\link[foreign]{read.dbf}} function from
#' the foreign package. Columns must be named and must have a valid name (case
#' doesn't not matter). X/Y coordinates are optional and must be named as "x",
#' "lon", "long", "longitude", or "y", "lat", "latitude". Elevation columns can
#' be "elev", "elevation", or "z". If no "x" and "y" columns are suppled,
#' \code{nrow} and \code{ncol} arguments must be supplied. Column names matter,
#' column order does not. Extra columns, if present, are ignored.
#' \itemize{
#'   \item dBase files (.dbf)
#' }
#'
#' \strong{Grid file types:}
#' These file types are loaded via the \code{\link[raster]{raster}} function.
#' \itemize{
#'   \item Surfer grid files (.grd)
#'   \item Esri grid files (binary .adf or ascii .asc)
#'   \item Geo Tiff (.tif)
#'   \item Floating point raster files (.flt) (**Note** the companion header,
#'   .hdr, file must also be present)
#'   }
#'
#' \strong{Text/Spreadsheet file types:}
#' Data in these files are all assumed to be arranged in three columns
#' reflecting x, y, and z dimensions (z = elevation). Column **order** is
#' important. Column names don't matter, but they should be present.
#' \itemize{
#'   \item Text files (.txt, .dat, .csv) are loaded via R base
#'   \code{link[utils]{read.table}} function.
#'   \item Excel files (.xls, .xlsx) are loaded via the
#'   \code{\link[readxl]{read_excel}} function.
#'   }
#'
#' @export
load_file <- function(file, nrow = NULL, ncol = NULL, missing_value = -9999,
                      rlim = NULL, clim = NULL, grid = NULL,
                      min_x = 1, min_y = 1, edge = TRUE, verbose = TRUE) {

  if(!file.exists(file)) stop("Cannot locate ", file,
                              " relative to working directory, ", getwd(),
                              call. = FALSE)
  ext <- tolower(tools::file_ext(file))

  if(ext %in% c("grd", "tif", "adf", "asc", "flt", "")) {
    if(ext == "" & verbose) message("  Assuming folder representing Arc/Info Binary Grid")
    db <- load_raster(file)
  } else if(ext == "dbf") {
    db <- load_dem(file, type = "elev")
  } else if(ext %in% c("xlsx", "xls")) {
    db <- load_excel(file, type = "elev")
  } else if(ext %in% c("txt", "csv", "dat")) {
    db <- load_txt(file, type = "elev")
  } else {
    stop("Unknown file format", call. = FALSE)
  }

  # Sort if x/y present
  if(ncol(db) > 1 && all(c("x", "y", "elev") %in% names(db))) {
    db <- dplyr::arrange(db, dplyr::desc(y), x)
    nrow <- length(unique(db$y))
    ncol <- length(unique(db$x))
    if(verbose) message("  Detected ", nrow, " rows and ", ncol, " columns")
  } else if(!is.null(nrow) && !is.null(ncol)) {
    if(verbose) message("  Using supplied ", nrow, " rows and ", ncol, " columns")
  } else {
    stop("dbf files with only one column require 'nrow' and 'ncol' arguments.",
         call. = FALSE)
  }

  db_format(db, nrow = nrow, ncol = ncol, grid = grid,
            min_x = min_x, min_y = min_y,
            missing_value = missing_value, verbose = verbose) %>%
    db_prep(clim = clim, rlim = rlim, edge = edge, verbose = verbose)
}


load_dem <- function(file, type = "elev") {
  if(!requireNamespace("foreign", quietly = TRUE)) {
    stop("Require package 'foreign' to load .dbf files.\n
         Install with \"install.packages('foreign')\", then try again")
  }

  # Load file
  foreign::read.dbf(file, as.is = TRUE) %>%
    check_names(type = type)
}

check_names <- function(db, type) {

  # Format names
  names(db) <- tolower(names(db))
  names(db) <- stringr::str_trim(names(db))

  all_names <- dplyr::filter(match_names, .data$type == !!type) %>%
    dplyr::pull("name")
  req_names <- dplyr::filter(match_names,
                             .data$type == !!type,
                             .data$required == TRUE) %>%
    dplyr::pull("name")

  # Fix fixable names
  if(!is.null(fix_names[[type]])) {
    names(db) <- stringr::str_replace_all(names(db), pattern = fix_names[[type]])
  }

  # Check if required columns present
  if(!all(req_names %in% names(db))) {
    stop("Required columns missing from data: '",
         paste0(m, collapse = "', '"), "'")
  }

  if(type == "elev") db <-  dplyr::mutate(db, elev = as.numeric(elev))

  # Grab names which are present
  dplyr::select(db, tidyselect::any_of(all_names))
}

load_excel <- function(file, type = "elev") {
  if(!requireNamespace("readxl", quietly = TRUE)) {
    stop("Require package 'readxl' to load .xlsx or .xls files.\n
         Install with \"install.packages('readxl')\", then try again")
  }
  h <- readxl::read_excel(file, n_max = 5,
                          col_names = FALSE, .name_repair = "minimal")
  header <- any(stringr::str_detect(h[1,], "[a-zA-Z]+"))

  db <- readxl::read_excel(file, col_names = header)
  if(!header) {
    names(db) <- match_names$name[match_names$type == type]
  } else {
    db <- check_names(db, type = type)
  }
  db
}

load_txt <- function(file, type = "elev") {
  t <- readLines(file, 10)
  header <- any(stringr::str_detect(t[1], "[a-zA-Z]+"))
  if(any(stringr::str_detect(t, ","))) sep <- "," else sep <- ""

  db <- utils::read.table(file, header = header, sep = sep)
  if(!header) {
    names(db) <- match_names$name[match_names$type == type]
  } else {
    db <- check_names(db, type = type)
  }
  dplyr::mutate_all(db, as.numeric)
}


load_raster <- function(file) {
  db <- raster::raster(file) %>%
    raster::as.data.frame(xy = TRUE)
  names(db) <- match_names$name[match_names$type == "elev"]
  db
}

db_format <- function(db, nrow, ncol, grid, min_x, min_y,
                      missing_value = -9999, verbose) {
  if(verbose) message("  Formating grid")
  # Check if valid rows/cols
  if(nrow * ncol != length(db$elev)){
    stop("Number of rows and columns does not match the total number of cells in the data base, Try again!")
  }

  # Arrange as grid
  db <- db %>%
    dplyr::mutate(seqno = 1:length(elev),
                  row = sort(rep(1:nrow, length(elev)/nrow)),
                  col = rep(1:ncol, length(elev)/ncol),
                  missing = elev == missing_value,
                  elev = replace(elev, missing, NA_real_)) %>%
    dplyr::mutate(dplyr::across(-"missing", as.numeric))

  if(any(!c("x", "y") %in% names(db))) {
    if(is.null(grid)) stop("No grid dimensions in data, require 'grid' argument",
                           call. = FALSE)
    if(verbose) message("  No x/y in file, creating x/y from cols/rows/grid")

    db <- dplyr::mutate(db,
                        x = col * grid + min_x - 1,
                        y = rev(row) * grid + min_y - 1)
  } else {
    # Check for regular grid
    g <- (max(db$x) - min(db$x) + 1) / ncol
    if(g != (max(db$y) - min(db$y) + 1) / nrow) {
      stop("Inconsistent grid size. Grid must be square", call. = FALSE)
    }
  }

  db
}

db_prep <- function(db, clim, rlim, edge, verbose) {

  # Subset
  if(!is.null(clim) || !is.null(rlim)) {
    if(verbose) message("  Subsetting data")
    if((!is.null(clim) & (!is.numeric(clim) | length(clim) != 2)) |
       (!is.null(rlim) & (!is.numeric(rlim) | length(rlim) != 2))) {
         stop("clim and rlim must be each be a vector of two numbers (start/end row/col) or NULL")
       }
    if(any(rlim > max(db$row)) | any(clim > max(db$col))) stop("Subset cannot be bigger than data")
    if(length(rlim[1]:rlim[2]) < 2 | length(clim[1]:clim[2]) < 2) stop("Subset is too small (less than 2x2)")
    db <- db %>%
      dplyr::filter(row >= rlim[1] & row <= rlim[2] & col >= clim[1] & col <= clim[2]) %>%
      dplyr::mutate(seqno = 1:length(seqno),
                    row = row - min(row) + 1,
                    col = col - min(col) + 1)
  }

  # Add edges
  if(edge) {

    if(verbose) message("  Adding buffer")
    # Surround in impossible elevation
    db <- add_buffer(db) %>%
      dplyr::arrange(seqno) %>%
      dplyr::mutate(elev_orig = elev) # make a backup of the original elevation data

    # Note which cells are edge cells
    db1 <- nb_values(db, max_cols = max(db$col), col = c("buffer", "missing"),
                     format = "wide")
    b_cols <- names(db1)[stringr::str_detect(names(db1), "buffer_n")]
    m_cols <- names(db1)[stringr::str_detect(names(db1), "missing_n")]
    db1$buffer_edge <- rowSums(db1[, b_cols], na.rm = TRUE) > 0
    db1$missing_edge <- rowSums(db1[, m_cols], na.rm = TRUE) > 0
    db1$edge_map <- rowSums(db1[, c("buffer_edge", "missing_edge")],
                            na.rm = TRUE) > 0

    db <- dplyr::left_join(db, db1[, c("seqno", "edge_map")], by = "seqno")
  }

  db
}

add_buffer <- function(db, stats = NULL) {

  if(!is.null(stats)) {
    if(!"buffer" %in% names(db)) stop("If adding buffer to 'stats', 'db' ",
                                      "must already have buffer", call. = FALSE)

    index <- dplyr::filter(db, !.data$buffer) %>%
      dplyr::select("seqno") %>%
      dplyr::mutate(seqno_orig = 1:dplyr::n())

    r <- stats %>%
      dplyr::mutate(dplyr::across(dplyr::matches("row|col"), ~ . + 1)) %>%
      dplyr::mutate(dplyr::across(dplyr::contains("seqno"), ~index$seqno[.])) %>%
      dplyr::mutate(dplyr::across(dplyr::contains("drec"), ~index$seqno[.]))

  } else {
    ncols <- max(db$col)
    nrows <- max(db$row)

    db <- dplyr::arrange(db, seqno)

    if("drec" %in% names(db)) {
      drec <- dplyr::select(db, "drec") %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(drec))
    }

    buffers <- tibble::tibble(row = c(rep(1, ncols+2),        #top
                                      1:(nrows+2),            #left
                                      1:(nrows+2),            #right
                                      rep(nrows+2, ncols+2)), #bottom
                              col = c(1:(ncols+2),            #top
                                      rep(1, nrows+2),        #left
                                      rep(ncols+2, nrows+2),  #right
                                      1:(ncols+2)),           #bottom
                              buffer = TRUE) %>%

      dplyr::distinct()

    db <- db %>%
      dplyr::mutate(row = row + 1, col = col + 1, buffer = FALSE) %>%
      dplyr::bind_rows(buffers) %>%
      dplyr::arrange(row, col) %>%
      dplyr::mutate(seqno_buffer = 1:length(row))

    if("drec" %in% names(db)) {
      drec <- dplyr::left_join(drec, dplyr::select(db, "seqno", "seqno_buffer"),
                               by = c("drec" = "seqno")) %>%
        dplyr::rename("drec_buffer" = "seqno_buffer")
      db <- dplyr::left_join(db, drec, by = "drec") %>%
        dplyr::select(-"drec", "drec" = "drec_buffer")
    }

    r <- db %>%
      dplyr::select(-"seqno", "seqno" = "seqno_buffer") %>%
      dplyr::select("seqno", dplyr::everything()) %>%
      dplyr::arrange(.data$seqno)
  }
  r
}

load_extra <- function(file, type) {

  if(!type %in% c("arule", "crule", "zone")) {
    stop("'type' must be either 'arule', 'crule', or 'zone'", call. = FALSE)
  }

  if(!file.exists(file)) stop(type, " file ('", file,
                              "') doesn't exist", call. = FALSE)

  ext <- tolower(tools::file_ext(file))

  if(ext == "dbf") {
    extra <- load_dem(file, type = type)
  } else if(ext %in% c("xlsx", "xls")) {
    extra <- load_excel(file)
  } else if(ext %in% c("txt", "csv", "dat")) {
    extra <- load_txt(file)
  } else {
    t <- dplyr::if_else(type == "zone", "zone", "rule")
    stop(paste0("Expecting ", t, " file as data base (dbf), Excel (xlsx, xls)\n",
                "       or text (txt, csv, dat)"), call. = FALSE)
  }
  extra
}

format_rule <- function(rule, type, quiet) {
  if(!type %in% c("arule", "crule")) {
    stop("'type' must be either 'arule' or 'crule'", call. = FALSE)
  }
  if(!quiet) message("Formatting ", type, " file")
  rule <- dplyr::mutate_if(rule, ~!is.numeric(.), tolower)

  msg <- vector()
  if(type == "arule") {
    if(any(rule$attr_in == "slope")) {
      msg <- c(msg, "  - Renaming 'slope' to 'slope_pct'")
    }

    rule <- dplyr::mutate(
      rule,
      attr_in = stringr::str_replace_all(
        attr_in,
        c("slope" = "slope_pct")))
  }
  if(!quiet) message(paste0(msg, collapse = "\n"))

  if(!"zone" %in% names(rule)) rule <- dplyr::mutate(rule, zone = 0)
  rule
}

seqno_to_buffer <- function(seqno, seqno_buffer) {
  seqno_buffer[seqno]
}

seqno_as_buffer <- function(seqno, db) {
  dplyr::mutate(db, seqno2 = seqno) %>%
    add_buffer() %>%
    dplyr::filter(.data$seqno2 == !!seqno) %>%
    dplyr::pull(.data$seqno)
}

seqno_from_buffer <- function(seqno_buffer, db) {
  dplyr::mutate(db, seqno2 = seqno) %>%
    add_buffer() %>%
    dplyr::filter(.data$seqno == !!seqno_buffer) %>%
    dplyr::pull(.data$seqno2)
}

