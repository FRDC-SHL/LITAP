load_dem <- function(file, nrow, ncol, missing_value = -9999) {

  # Load
  db <- foreign::read.dbf(file)

  # Check if valid rows/cols
  if(nrow * ncol != length(db$ELEV)){
    stop("Number of rows and columns does not match the total number of cells in the data base, Try again!")
  }

  # Arrange as grid
  db <- tibble::data_frame(elev = db$ELEV,
                           seqno = 1:length(elev),
                           row = sort(rep(1:nrow, length(elev)/nrow)),
                           col = rep(1:ncol, length(elev)/ncol),
                           missing = elev == missing_value) %>%
    dplyr::mutate(elev = replace(elev, missing, NA))

  return(db)
}

load_xyz <- function(file) {
}

load_gridfile <- function(file) {
}

load_tiff <- function(file) {
}

load_surfer <- function(file) {
}

load_file <- function(file, nrow, ncol, missing_value = -9999, clim = NULL, rlim = NULL) {

  # Add in options for different file types
  # - Automatically detect by file extension
  # - Option to override

  load_dem(file, nrow, ncol, missing_value) %>%
   prep_db(clim = clim, rlim = rlim)
}

prep_db <- function(db, clim, rlim) {

  # Subset
  if(!is.null(clim) || !is.null(rlim)) {
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

  ##### Add edges

  # Surround in impossible elevation
  db <- add_buffer(db) %>%
    dplyr::arrange(seqno) %>%
    dplyr::mutate(elev_orig = elev) # make a backup of the original elevation data

  # Note which cells are edge cells
  db <- db %>%
    nb_values(max_cols = max(db$col), col = c("missing", "buffer")) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(edge_map = any(buffer_n) | any(missing_n)) %>%
    dplyr::right_join(db, by = "seqno")

  return(db)
}

add_buffer <- function(db) {
  ncols = max(db$col)
  nrows = max(db$row)

  buffers <- tibble::data_frame(row = c(rep(1, ncols+2),        #top
                                        1:(nrows+2),            #left
                                        1:(nrows+2),            #right
                                        rep(nrows+2, ncols+2)), #bottom
                                col = c(1:(ncols+2),            #top
                                        rep(1, nrows+2),        #left
                                        rep(ncols+2, nrows+2),  #right
                                        1:(ncols+2)),           #bottom
                                buffer = TRUE) %>%

    dplyr::distinct()

  db %>%
    dplyr::mutate(row = row + 1, col = col + 1, buffer = FALSE) %>%
    dplyr::bind_rows(buffers) %>%
    dplyr::arrange(row, col) %>%
    dplyr::mutate(seqno = 1:length(row))
}
