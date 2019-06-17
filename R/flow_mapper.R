#' Map flow through the landscape
#'
#' Run an elevation file through all functions to calculate watershed flow and
#' fill patterns. Based on FlowMapR by R. A. (Bob) MacMillan, LandMapper
#' Environmental Solutions.
#'
#' @param file Character. Elevation file (see \code{\link{load_file}} for
#'   supported file types.
#' @param nrow Numeric. Number of rows in dem file (required for dbf files with
#'   a single column, but can be automatically assessed from files with x and y
#'   coordinates.
#' @param ncol Numeric. Number of columns in dem file (required for dbf files
#'   with a single column, but can be automatically assessed from files with x
#'   and y coordinates.
#' @param missing_value Numeric/Character. Symbols which define missing data
#' @param max_area Numeric. Largest area of pits to be removed during initial
#'   pit removal
#' @param max_depth Numeric. Largest depth of pits to be removed during initial
#'   pit removal
#' @param folder_out Charater. Folder to store output files. Defaults to folder
#'   in the same location and with the same name as the dem file
#' @param clean Logical. Remove all backup files and output files from previous
#'   runs in this folder?
#' @param clim Numeric vector. Column limits if specifying a subset of the dem
#' @param rlim Numeric vector. Row limits if specifying a subset of the dem
#'
#' @inheritParams args
#'
#' @details For information regarding loading other file types see
#'   \code{\link{load_file}}.
#'
#'   For resuming or ending a run, \code{resume} or \code{end} must be one of
#'   the following:
#'
#'   \enumerate{
#'     \item \code{directions} (Calculating Directions)
#'     \item \code{watersheds} (Calculating Watersheds)
#'     \item \code{local} (Initial Pit Removal)
#'     \item \code{pond} (Calculating Pond Shed Statistics - Second Pit Removal)
#'     \item \code{fill} (Calculating Fill Shed Statistics - Third Pit Removal)
#'     \item \code{inverted} (Calculating Directions on Inverted DEM)
#'     \item \code{iwatersheds} (Calculating Inverted Watersheds)
#'     \item \code{ilocal} (Initial Inverted Pit Removal)
#'     \item \code{report} (Create the final report)
#'   }
#'
#' @examples
#' # Basic Run
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'             folder_out = "./testELEV/", nrow = 150, ncol = 150)
#'
#' # Specify parameters for initial pit removal
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'             folder_out = "./testELEV/", nrow = 150, ncol = 150,
#'             max_area = 5, max_depth = 2)
#'
#' # Clean up (remove created folder and output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#'
#' @export
flow_mapper <- function(file, nrow = NULL, ncol = NULL, missing_value = -9999,
                        max_area = 10, max_depth = 0.5,
                        folder_out = NULL, clean = FALSE,
                        clim = NULL, rlim = NULL,
                        resume = NULL, end = NULL,
                        log = TRUE, report = TRUE,
                        verbose = FALSE, quiet = FALSE) {

  if(quiet) verbose <- FALSE

  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""

  cont_options <- c("", "directions", "watersheds",
                    "local", "pond", "fill",
                    "inverted", "iwatersheds",
                    "ilocal", "report")

  cont_reg <- c("", "directions", "watersheds", "local", "pond", "fill")

  if(!(resume %in% cont_options) | !(end %in% cont_options)) {
    stop("resume/end must be one of '",
         paste0(cont_options[-1], collapse = "', '"), "'", call. = FALSE)
  }

  # Check for files
  m <- list.files(dirname(file), pattern = basename(file), ignore.case = TRUE)
  if(length(m) == 0) stop("Cannot find starting elevation file: ", file, call. = FALSE)
  if(length(m) > 1) stop("More than one match found (note that case ignored) for starting elevation file: ", file, "\nMatches: ", paste0(m, collapse = ", "), call. = FALSE)

  f <- tools::file_path_sans_ext(basename(file))

  if(is.null(folder_out)) folder_out <- dirname(file)
  if(!dir.exists(folder_out)) dir.create(folder_out)

  out_locs <- locs_create(folder_out, f)

  # Setup Log
  if(log) log_file <- paste0(folder_out, "/", f, ".log")

  # Clean records
  if(clean) lapply(out_locs, function(x) file.remove(list.files(x,
                                                                full.names = TRUE)))

  if(log && file.exists(log_file)) file.remove(log_file)

  start <- Sys.time()

  db_start <- load_file(file, nrow = nrow, ncol = ncol,
                        missing_value = missing_value,
                        clim = clim, rlim = rlim, verbose = verbose)

  ncol_orig <- ncol
  nrow_orig <- nrow
  ncol <- max(db_start$col) - 2
  nrow <- max(db_start$row) - 2

  if(log) {
    # File details to log
    write("Run options:", log_file)
    write(paste0("  Dimensions: nrows = ", nrow_orig, "; ncols = ", ncol_orig),
          file = log_file, append = TRUE)

    # Subset to log
    if(!is.null(clim) || !is.null(rlim)) {
      write(paste0("  Subset: rows ", rlim[1], "-", rlim[2],
                   "; cols ", clim[1], "-", clim[2]),
            log_file, append = TRUE)
    }

    # Run start to log
    write(paste0("\nRun started: ", start, "\n"),
          file = log_file, append = TRUE)

    if(is.null(nrow_orig)) {
      write(paste0("  Dimensions detected: nrows = ", nrow,
                   "; ncols = ", ncol, "\n"),
            file = log_file, append = TRUE)
    }
  }

  # Calculate directions -------------------------------------------------------
  if(!quiet) message("CALCULATING DIRECTIONS")
  if(resume == "" | resume == "directions") {
    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating directions at: ", sub_start), file = log_file, append = TRUE)
    db_dir <- calc_ddir2(db_start, verbose = verbose)
    save_all(locs = out_locs, data = list("db" = db_dir), name = "dir")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)
  } else  {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping directions\n", log_file, append = TRUE)
  }

  if(end == "directions") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }


  # Calculate watersheds -------------------------------------------------------
  if(!quiet) message("CALCULATING WATERSHEDS")
  if(resume == "watersheds") db_dir <- read_shed(out_locs$backup_out, "backup_dir")
  if(resume %in% c("", "watersheds")) {

    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating watersheds at: ", sub_start), file = log_file, append = TRUE)

    db_initial <- list()
    db_initial$db <- calc_shed4(db_dir)

    db_initial$stats <- pit_stat1(db_initial$db) %>%
      out_stat()

    # Calc stats for first vol2fl
    db_initial$db <- calc_vol2fl(db = db_initial$db,
                                 i_stats = db_initial$stats,
                                 verbose = verbose)

    # Save
    save_all(locs = out_locs, data = list("db" = db_initial$db, "stats" = db_initial$stats), name = "initial")

    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)
  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping watersheds\n", log_file, append = TRUE)
  }

  if(end == "watersheds") {
    if(!quiet) run_time(start, log, log_file)
    return(db_initial)
  }

  # Remove initial pits --------------------------------------------------------
  if(!quiet) message("REMOVING INITIAL PITS")
  if(resume == "local") db_initial <- read_shed(out_locs$backup_out, "backup_initial")
  if(resume %in% c("", "watersheds", "local")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started removing initial pits at: ", sub_start), file = log_file, append = TRUE)

    # Pit removal
    db_local <- first_pitr1(db_initial$db, max_area = max_area, max_depth = max_depth, verbose = verbose)

    # Stats
    stats_local <- pit_stat1(db_local) %>%
      out_stat()

    db_local <- list("db" = db_local, "stats" = stats_local)
    save_all(locs = out_locs, data = db_local, name = "local")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)
  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping initial pit removal\n", log_file, append = TRUE)
  }

  if(end == "local") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }

  # Calc pond Sheds ---------------------------------------------------------
  if(!quiet) message("CALCULATING POND (GLOBAL) WATERSHEDS")
  if(resume == "pond") db_local <- read_shed(out_locs$backup_out, "backup_local")
  if(resume %in% c("", "watersheds", "local", "pond")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating pond watersheds at: ", sub_start), file = log_file, append = TRUE)
    if(length(unique(db_local$db$shedno[!is.na(db_local$db$shedno)])) > 1){
      db_pond <- second_pitr1(db_local$db, verbose = verbose) #also 2nd vol2fl and parea
    } else {
      if(!quiet) message("  Only a single watershed: No pond outputs")
      db_pond <- list()
      db_pond$db <- dplyr::mutate(db_local$db, pond_shed = local_shed)
      db_pond$stats <- tibble::tibble()
    }
    save_all(locs = out_locs, data = list("db" = db_pond$db, "stats" = db_pond$stats), name = "pond")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)
  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping pond watersheds\n", log_file, append = TRUE)
  }

  if(end == "pond") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }

  # Calc fill sheds ---------------------------------------------------------
  if(resume == "fill") {
    db_initial <- read_shed(out_locs$backup_out, "backup_initial")
    db_local <- read_shed(out_locs$backup_out, "backup_local")
    db_pond <- read_shed(out_locs$backup_out, "backup_pond")
  }

  if(!quiet) message("CALCULATING FILL PATTERNS")
  if(resume %in% c("", "watersheds", "local", "pond", "fill")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating fill watersheds at: ", sub_start), file = log_file, append = TRUE)
    if(length(unique(db_local$db$shedno[!is.na(db_local$db$shedno)])) > 1){

      # Add pond sheds details to local sheds
      db_local$db[, c("pond_shed", "vol2fl", "mm2fl", "parea")] <-
        db_pond$db[, c("pond_shed", "vol2fl", "mm2fl", "parea")]

      db_fill <- third_pitr1(db_local$db, verbose = verbose) # calc 2nd mm2fl as progresses

    } else {
      if(!quiet) message("  Only a single watershed: No fill outputs")
      db_fill <- list()
      db_fill$db <- dplyr::mutate(db_pond$db, fill_shed = local_shed,
                                  vol2fl = 0, mm2fl = 0, parea = 0)
      db_fill$stats <- tibble::tibble()
    }

    save_all(locs = out_locs, data = list("db" = db_fill$db, "stats" = db_fill$stats), name = "fill")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)

    if(nrow(db_fill$stats) > 0) {
      # Create PIT file
      pit <- db_fill$stats %>%
        dplyr::filter(final == TRUE) %>%
        dplyr::mutate(edge_pit = FALSE) %>%
        dplyr::arrange(shedno)

      save_all(locs = out_locs, data = list("db" = db_fill$db, "stats" = pit), name = "pit")
    } else if(!quiet) message("  Only a single watershed: No pit outputs")

  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping fill watersheds\n", log_file, append = TRUE)
  }

  if(end == "fill") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }

  # Inverted DEM --------------------------------------------------------------

  if(!quiet) message("INVERTING DEM")
  if(!quiet) message("CALCULATING INVERTED DIRECTIONS")

  if(resume %in%  c(cont_reg, "inverted")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating inverted directions at: ", sub_start), file = log_file, append = TRUE)

    db_invert <- read_shed(out_locs$backup_out, "backup_local")$db %>%
      dplyr::select(elev, seqno, row, col, missing, buffer, elev_orig, edge_map) %>%
      invert()

  # Inverted Directions --------------------------------------------------------

    db_idir <- calc_ddir2(db_invert, verbose = verbose)
    save_all(locs = out_locs, data = list("db" = db_idir), name = "idir")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)

  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping inverted directions\n", log_file, append = TRUE)
  }

  if(end == "idirections") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }

  # Inverted Watersheds --------------------------------------------------------
  if(!quiet) message("CALCULATING INVERTED WATERSHEDS")
  if(resume == "iwatersheds") db_idir <- read_shed(out_locs$backup_out, "backup_idir")
  if(resume %in% c(cont_reg, "inverted", "iwatersheds")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started calculating inverted watersheds at: ", sub_start), file = log_file, append = TRUE)
    db_iinitial <- calc_shed4(db_idir)
    save_all(locs = out_locs, data = list("db" = db_iinitial), name = "iinitial")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)
  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping inverted watersheds\n", log_file, append = TRUE)
  }

  if(end == "iwatersheds") {
    if(!quiet) run_time(start, log, log_file)
    return()
  }


  # Invert Remove Initial Pits -----------------------------------------------
  if(!quiet) message("REMOVING INVERTED INITIAL PITS")
  if(resume == "ilocal") db_iinitial <- read_shed(out_locs$backup_out, "backup_iinitial")
  if(resume %in% c(cont_reg, "inverted", "iwatersheds", "ilocal")) {
    sub_start <- Sys.time()
    if(log) write(paste0("Started inverted pit removal at: ", sub_start), file = log_file, append = TRUE)

    db_ilocal <- first_pitr1(db_iinitial, max_area = max_area, max_depth = max_depth, verbose = verbose)
    db_ilocal$pond_shed <- db_ilocal$local_shed

    if(length(unique(db_ilocal$shedno[!is.na(db_ilocal$shedno)])) > 1) {
      ipit <- pit_stat1(db_ilocal) %>%
        out_stat() %>%
        dplyr::mutate(edge_pit = FALSE)
    } else ipit <- tibble::tibble()

    save_all(locs = out_locs, data = list("db" = db_ilocal, "stats" = ipit), name = "ilocal")
    if(log) write(paste0("  Total time: ", round(difftime(Sys.time(), sub_start, units = "min"), 2), "\n"), file = log_file, append = TRUE)

  } else {
    if(!quiet) message("  Skipping")
    if(log) write("Skipping inverted pit removal\n", log_file, append = TRUE)
  }


  # Final Report ------------------------------------------------------------
  if(resume %in% c(cont_reg, "inverted", "iwatersheds", "ilocal", "report")) {
    if(report == TRUE){
      files <- normalizePath(list.files(path = paste0(folder_out, "/final"), full.names = TRUE))
      report_final(file = file, report_loc = folder_out, out_files = files, run = f, nrow = nrow, ncol = ncol,
                   max_area = max_area, max_depth = max_depth, rlim = rlim, clim = clim)
    }
  }

  if(!quiet) run_time(start, log, log_file)

}

run_time <- function(start, log, log_file) {
  stop <- Sys.time()
  runtime <- round(difftime(stop, start, units = "min"), 2)
  message("Run took: ", runtime, " min")
  if(log) write(paste0("Total run time: ", runtime, " min\n"), log_file, append = TRUE)
}
