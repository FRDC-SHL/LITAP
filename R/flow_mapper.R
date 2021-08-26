#' Map flow through the landscape
#'
#' Run an elevation file through all functions to calculate watershed flow and
#' fill patterns. Based on FlowMapR by R. A. (Bob) MacMillan, LandMapper
#' Environmental Solutions.
#'
#' @param file Character. Elevation file (see \code{\link{load_file}}) for
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
#' @param out_folder Character. Folder in which to store output files. Defaults
#'   to folder in the same location and with the same name as the dem file
#' @param out_format Character. What format should the data be output as? "rds"
#'   for R data format (default), "csv" for Comma-separated values, or "dbf" for
#'   dbf database files. This format is used for all subsequent functions (i.e.
#'   `form_mapper()`, `facet_mapper()` and `wepp_mapper()`.
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
#'     \item \code{slope} (Slope Gradient and Curvature values)
#'     \item \code{idirections} (Calculating Directions on Inverted DEM)
#'     \item \code{iwatersheds} (Calculating Inverted Watersheds)
#'     \item \code{inverted} (Inverted Pit Removal)
#'     \item \code{report} (Create the final report)
#'   }
#'
#' @examples
#' # Basic Run
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 90, ncol = 90)
#'
#' # Specify parameters for initial pit removal
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'             out_folder = "./testELEV/", nrow = 90, ncol = 90,
#'             max_area = 5, max_depth = 2)
#'
#' # Clean up (remove created folder and output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#' @export
flow_mapper <- function(file, nrow, ncol, grid = NULL, missing_value = -9999,
                        max_area = 10, max_depth = 0.5,
                        out_folder = NULL, out_format = "rds", clean = FALSE,
                        clim = NULL, rlim = NULL,
                        resume = NULL, end = NULL, log = TRUE, report = TRUE,
                        verbose = FALSE, quiet = FALSE, debug = FALSE) {

  check_out_format(out_format)

  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "directions", "watersheds",
                      "local", "pond", "fill", "slope",
                      "idirections", "iwatersheds",
                      "inverted", "report")
  check_resume(resume, end, resume_options)

  if(quiet) verbose <- FALSE

  # Check for files
  m <- list.files(dirname(file), pattern = basename(file), ignore.case = TRUE)
  if(length(m) == 0) stop("Cannot find starting elevation file: ", file, call. = FALSE)
  if(length(m) > 1) stop("More than one match found (note that case ignored) for starting elevation file: ", file, "\nMatches: ", paste0(m, collapse = ", "), call. = FALSE)

  f <- tools::file_path_sans_ext(basename(file))

  if(is.null(out_folder)) out_folder <- file.path(dirname(file), f)
  if(!dir.exists(out_folder)) dir.create(out_folder)
  folder <- out_folder
  out_locs <- locs_create(folder, which = "flow", clean = clean)

  # Setup Log
  log_file <- log_setup(folder, which = "flow", log)

  start <- Sys.time()

  db_start <- load_file(file, nrow = nrow, ncol = ncol, grid = grid,
                        missing_value = missing_value,
                        clim = clim, rlim = rlim, verbose = verbose)

  if(is.null(grid)) grid <- calc_grid(db_start)

  ncol_orig <- ncol
  nrow_orig <- nrow
  ncol <- max(db_start$col) - 2
  nrow <- max(db_start$row) - 2

  # File details to log
  log_write("Run options:", log = log_file)
  log_write("  Dimensions: nrows = ", nrow_orig,
            "; ncols = ", ncol_orig,
            "; grid = ", grid,
            "; max_area = ", max_area,
            "; max_depth = ", max_depth,
            log = log_file)

  # Subset to log
  if(!is.null(clim) || !is.null(rlim)) {
    log_write("  Subset: rows ", rlim[1], "-", rlim[2],
              "; cols ", clim[1], "-", clim[2], log = log_file)
  }

  # Run start to log
  log_write("\nRun started: ", start, "\n", log = log_file)

  if(is.null(nrow_orig)) {
    log_write("  Dimensions detected: nrows = ", nrow, "; ncols = ", ncol, "\n",
              log = log_file)
  }

  # Calculate directions -------------------------------------------------------

  task <- "calculating directions"
  if(resume == "" || resume == "directions") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    db_dir <- calc_ddir2(db_start, verbose = verbose)

    save_output(data = db_dir, name = "dir", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    log_time(sub_start, log_file)

    resume <- "watersheds"

  } else  skip_task(task, log_file, quiet)
  if(end == "directions") {
    run_time(start, log_file, quiet)
    return()
  }


  # Calculate watersheds -------------------------------------------------------
  task <- "calculating watersheds"
  if(resume == "watersheds") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_dir")) {
      db_dir <- get_previous(folder, step = "dir", where = "flow") %>%
      add_buffer()
    }

    db_initial <- calc_shed4(db_dir, verbose = verbose)
    stats_initial <- pit_stat1(db_initial, verbose = verbose) %>%
      out_stat()

    # Calc stats for first vol2fl
    db_initial <- calc_vol2fl(db = db_initial,
                              i_stats = stats_initial,
                              verbose = verbose)

    # Save
    save_output(data = db_initial, stats = stats_initial,
                name = "initial", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    save_output(data = db_initial,
                name = "initial", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)
    resume <- "local"

  } else skip_task(task, log_file, quiet)
  if(end == "watersheds") {
    run_time(start, log_file, quiet)
    return()
  }

  # Remove initial pits --------------------------------------------------------
  task <- "removing initial pits"
  if(resume == "local") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_initial")) {
      db_initial <- get_previous(folder, step = "initial", where = "flow") %>%
        add_buffer()
    }

    # Pit removal
    db_local <- first_pitr1(db_initial, max_area = max_area,
                            max_depth = max_depth, verbose = verbose)

    # Stats
    stats_local <- pit_stat1(db_local, verbose = verbose) %>%
      out_stat()

    save_output(data = db_local, stats = stats_local, name = "local", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    save_output(data = db_local, name = "local", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log = log_file)

    resume <- "pond"
  } else skip_task(task, log_file, quiet)
  if(end == "local") {
    run_time(start, log_file, quiet)
    return()
  }

  # Calc pond Sheds ---------------------------------------------------------
  task <- "calculating pond (global) watersheds"

  if(resume == "pond") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_local")) {
      db_local <- get_previous(folder, step = "local", where = "flow") %>%
        add_buffer()
    }

    if(length(unique(db_local$shedno[!is.na(db_local$shedno)])) > 1){
      db_pond <- second_pitr1(db_local, verbose = verbose) #also 2nd vol2fl and parea
      stats_pond <- db_pond$stats
      db_pond <- db_pond$db
    } else {
      if(!quiet) message("  Only a single watershed: No pond outputs")
      db_pond <- dplyr::mutate(db_local, pond_shed = local_shed)
      stats_pond <- tibble::tibble()
    }
    save_output(data = db_pond, stats = stats_pond, name = "pond", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    save_output(data = db_pond, name = "pond", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)
    resume <- "fill"
  } else skip_task(task, log_file, quiet)
  if(end == "pond") {
    run_time(start, log_file, quiet)
    return()
  }

  # Calc fill sheds ---------------------------------------------------------
  task <- "calculating fill patterns"
  if(resume == "fill") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_initial") || !exists("db_local") || !exists("db_pond")) {
      db_initial <- get_previous(folder, step = "initial", where = "flow")
      db_local <- get_previous(folder, step = "local", where = "flow")
      db_pond <- get_previous(folder, step = "pond", where = "flow")
    }

    if(length(unique(db_local$shedno[!is.na(db_local$shedno)])) > 1){

      # Add pond sheds details to local sheds
      db_local[, c("pond_shed", "vol2fl", "mm2fl", "parea")] <-
        db_pond[, c("pond_shed", "vol2fl", "mm2fl", "parea")]

      db_fill <- third_pitr1(db_local, verbose = verbose) # calc 2nd mm2fl as progresses
      stats_fill <- db_fill$stats
      db_fill <- db_fill$db
    } else {
      if(!quiet) message("  Only a single watershed: No fill outputs")
      db_fill <- list()
      db_fill <- dplyr::mutate(db_pond, fill_shed = local_shed,
                               vol2fl = 0, mm2fl = 0, parea = 0)
      stats_fill <- tibble::tibble()
    }

    # Calculate upslope in m2
    db_fill <- dplyr::mutate(db_fill, upslope_m = upslope * grid^2)

    # Calculate slope gradients and curvatures
    db_fill <- slope_gc(db_fill, grid = 1)

    save_output(data = db_fill, stats = stats_fill, name = "fill", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    save_output(data = db_fill, name = "fill", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)

    if(nrow(stats_fill) > 0) {
      # Create PIT file
      stats_pit <- stats_fill %>%
        dplyr::filter(final == TRUE) %>%
        dplyr::mutate(edge_pit = FALSE) %>%
        dplyr::arrange(shedno)

      save_output(data = db_fill, stats = stats_pit, name = "pit", locs = out_locs,
                  out_format = out_format, where = "flow", debug = debug)

    } else if(!quiet) message("  Only a single watershed: No pit outputs")

    resume <- "idirections"
  } else skip_task(task, log_file, quiet)
  if(end == "fill") {
    run_time(start, log_file, quiet)
    return()
  }


  # Inverted DEM --------------------------------------------------------------
  task <- "inverting dem"
  announce(task, quiet)
  task <- "calculating inverted directions"
  if(resume == "idirections") {

    if(!exists("db_local")) {
      db_local <- get_previous(folder, step = "local", where = "flow") %>%
        add_buffer()
    }

    db_invert <- db_local %>%
      dplyr::select("elev", "seqno", "x", "y", "row", "col", "buffer",
                    "elev_orig", "edge_map") %>%
      invert()

    # Inverted Directions --------------------------------------------------------
    announce(task, quiet)

    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    db_idir <- calc_ddir2(db_invert, verbose = verbose)

    # Calculate upslope in m2
    db_idir <- dplyr::mutate(db_idir, upslope_m = upslope * grid^2)

    save_output(data = db_idir, name = "idir", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)

    resume <- "iwatersheds"
  } else skip_task(task, log_file, quiet)
  if(end == "idirections") {
    run_time(start, log_file, quiet)
    return()
  }

  # Inverted Watersheds --------------------------------------------------------
  task <- "calculating inverted watersheds"

  if(resume == "iwatersheds") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_idir")) {
      db_idir <- get_previous(folder, step = "idir", where = "flow")
    }

    db_iinitial <- calc_shed4(db_idir, verbose = verbose)
    save_output(data = db_iinitial, name = "iinitial", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)
    resume <- "inverted"
  } else skip_task(task, log_file, quiet)
  if(end == "iwatersheds") {
    run_time(start, log_file, quiet)
    return()
  }


  # Invert Remove Initial Pits -----------------------------------------------
  task <- "removing inverted pits"

  if(resume == "inverted") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_iinitial")) {
      db_iinitial <- get_previous(folder, step = "iinitial", where = "flow")
    }

    db_inverted <- first_pitr1(db_iinitial, max_area = max_area,
                             max_depth = max_depth, verbose = verbose)

    if(length(unique(db_inverted$shedno[!is.na(db_inverted$shedno)])) > 1) {
      stats_ipit <- pit_stat1(db_inverted, verbose = verbose) %>%
        out_stat() %>%
        dplyr::mutate(edge_pit = FALSE)
    } else stats_ipit <- tibble::tibble()

    db_inverted <- dplyr::rename(db_inverted, "inverted_shed" = "shedno")

    save_output(data = db_inverted, stats = stats_ipit, name = "inverted", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)
    save_output(data = db_inverted, name = "inverted", locs = out_locs,
                out_format = out_format, where = "flow", debug = debug)

    log_time(sub_start, log_file)
    resume <- "report"
  } else skip_task(task, log_file, quiet)

  # Final Report ------------------------------------------------------------
  if(resume == "report") {
    task <- "creating report"
    if(report == TRUE){
      announce(task, quiet)

      files <- normalizePath(list.files(path = paste0(out_folder, "/flow"), full.names = TRUE))
      report_final(file = file, report_loc = out_folder, out_files = files,
                   run = f, nrow = nrow, ncol = ncol,
                   max_area = max_area, max_depth = max_depth, rlim = rlim, clim = clim)
    } else skip_task(task, log_file, quiet)
  }


  # Clean up
  if(!debug) remove_output(locs = out_locs, out_format = out_format,
                           where = "flow")

  # Save final time
  run_time(start, log_file, quiet)
}
