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
#' @param out_folder Charater. Folder to store output files. Defaults to folder
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
#'            out_folder = "./testELEV/", nrow = 150, ncol = 150)
#'
#' # Specify parameters for initial pit removal
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'             out_folder = "./testELEV/", nrow = 150, ncol = 150,
#'             max_area = 5, max_depth = 2)
#'
#' # Clean up (remove created folder and output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#' @export
flow_mapper <- function(file, nrow = NULL, ncol = NULL, missing_value = -9999,
                        max_area = 10, max_depth = 0.5,
                        out_folder = NULL, out_format = "rds", clean = FALSE,
                        clim = NULL, rlim = NULL,
                        resume = NULL, end = NULL, log = TRUE, report = TRUE,
                        verbose = FALSE, quiet = FALSE) {

  check_out_format(out_format)

  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "directions", "watersheds",
                    "local", "pond", "fill",
                    "inverted", "iwatersheds",
                    "ilocal", "report")
  check_resume(resume, end, resume_options)
  cont_reg <- c("", "directions", "watersheds", "local", "pond", "fill")

  if(quiet) verbose <- FALSE


  # Check for files
  m <- list.files(dirname(file), pattern = basename(file), ignore.case = TRUE)
  if(length(m) == 0) stop("Cannot find starting elevation file: ", file, call. = FALSE)
  if(length(m) > 1) stop("More than one match found (note that case ignored) for starting elevation file: ", file, "\nMatches: ", paste0(m, collapse = ", "), call. = FALSE)

  f <- tools::file_path_sans_ext(basename(file))

  if(is.null(out_folder)) out_folder <- dirname(file)
  if(!dir.exists(out_folder)) {
    stop("The location '", out_folder, "' doesn't exist. ",
         "Please specify an existing folder", call. = FALSE)
  }
  out_folder <- file.path(out_folder, f)

  if(!dir.exists(out_folder)) dir.create(out_folder)

  out_locs <- locs_create(out_folder, f)

  # Clean records
  if(clean) lapply(stringr::str_remove(out_locs, paste0(f, "$")),
                   function(x) file.remove(list.files(x, full.names = TRUE,
                                                      recursive = TRUE)))

  # Setup Log
  if(log) {
    log_file <- paste0(out_folder, "/", f, "_flow.log")
    if(file.exists(log_file)) file.remove(log_file)
  } else log_file <- FALSE

  start <- Sys.time()

  db_start <- load_file(file, nrow = nrow, ncol = ncol,
                        missing_value = missing_value,
                        clim = clim, rlim = rlim, verbose = verbose)

  ncol_orig <- ncol
  nrow_orig <- nrow
  ncol <- max(db_start$col) - 2
  nrow <- max(db_start$row) - 2

  # File details to log
  write_log("Run options:", log = log_file)
  write_log("  Dimensions: nrows = ", nrow_orig, "; ncols = ", ncol_orig,
            log = log_file)

  # Subset to log
  if(!is.null(clim) || !is.null(rlim)) {
    write_log("  Subset: rows ", rlim[1], "-", rlim[2],
              "; cols ", clim[1], "-", clim[2], log = log_file)
  }

  # Run start to log
  write_log("\nRun started: ", start, "\n", log = log_file)

  if(is.null(nrow_orig)) {
    write_log("  Dimensions detected: nrows = ", nrow, "; ncols = ", ncol, "\n",
              log = log_file)
  }

  # Calculate directions -------------------------------------------------------
  task <- "calculating directions"
  if(resume == "" | resume == "directions") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_dir <- calc_ddir2(db_start, verbose = verbose)

    save_backup(locs = out_locs, data = list("db" = db_dir), name = "dir")

    write_time(sub_start, log_file)

  } else  skip_task(task, log_file, quiet)

  if(end == "directions") {
    run_time(start, log_file, quiet)
    return()
  }


  # Calculate watersheds -------------------------------------------------------
  task <- "calculating watersheds"
  if(resume == "watersheds") db_dir <- read_shed(out_locs$backup, "dir")
  if(resume %in% c("", "watersheds")) {

    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_initial <- list()
    db_initial$db <- calc_shed4(db_dir)

    db_initial$stats <- pit_stat1(db_initial$db) %>%
      out_stat()

    # Calc stats for first vol2fl
    db_initial$db <- calc_vol2fl(db = db_initial$db,
                                 i_stats = db_initial$stats,
                                 verbose = verbose)

    # Save
    save_backup(locs = out_locs,
                data = list("db" = db_initial$db, "stats" = db_initial$stats),
                name = "initial")

    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)

  if(end == "watersheds") {
    run_time(start, log_file, quiet)
    return()
  }

  # Remove initial pits --------------------------------------------------------
  task <- "removing initial pits"
  if(resume == "local") db_initial <- read_shed(out_locs$backup, "initial")
  if(resume %in% c("", "watersheds", "local")) {

    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    # Pit removal
    db_local <- first_pitr1(db_initial$db, max_area = max_area, max_depth = max_depth, verbose = verbose)

    # Stats
    stats_local <- pit_stat1(db_local) %>%
      out_stat()

    db_local <- list("db" = db_local, "stats" = stats_local)
    save_backup(locs = out_locs, data = db_local, name = "local")
    write_time(sub_start, log = log_file)

  } else skip_task(task, log_file, quiet)

  if(end == "local") {
    run_time(start, log_file, quiet)
    return()
  }

  # Calc pond Sheds ---------------------------------------------------------
  task <- "calculating pond (global) watersheds"

  if(resume == "pond") db_local <- read_shed(out_locs$backup, "local")
  if(resume %in% c("", "watersheds", "local", "pond")) {

    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(length(unique(db_local$db$shedno[!is.na(db_local$db$shedno)])) > 1){
      db_pond <- second_pitr1(db_local$db, verbose = verbose) #also 2nd vol2fl and parea
    } else {
      if(!quiet) message("  Only a single watershed: No pond outputs")
      db_pond <- list()
      db_pond$db <- dplyr::mutate(db_local$db, pond_shed = local_shed)
      db_pond$stats <- tibble::tibble()
    }
    save_backup(locs = out_locs, data = list("db" = db_pond$db, "stats" = db_pond$stats), name = "pond")
    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)

  if(end == "pond") {
    run_time(start, log_file, quiet)
    return()
  }

  # Calc fill sheds ---------------------------------------------------------
  task <- "calculating fill patterns"

  if(resume == "fill") {
    db_initial <- read_shed(out_locs$backup, "initial")
    db_local <- read_shed(out_locs$backup, "local")
    db_pond <- read_shed(out_locs$backup, "pond")
  }

  if(resume %in% c("", "watersheds", "local", "pond", "fill")) {

    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

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

    save_backup(locs = out_locs, data = list("db" = db_fill$db, "stats" = db_fill$stats), name = "fill")
    write_time(sub_start, log_file)

    if(nrow(db_fill$stats) > 0) {
      # Create PIT file
      pit <- db_fill$stats %>%
        dplyr::filter(final == TRUE) %>%
        dplyr::mutate(edge_pit = FALSE) %>%
        dplyr::arrange(shedno)

      save_backup(locs = out_locs, data = list("db" = db_fill$db, "stats" = pit), name = "pit")
    } else if(!quiet) message("  Only a single watershed: No pit outputs")

  } else skip_task(task, log_file, quiet)

  if(end == "fill") {
    run_time(start, log_file, quiet)
    return()
  }

  # Inverted DEM --------------------------------------------------------------
  task <- "inverting dem"
  announce(task, quiet)
  task <- "calculating inverted directions"
  if(resume %in%  c(cont_reg, "inverted")) {
    db_invert <- read_shed(out_locs$backup, "local")$db %>%
      dplyr::select(elev, seqno, row, col, missing, buffer, elev_orig, edge_map) %>%
      invert()

  # Inverted Directions --------------------------------------------------------
    announce(task, quiet)

    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_idir <- calc_ddir2(db_invert, verbose = verbose)
    save_backup(locs = out_locs, data = list("db" = db_idir), name = "idir")
    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)

  if(end == "idirections") {
    run_time(start, log_file, quiet)
    return()
  }

  # Inverted Watersheds --------------------------------------------------------
  task <- "calculating inverted watersheds"

  if(resume == "iwatersheds") db_idir <- read_shed(out_locs$backup, "idir")
  if(resume %in% c(cont_reg, "inverted", "iwatersheds")) {

    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_iinitial <- calc_shed4(db_idir)
    save_backup(locs = out_locs, data = list("db" = db_iinitial), name = "iinitial")
    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)

  if(end == "iwatersheds") {
    run_time(start, log_file, quiet)
    return()
  }


  # Invert Remove Initial Pits -----------------------------------------------
  task <- "removing inverted initial pits"

  if(resume == "ilocal") db_iinitial <- read_shed(out_locs$backup, "iinitial")
  if(resume %in% c(cont_reg, "inverted", "iwatersheds", "ilocal")) {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_ilocal <- first_pitr1(db_iinitial, max_area = max_area, max_depth = max_depth, verbose = verbose)
    db_ilocal$pond_shed <- db_ilocal$local_shed

    if(length(unique(db_ilocal$shedno[!is.na(db_ilocal$shedno)])) > 1) {
      ipit <- pit_stat1(db_ilocal) %>%
        out_stat() %>%
        dplyr::mutate(edge_pit = FALSE)
    } else ipit <- tibble::tibble()

    save_backup(locs = out_locs, data = list("db" = db_ilocal, "stats" = ipit), name = "ilocal")
    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)


  # Save output -------------------------------------------------------------
  task <- "saving output"
  announce(task, quiet)
  save_output(out_locs, out_format)

  # Final Report ------------------------------------------------------------
  if(resume %in% c(cont_reg, "inverted", "iwatersheds", "ilocal", "report")) {
    task <- "creating report"
    if(report == TRUE){
      announce(task, quiet)

      files <- normalizePath(list.files(path = paste0(out_folder, "/flow"), full.names = TRUE))
      report_final(file = file, report_loc = out_folder, out_files = files, run = f, nrow = nrow, ncol = ncol,
                   max_area = max_area, max_depth = max_depth, rlim = rlim, clim = clim)
    } else skip_task(task, log_file, quiet)
  }

  # Save final time
  run_time(start, log_file, quiet)
}
