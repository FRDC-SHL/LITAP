#' Map form and relief of the landscape
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' calculates form, wetness indices, reflief and stream/crest lengths (among
#' other metrics).  Based on FormMapR by R. A. (Bob) MacMillan, LandMapper
#' Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param str_val Numeric. Definition of a stream (number of upslope cells)
#' @param ridge_val Numeric. Definition of a ridge (number of downslope cells)
#'
#' @inheritParams args
#'
#' @details For resuming a run, \code{resume} must be one of the following:
#'
#'   1. `weti` (Calculating Wetness Indices)
#'   2. `relief` (Calculating Relief Derivitives)
#'   3. `length` (Calculating Slope Length)
#'
#'   Note that some variables have a version 1 and a version 2 (i.e. `qweti1` and
#'   `qweti2`). These reflect variables calculated (1) area based on number of
#'   cells vs. (2) area based on actual grid cell area values.
#'
#' @examples
#'
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 90, ncol = 90, grid = 5)
#'
#' # Now can run form_mapper()
#' form_mapper(folder = "./testELEV/")
#'
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#' @export

form_mapper <- function(folder, str_val = 10000, ridge_val = 10000,
                        resume = NULL,
                        log = TRUE, clean = FALSE,
                        verbose = FALSE, quiet = FALSE, debug = FALSE) {

  # Checks
  check_folder(folder)

  # Messaging
  if(quiet) verbose <- FALSE

  # Get resume options
  if(is.null(resume)) resume <- ""
  resume_options <- c("", "form", "weti", "relief", "length")
  check_resume(resume, resume_options)

  announce("setup", quiet)

  # Get out format
  out_format <- get_format(folder, where = "flow")

    # Get fill dem
  db <- get_previous(folder, where = "flow", step = "fill") %>%
    dplyr::select("seqno", "x", "y", "row", "col", "elev", "ddir", "drec",
                  "upslope", "fill_shed", "local_shed") %>%
    add_buffer()

  grid <- calc_grid(db)
  check_grid(grid)

  # Get backup inverted dem
  idb <- get_previous(folder, where = "flow", step = "inverted")
  if("ldir" %in% names(idb)) idb <- dplyr::rename(idb, "ddir" = "ldir")
  idb <- dplyr::select(idb, "seqno", "x", "y", "row", "col", "elev", "drec", "ddir",
                       "upslope", "inv_local_shed") %>%
    add_buffer()

  # Get pond stats
  pond <- get_previous(folder, where = "flow", step = "pond", type = "stats") %>%
    add_buffer(db = db, stats = .)

  # Get out locs
  out_locs <- locs_create(folder, which = "form", clean = clean)

  # Setup Log
  log_file <- log_setup(folder, which = "form", log)

  start <- Sys.time()

  # File details to log
  log_write("Run options:", log = log_file)
  log_write("  Grid size = ", grid, log = log_file)
  log_write("  Relief derivative values: str_val = ", str_val,
            "; ridge_val = ", ridge_val, log = log_file)

  # Run start to log
  log_write("\nRun started: ", start, "\n", log = log_file)

  # Form ------------------------------------------------------------------
  task <- "calculating form"
  if(resume == "" || resume == "form"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    db_form <- calc_form(db, grid, verbose = verbose)


    save_output(data = db_form, name = "form", locs = out_locs,
                out_format = out_format, where = "form", debug = debug)
    log_time(sub_start, log_file)

    resume <- "weti"
  } else skip_task(task, log_file, quiet)

  # Wetness indices -------------------------------------------------------
  task <- "calculating wetness indices"
  if(resume == "weti") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_form")) {
      db_form <- get_previous(folder, where = "form", step = "form") %>%
        dplyr::select(dplyr::any_of(c("seqno", "row", "col", "slope_pct",
                                      "slope_deg", "aspect", "prof", "plan"))) %>%
        add_buffer()
    }

    db_weti <- calc_weti2(db, grid, verbose = verbose)

    db_form <- dplyr::full_join(db_form, db_weti,
                                by = c("seqno", "col", "row", "buffer")) %>%
      dplyr::mutate(lnqarea1 = dplyr::if_else(.data$aspect > -1, log(.data$qarea1), 0),
                    lnqarea2 = dplyr::if_else(.data$aspect > -1, log(.data$qarea2), 0),
                    new_asp = dplyr::if_else(.data$aspect > -1, .data$aspect + 45, 0),
                    new_asp = dplyr::if_else(.data$new_asp > 360,
                                             .data$new_asp -360, .data$new_asp),
                    lnqarea1 = trunc_dec(.data$lnqarea1, 3),
                    lnqarea2 = trunc_dec(.data$lnqarea2, 3))

    save_output(data = db_form, name = "form", locs = out_locs,
                out_format = out_format, where = "form", debug = debug)

    rm(db_form, db_weti)
    log_time(sub_start, log_file)

    resume <- "relief"
  } else skip_task(task, log_file, quiet)

  # Relief ------------------------------------------------------------------
  task <- "calculating relief derivitives"
  if(resume == "relief"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)
    db_relz <- calc_relz(db, idb, str_val = str_val, ridge_val = ridge_val,
                         pond = pond, verbose = verbose)

    save_output(data = db_relz, name = "relief", locs = out_locs,
                out_format = out_format, where = "form", debug = debug)

    log_time(sub_start, log_file)

    resume <- "length"
  } else skip_task(task, log_file, quiet)

  # Length ------------------------------------------------------------------
  task <- "calculating slope length"
  if(resume == "length") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_relz")) {
      db_relz <- get_previous(folder, where = "form", step = "relief") %>%
        add_buffer()
    }

    db_length <- calc_length(db, db_relz, grid = grid, verbose = verbose)

    save_output(data = db_length, name = "length", locs = out_locs,
                out_format = out_format, where = "form",
                add_db = dplyr::select(db, "seqno", "x", "y"), debug = debug)
    rm(db_length, db_relz)
    log_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)

  # Clean up
  if(!debug) remove_output(locs = out_locs, out_format = out_format,
                           where = "form")


  # Save final time
  run_time(start, log_file, quiet)
}
