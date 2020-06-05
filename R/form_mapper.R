#' Map form and relief of the landscape
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' calculates form, wetness indices, reflief and stream/crest lengths (among
#' other metrics).  Based on FormMapR by R. A. (Bob) MacMillan, LandMapper
#' Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param grid Numeric. Grid size for the original dem
#' @param str_val Numeric. Definition of a stream (number of upslope cells)
#' @param ridge_val Numeric. Definition of a ridge (number of downslope cells)
#'
#' @inheritParams args
#'
#' @details For resuming or ending a run, \code{resume} or \code{end} must be
#'   one of the following:
#'
#'   1. `weti` (Calculating Wetness Indices)
#'   2. `relief` (Calculating Relief Derivitives)
#'   3. `length` (Calculating Slope Length)
#'
#' @examples
#'
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 150, ncol = 150)
#'
#' # Now can run form_mapper()
#' form_mapper(folder = "./testELEV/", grid = 5)
#'
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#' @export

form_mapper <- function(folder, grid, str_val = 10000, ridge_val = 10000,
                        out_format = "rds",
                        resume = NULL, end = NULL,
                        log = TRUE, report = TRUE, clean = TRUE,
                        verbose = FALSE, quiet = FALSE) {


  # Messaging
  if(quiet) verbose <- FALSE

  # Get resume options
  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "form", "weti", "relief", "length")
  check_resume(resume, end, resume_options)
  check_grid(grid)

  announce("setup", quiet)

  # Get backup fill dem
  db <- get_previous(folder, step = "fill", where = "flow") %>%
    dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
    add_buffer()

  # Get backup inverted dem
  idb <- get_previous(folder, step = "ilocal", where = "flow") %>%
    dplyr::select(seqno, row, col, elev, drec, ldir, upslope, shedno) %>%
    add_buffer()

  # Get backup pond stats
  pond <- get_previous(folder, step = "pond", type = "stats", where = "flow")

  # Get out locs
  out_locs <- locs_create(folder, which = c("backup", "form"))

  # Setup Log
  if(log) {
    log_file <- file.path(folder, paste0(basename(folder), "_form.log"))
    if(file.exists(log_file)) file.remove(log_file)
  } else log_file <- FALSE

  start <- Sys.time()

  # File details to log
  write_log("Run options:", log = log_file)
  write_log("  Grid size = ", grid, log = log_file)
  write_log("  Relief derivative values: str_val = ", str_val,
            "; ridge_val = ", ridge_val, log = log_file)

  # Run start to log
  write_log("\nRun started: ", start, "\n", log = log_file)

  # Form ------------------------------------------------------------------
  task <- "calculating form"
  if(resume == "" || resume == "form"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_form <- calc_form(db, grid, verbose = verbose)

    save_output2(data = db_form, name = "form", locs = out_locs,
                 out_format = out_format, where = "form")
    #save_backup(locs = out_locs, data = db_form, name = "form")
    #rm(db_form)
    write_time(sub_start, log_file)

    resume <- "weti"
  } else skip_task(task, log_file, quiet)
  if(end == "form") {
    run_time(start, log_file, quiet)
    return()
  }

  # Wetness indices -------------------------------------------------------
  task <- "calculating wetness indices"
  if(resume == "weti") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_form")) {
      db_form <- get_previous(folder, step = "form", where = "form") %>%
        add_buffer()
    }
    #db_form <- read_shed(out_locs$backup, "form")

    db_weti <- calc_weti2(db, grid, verbose = verbose)

    db_form <- dplyr::full_join(db_form, db_weti,
                                by = c("seqno", "col", "row", "buffer")) %>%
      dplyr::mutate(lnqarea = dplyr::if_else(aspect > -1, log(qarea), 0),
                    new_asp = dplyr::if_else(aspect > -1, aspect + 45, 0),
                    new_asp = dplyr::if_else(new_asp > 360,
                                             new_asp -360, new_asp),
                    lnqarea = round(lnqarea, 3))

    save_output2(data = db_form, name = "weti", locs = out_locs,
                 out_format = out_format, where = "form")
    #save_backup(locs = out_locs, data = db_form, name = "weti")
    rm(db_form, db_weti)
    write_time(sub_start, log_file)

    resume <- "relief"
  } else skip_task(task, log_file, quiet)
  if(end == "weti") {
    run_time(start, log_file, quiet)
    return()
  }

  # Relief ------------------------------------------------------------------
  task <- "calculating relief derivitives"
  if(resume == "relief"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)
    db_relz <- calc_relz(db, idb, str_val = str_val, ridge_val = ridge_val,
                         pond = pond, verbose = verbose)

    save_output2(data = db_relz, name = "relief", locs = out_locs,
                 out_format = out_format, where = "form")
    #save_backup(locs = out_locs, data = db_relz, name = "relief")
    #rm(db_relz)
    write_time(sub_start, log_file)

    resume <- "length"
  } else skip_task(task, log_file, quiet)
  if(end == "relief") {
    run_time(start, log_file, quiet)
    return()
  }

  # Length ------------------------------------------------------------------
  task <- "calculating slope length"
  if(resume == "length") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_relz")) {
      db_relz <- get_previous(folder, step = "relief", where = "form") %>%
        add_buffer()
    }
    #db_relz <- read_shed(out_locs$backup, "relief")
    db_length <- calc_length(db, db_relz, verbose = verbose)

    save_output2(data = db_length, name = "length", locs = out_locs,
                 out_format = out_format, where = "form")
    #save_backup(locs = out_locs, data = db_length, name = "length")
    rm(db_length, db_relz)
    write_time(sub_start, log_file)

  } else skip_task(task, log_file, quiet)
  if(end == "length") {
    run_time(start, log_file, quiet)
    return()
  }

  # Save output -------------------------------------------------------------
  #task <- "saving output"
  #announce(task, quiet)

  #save_output(out_locs, out_format,
  #            which = c("weti", "relief", "length"),
  #            where = "form", add_db = db)

  # Clean up ----------------------------------------------------------------
  ##if(clean == TRUE) {
   # task <- "clean up"
  #  announce(task, quiet)
  #  unlink(out_locs$backup)
  #}

  # Save final time
  run_time(start, log_file, quiet)
}
