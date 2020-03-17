#' Calculate landform facets using LSM logic
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates fuzzy attributes (among other metrics).  Based on
#' FacetMapR by R. A. (Bob) MacMillan, LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#'
#' @inheritParams args
#'
#' @details For resuming or ending a run, \code{resume} or \code{end} must be
#'   one of the following:
#'
#'   - facet
#'
#' @export

facet_mapper <- function(folder, arule, crule,
                         out_format = "rds",
                         resume = NULL, end = NULL,
                         log = TRUE, report = TRUE,
                         verbose = FALSE, quiet = FALSE) {

  message("\nCAUTION: Function Under Development\n")

  # Get resume options
  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "facet", "facet2")
  check_resume(resume, end, resume_options)

  # Get backup fill dem
  db <- get_previous(folder, step = "fill")

  # Get backup form dem
  weti <- get_previous(folder, step = "weti", where = "form") %>%
    dplyr::select(-seqno) %>%
    dplyr::rename(seqno = seqno_buffer)
  relief <- get_previous(folder, step = "relief", where = "form") %>%
    dplyr::select(-seqno) %>%
    dplyr::rename(seqno = seqno_buffer)

  # Get out locs
  out_locs <- locs_create(folder, which = c("backup", "facet"))

  # Messaging
  if(quiet) verbose <- FALSE

  # Setup Log
  if(log) {
    log_file <- list.files(folder, pattern = "_flow.log") %>%
      stringr::str_replace("flow", "form") %>%
      file.path(folder, .)
    if(file.exists(log_file)) file.remove(log_file)
  } else log_file <- FALSE

  start <- Sys.time()

  # File details to log
  write_log("Run options:", log = log_file)
  #write_log("  Rules: = ", str_val,
  #          "; ridge_val = ", ridge_val, log = log_file)

  # Run start to log
  write_log("\nRun started: ", start, "\n", log = log_file)


  # Get Rules ---------------------------------------------------------------

  if(missing(arule) || missing(crule)) {
    stop("Must supply locations of 'arule' and 'crule' files", call. = FALSE)
  }
  arule <- load_rule(arule, type = "arule") %>%
    format_rule(type = "arule")
  crule <- load_rule(crule, type = "crule") %>%
    format_rule(type = "crule")

  # Facets ------------------------------------------------------------------

  task <- "calculating facets"
  if(resume == "" || resume == "facet"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    # Option 1: Original LandMapR LSM program
    fuzzattr <- lsm_fuza(weti = weti, relief = relief, arule = arule)
    save_backup(locs = out_locs, data = fuzzattr, name = "fuza")

    fuzzattr <- lsm_fuzc(fuzzattr, crule = crule) # Also max
    save_backup(locs = out_locs, data = fuzzattr, name = "fuzc")
    write_time(sub_start, log_file)

    # Option 2: BC-PEM Direct-to-Site-Series DSS program
    #calc_dss(arule = arule, crule = crule)

    # Option 3: Condensed LandMapR LSM program
    #calc_lsm()
  }

  # Save output -------------------------------------------------------------
  task <- "saving output"
  announce(task, quiet)

  save_output(out_locs, out_format,
              which = c("fuzc", "fuza"),
              where = "facet",
              add_db = dplyr::select(weti, seqno, buffer, col, row))

  # Save final time
  run_time(start, log_file, quiet)
}
