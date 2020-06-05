#' Calculate landform facets using LSM logic
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates fuzzy attributes (among other metrics).  Based on
#' FacetMapR by R. A. (Bob) MacMillan, LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param arule Character. Location of arule file
#' @param crule Character. Location of crule file
#' @param procedure Character. Which LSM procedure to use. One of `lsm` or
#'   `bc_pem`.
#' @param zone
#'
#' @inheritParams args
#'
#' @details
#'   Procedure `lsm` refers to... Procedure `bc_pem` refers to...
#'
#'   For resuming or ending a run, \code{resume} or \code{end} must be
#'   one of the following:
#'
#'   - attributes
#'   - classes
#'
#' @export

facet_mapper <- function(folder, arule, crule,
                         procedure = "lsm",
                         zone = NULL,
                         out_format = "rds",
                         resume = NULL, end = NULL,
                         log = TRUE, report = TRUE,
                         verbose = FALSE, quiet = FALSE) {

  # Messaging
  if(quiet) verbose <- FALSE

  message("\nCAUTION: Function Under Development\n")

  # Get resume options
  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "attributes", "classes")
  check_resume(resume, end, resume_options)

  # Get procedure
  if(!procedure %in% c("lsm", "bc_pem")) {
    stop("'procedure' must be one of 'lsm' or 'bc_pem'",
         call. = FALSE)
  }

  # Get fill dem (flow_mapper)
  db <- get_previous(folder, step = "fill", where = "flow") %>%
    dplyr::select(seqno, row, col, elev, drec, upslope, fill_shed, local_shed) %>%
    add_buffer()

  # Get form dem (form_mapper)
  weti <- get_previous(folder, step = "weti", where = "form") %>%
    dplyr::select(-tidyselect::any_of(c("seqno_buffer", "drec_buffer"))) %>%
    add_buffer()
  relief <- get_previous(folder, step = "relief", where = "form") %>%
    dplyr::select(-tidyselect::any_of(c("seqno_buffer", "drec_buffer"))) %>%
    add_buffer()

  # Get out locs
  out_locs <- locs_create(folder, which = "facet")

  # Get Rules ---------------------------------------------------------------

  if(missing(arule) || missing(crule)) {
    stop("Must supply locations of 'arule' and 'crule' files", call. = FALSE)
  }

  afile <- arule
  cfile <- crule

  arule <- load_extra(arule, type = "arule") %>%
    format_rule(type = "arule")
  crule <- load_extra(crule, type = "crule") %>%
    format_rule(type = "crule")

  check_rules(arule, crule)

  crule <- prep_rule(crule, type = "crule")

  # Get Zones (if applicable) -----------------------------------------------
  if (procedure == "bc_pem") {
    if(is.null(zone) && !"zone" %in% names(weti)) {
      stop("For procedure 'bc_pem', must either supply 'zone' file OR ",
           "have 'zone' field/column in weti data.", call. = FALSE)
    } else if (!is.null(zone)) {
      zone <- load_extra(zone, type = "zone") %>%
        dplyr::arrange(zone, seqno)
    }
    if(!"zone" %in% c(names(arule), names(crule)) ||
       !all(unique(zone$zone) %in% unique(arule$zone)) ||
       !all(unique(zone$zone) %in% unique(crule$zone))) {
      stop("For procedure 'bc_pem', 'arule' and 'crule' files must have ",
           "rules for all eco zones present in 'zone' file.", call. = FALSE)
    }

    if(!c("steep", "sw_aspect", "ne_aspect", "slopelt20") %in% names(arule)) {
      stop("For procedure 'bc_pem', 'arule' file requires classes ('class_out') ",
           "'sw_aspect', 'ne_aspect', 'steep', and 'slopelt20'", call. = FALSE)
    }

    weti <- dplyr::left_join(weti, zone, by = "seqno")


  } else {
    weti <- dplyr::mutate(weti, zone = 0)
    arule <- dplyr::mutate(arule, zone = 0)
    crule <- dplyr::mutate(crule, zone = 0)
  }


  # Setup Log
  if(log) {
    log_file <- file.path(folder, paste0(basename(folder), "_facet.log"))
    if(file.exists(log_file)) file.remove(log_file)
  } else log_file <- FALSE

  start <- Sys.time()

  # File details to log
  write_log("Run options:\n", log = log_file)
  write_log("  Input folder = ", normalizePath(folder), "\n",
            "  arule file =  ", normalizePath(afile), "\n",
            "  crule file = ", normalizePath(cfile), "\n",
            "  Procedure = ", procedure, "\n",
            log = log_file)

  # Run start to log
  write_log("\nRun started: ", start, "\n", log = log_file)

  # Facets ------------------------------------------------------------------

  task <- "calculating fuzzy attributes"
  if(resume == "" || resume == "attributes"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    # Get attributes
    attr <- get_attr(weti, relief)

    # Get fuzzy attributes
    fuzzattr <- lsm_fuza(attr = attr, arule = arule, procedure = procedure)
    save_output2(data = fuzzattr, name = "fuza", locs = out_locs,
                 out_format = out_format, where = "facet",
                 add_db = dplyr::select(db, "seqno", "buffer", "row", "col"))
    #save_backup(locs = out_locs, data = fuzzattr, name = "fuza")
    write_time(sub_start, log_file)
  } else skip_task(task, log_file, quiet)
  if(end == "attributes") {
    run_time(start, log_file, quiet)
    return()
  }


  task <- "calculating classes"
  if(resume == "" || resume == "classes"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("fuzzattr")) {
      fuzzattr <- get_previous(folder, step = "fuza", where = "facet") %>%
        add_buffer()
    }

    fuzzattr <- lsm_fuzc(fuzzattr, crule = crule) # Also max
    save_output2(data = fuzzattr, name = "fuzc", locs = out_locs,
                 out_format = out_format, where = "facet",
                 add_db = dplyr::select(db, "seqno", "buffer", "row", "col"))
    #save_backup(locs = out_locs, data = fuzzattr, name = "fuzc")
    write_time(sub_start, log_file)
  }

  # Save output -------------------------------------------------------------
  # task <- "saving output"
  # announce(task, quiet)
  #
  # save_output(out_locs, out_format,
  #             which = c("fuzc", "fuza"),
  #             where = "facet",
  #             add_db = dplyr::select(weti, seqno, buffer, col, row))

  # Save final time
  run_time(start, log_file, quiet)
}
