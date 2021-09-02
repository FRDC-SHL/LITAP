#' Calculate landform facets using LSM logic
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates fuzzy attributes (among other metrics).  Based on
#' FacetMapR by R. A. (Bob) MacMillan, LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param arule Character. Location of ARULE file. If NULL, A Rules are derived
#'   from the dem file (see Details).
#' @param crule Character. Location of CRULE file
#' @param n_remove Numeric. Number of cells (rows/columns) to remove around the
#'   edge of the dem before deriving the A Rules.
#' @param procedure Character. Which LSM procedure to use. One of `lsm`
#'   (Original LandMapR program) or `bc_pem` (newer BC-PEM Direct-to-Site-SEries
#'   DSS program).
#' @param zone file. If `procedure = "bc_pem"`, zones must either be defined for
#'   each seqno in the weti dem file, OR must be provided as an index file here.
#'   With a `zone` defined for each `seqno`. `zone` file can be either dem (.dem),
#'   Excel (.xlsx, .xls), or text (.txt, .csv, .dat)
#'
#' @inheritParams args
#'
#' @details
#'   Based on the technique described in Li et al. 2011, if no `arule` file is
#'   provided, the ARULE cutoffs are calculated from the `form_mapper()` dem
#'   files. These A Rules are saved as `afile_derived.csv` in the `folder`
#'   provided.
#'
#'   Procedure `lsm` refers to... Procedure `bc_pem` refers to...
#'
#'   For resuming  a run, \code{resume} must be
#'   one of the following:
#'
#'   - attributes
#'   - classes
#'
#'  @references
#'  Sheng Li, David A. Lobb, Brian G. McConkey, R. A. MacMillan, Alan Moulin,
#'  and Walter R. Fraser. 2011. Extracting topographic characteristics of landforms
#'  typical of Canadian agricultural landscapes for agri-environmental modeling.
#'  I. Methodology. Canadian Journal of Soil Science 91(2), 251-266.
#'  <https://doi.org/10.1139/CJSS10080>
#'
#' @examples
#'
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 90, ncol = 90, grid = 5)
#'
#' # And form_mapper()
#' form_mapper(folder = "./testELEV/")
#'
#' # Now can run facet_mapper() - Derive A Rules
#' crule <- system.file("extdata", "crule.dbf", package = "LITAP")
#' facet_mapper(folder = "./testELEV/", arule = NULL, crule = crule)
#'
#' # Now can run facet_mapper() - supply A Rules
#' arule <- system.file("extdata", "arule.dbf", package = "LITAP")
#' crule <- system.file("extdata", "crule.dbf", package = "LITAP")
#' facet_mapper(folder = "./testELEV/", arule = arule, crule = crule)
#'
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#'
#' @export

facet_mapper <- function(folder, arule = NULL, crule, n_remove = 9,
                         procedure = "lsm",
                         zone = NULL,
                         clean = FALSE,
                         resume = NULL,
                         log = TRUE,
                         verbose = FALSE, quiet = FALSE, debug = FALSE) {

  # Messaging
  if(quiet) verbose <- FALSE

  # Get resume options
  if(is.null(resume)) resume <- ""
  resume_options <- c("", "attributes", "classes")
  check_resume(resume, resume_options)

  # Get procedure
  if(!procedure %in% c("lsm", "bc_pem")) {
    stop("'procedure' must be one of 'lsm' or 'bc_pem'",
         call. = FALSE)
  }

  # Get out format
  out_format <- get_format(folder, where = "flow")

  # Get fill dem (flow_mapper)
  db <- get_previous(folder, step = "fill", where = "flow") %>%
    dplyr::select("seqno", "x", "y", "row", "col", "elev", "drec", "upslope",
                  "fill_shed", "local_shed") %>%
    add_buffer()

  # Get form dem (form_mapper)
  weti <- get_previous(folder, step = "weti", where = "form") %>%
    dplyr::select(-tidyselect::any_of(c("seqno_buffer", "drec_buffer"))) %>%
    dplyr::rename("qweti" = "qweti1", "qarea" = "qarea1",
                  "lnqarea" = "lnqarea1") %>%
    add_buffer()

  # Get relief dem (form_mapper)
  relief <- get_previous(folder, step = "length", where = "form") %>%
    dplyr::select(-tidyselect::any_of(c("seqno_buffer", "drec_buffer"))) %>%
    add_buffer()

  # Add details to add to outputs
  db_add <- dplyr::select(db, "seqno", "elev", "buffer", "x", "y", "row", "col")

  # Get out locs
  out_locs <- locs_create(folder, which = "facet", clean = clean)

  # Get Rules ---------------------------------------------------------------

  if(missing(crule)) {
    stop("Must supply locations of 'crule' file", call. = FALSE)
  }

  if(is.null(arule)) {
    arule <- arule_derive(weti, relief, n_remove = n_remove)
  } else {
    afile <- arule
    arule <- load_extra(arule, type = "arule")
  }
  arule <- format_rule(arule, type = "arule", quiet)

  cfile <- crule
  crule <- load_extra(crule, type = "crule") %>%
    format_rule(type = "crule", quiet)

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
  log_file <- log_setup(folder, which = "facet", log)

  start <- Sys.time()

  # File details to log
  if(!exists("afile")) {
    afile <- file.path(folder, "afile_derived.csv")
    utils::write.csv(arule, afile, row.names = FALSE)
  }
  log_write("Run options:\n", log = log_file)
  log_write("  Input folder = ", normalizePath(folder), "\n",
            "  arule file =  ", normalizePath(afile), "\n",
            "  crule file = ", normalizePath(cfile), "\n",
            "  n_remove = ", n_remove, "\n",
            "  Procedure = ", procedure, "\n",
            log = log_file)

  # Run start to log
  log_write("\nRun started: ", start, "\n", log = log_file)

  # Facets - fuzzy attributes ------------------------------------------------
  task <- "calculating fuzzy attributes"
  if(resume == "" || resume == "attributes"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    # Get attributes
    attr <- get_attr(weti, relief)

    # Get fuzzy attributes
    fuzzattr <- lsm_fuza(attr = attr, arule = arule, procedure = procedure)

    save_output(data = fuzzattr, name = "fuza", locs = out_locs,
                out_format = out_format, where = "facet",
                add_db = db_add, debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Facets - classes ------------------------------------------------
  task <- "calculating classes"
  if(resume == "" || resume == "classes"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("fuzzattr")) {
      fuzzattr <- get_previous(folder, step = "fuza", where = "facet") %>%
        add_buffer()
    }

    fuzzattr <- lsm_fuzc(fuzzattr, crule = crule) # Also max
    save_output(data = fuzzattr, name = "fuzc", locs = out_locs,
                out_format = out_format, where = "facet",
                add_db = db_add, dynamic_cols = TRUE, debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  }

  # Save final time
  run_time(start, log_file, quiet)
}
