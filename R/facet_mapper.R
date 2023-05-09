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
#' @param edge_row Numeric. Number of rows to remove around the edge of the dem
#'   before deriving the A Rules. Default (NULL) results in removing 5% of the
#'   rows per side (total of 10%).
#' @param edge_col Numeric. Number of cols to remove around the edge of the dem
#'   before deriving the A Rules. Default (NULL) results in removing 5% of the
#'   cols per side (total of 10%).
#' @param procedure Character. Which procedure to use. One of `lsm`
#'   (Original LandMapR program) or `bc_pem` (newer BC-PEM Direct-to-Site-Series
#'   program).
#' @param zone file. If `procedure = "bc_pem"`, zones must either be defined for
#'   each seqno in the form dem file, OR must be provided as an index file
#'   here. With a `zone` defined for each `seqno`. `zone` file can be either dem
#'   (.dem), Excel (.xlsx, .xls), or text (.txt, .csv, .dat)
#'
#' @inheritParams args
#'
#' @details
#'   Based on the technique described in Li et al. 2011, if no `arule` file is
#'   provided, the ARULE cutoffs are calculated from the `form_mapper()` dem
#'   files. These A Rules are saved as `afile_derived.csv` in the `folder`
#'   provided. The topographic derivative percentiles are stored to
#'   `topographic_derivatives.csv`, also in the `folder`.
#'
#'   Procedure `lsm` refers to the landform segmentation model (LSM) offered in
#'   the original LandMapR. Procedure `bc_pem` refers to calculating variables
#'   required for the British Columbia Predictive Ecosystem Mapping
#'   Direct-to-Site-Series program (BC-PEM DSS).
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
#'  \doi{10.4141/CJSS10080}.
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
#' # Let's use the crule file included in the LITAP package
#' crule <- system.file("extdata", "crule.dbf", package = "LITAP")
#' crule
#'
#' # Run facet_mapper() - Derive A Rules
#' facet_mapper(folder = "./testELEV/", arule = NULL, crule = crule)
#'
#' # Derive A Rules, omitting rows and cols from the calculation
#' facet_mapper(folder = "./testELEV/", arule = NULL, crule = crule,
#'              edge_row = 3, edge_col = 1)
#'
#' # Run facet_mapper() - supply A Rules
#' arule <- system.file("extdata", "arule.dbf", package = "LITAP")
#' crule <- system.file("extdata", "crule.dbf", package = "LITAP")
#' facet_mapper(folder = "./testELEV/", arule = arule, crule = crule)
#'
#' \dontrun{
#' # Now consider using your own Rule files
#' facet_mapper(folder = "./testELEV/",
#'              arule = "./testELEV/my_arule.dbf",
#'              crule = "./testELEV/my_crule.dbf")
#'
#'}
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#'
#' @export

facet_mapper <- function(folder, arule = NULL, crule,
                         edge_row = NULL, edge_col = NULL,
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
  weti <- get_previous(folder, step = "form", where = "form") %>%
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

  # Calculate buffers at 5% if not provided
  edge_col_calc <- edge_row_calc <- FALSE
  if(is.null(edge_row)) {
    edge_row_calc <- TRUE
    edge_row <- round(length(unique(weti$row[!weti$buffer])) * 0.05)
  }
  if(is.null(edge_col)) {
    edge_col_calc <- TRUE
    edge_col <- round(length(unique(weti$col[!weti$buffer])) * 0.05)
  }

  perc <- arule_percentiles(weti, relief, edge_row = edge_row,
                            edge_col = edge_col, quiet = quiet)

  if(is.null(arule)) {
    arule <- arule_derive(perc)
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

  # Save afile if derived

  if(!exists("afile")) {
    name <- "ARULE - Derived"
  } else {
    name <- paste0("ARULE - Supplied (", basename(afile), ")")
  }

  d <- list(percentiles_format(perc), arule) %>%
    stats::setNames(nm = c("Site Summary", name))
  writexl::write_xlsx(d, path = file.path(folder,
                                          "topographic_derivatives.xlsx"))

  # Setup Log
  log_file <- log_setup(folder, which = "facet", log)

  start <- Sys.time()

  if(exists("afile")) {
    a <- normalizePath(afile)
  } else a <- "Derived (see topographic_derivatives.xlsx)"

  # File details to log
  log_write("Run options:\n", log = log_file)
  log_write("  Input folder = ", normalizePath(folder), "\n",
            "  arule file =  ", a, "\n",
            "  crule file = ", normalizePath(cfile), "\n",
            "  edge_row = ", edge_row, dplyr::if_else(edge_row_calc, " (5%)\n", "\n"),
            "  edge_col = ", edge_col, dplyr::if_else(edge_col_calc, " (5%)\n", "\n"),
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
