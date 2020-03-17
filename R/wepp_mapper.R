#' Calculate spatial entities required for WEPP
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates hillslope, channel segments and impoundment
#' spatial entities required for input in to WEPP (Water Erosion and Prediction
#' Project) (among other metrics).  Based on FacetMapR by R. A. (Bob) MacMillan,
#' LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param grid Numeric. Grid size for the original dem
#'
#' @inheritParams args
#'
#' @export

wepp_mapper <- function(folder,
                        out_format = "rds",
                        resume = NULL, end = NULL,
                        log = TRUE, report = TRUE,
                        verbose = FALSE, quiet = FALSE) {


  message("\nCAUTION: Function Under Development\n")


  # Get resume options
  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "mark_chan", "cut_chan", "merge_chan", "new_ups",
                      "remark_chan", "mark_pits", "split_segs", "flow2_chan",
                      "calc_segs", "order_segs", "redo_ddir", "add_ddir",
                      "find_upsegs", "hill_sheds", "renum_segs", "build_stru",
                      "wepp_form", "wepp_len", "hill_stats", "chan_stats",
                      "new_ups_final")

  check_resume(resume, end, resume_options)

  # Get backup fill dem
  db <- get_previous(folder, step = "fill")

  # Get out locs
  out_locs <- locs_create(folder, which = c("backup", "wepp"))

  # Messaging
  if(quiet) verbose <- FALSE

  # Setup Log
  if(log) {
    log_file <- list.files(folder, pattern = "_flow.log") %>%
      stringr::str_replace("flow", "wepp") %>%
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
}
