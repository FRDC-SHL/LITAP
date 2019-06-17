#' Map form and relief of the landscape
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' calculates form, wetness indices, reflief and stream/crest lengths (among
#' other metrics).  Based on FormMapR by R. A. (Bob) MacMillan, LandMapper
#' Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param grid Numeric. Grid size for the original dem
#'
#' @inheritParams args
#'
#' @details Runs can be resumed at the "relief" step or the "length" step.
#'
#'   For resuming or ending a run, \code{resume} or \code{end} must be one of
#'   the following:
#'
#'   1. `relief` (Calculating Relief Derivitives)
#'   2. `length` (Calculating Slope Length)
#'
#' @examples
#'
#' \dontrun{
#' form_mapper(folder = "my_runs/021/", grid = 5)
#' }
#'
#' @export

form_mapper <- function(folder, grid,
                        resume = NULL, end = NULL,
                        log = TRUE, report = TRUE,
                        verbose = FALSE, quiet = FALSE) {

  # Get backup pond
  db <- get_backups(folder)

  # Get backup inverted
  idb <- get_backups(folder, type = "backup_ilocal")

  # Resume
  if(!is.null(resume) && !resume %in% c(NULL, "relief", "length")) {
    stop("'resume' can only be 'relief', 'length', or NULL (no resume)",
         call. = FALSE)
  }

  if(is.null(resume)){
    if(!quiet) message("CALCULATING FORM")
    db_form <- calc_form(db, grid)

    if(!quiet) message("CALCULATING WETNESS INDICES")
    db_weti <- calc_weti(db, grid, verbose = verbose)
    db_form <- dplyr::full_join(db_form, db_weti) %>%
      dplyr::mutate(lnqarea = dplyr::if_else(aspect > -1, log(qarea), 0),
                    new_asp = dplyr::if_else(aspect > -1, aspect + 45, 0),
                    new_asp = dplyr::if_else(new_asp > 360,
                                             new_asp -360, new_asp))
    readr::write_csv(db_form, "form.csv")
    resume <- "relief"
  }

  if(resume == "relief"){
    if(!quiet) message("CALCULATING RELIEF DERIVITIVES")
    db_relz <- calc_relz(db, idb, verbose = verbose)
    readr::write_csv(db_relz, "relz.csv")
    resume <- "length"
  }

  if(resume == "length") {
    if(!quiet) message("CALCULATING SLOPE LENGTH")
    db_relz <- readr::read_csv("relz.csv")
    db_length <- calc_length(db, db_relz)
    readr::write_csv(db_length, "len.csv")
  }
}

get_backups <- function(folder, type = "backup_pond") {
  if(!dir.exists(folder)) stop("This folder doesn't exist: ", folder, call. = FALSE)
  f <- list.files(folder, pattern = type,
                  recursive = TRUE, full.names = TRUE)
  if(length(f) > 1) stop("There is more than one eligable ", type, " file:\n",
                         paste0(f, collapse = "\n"), call. = FALSE)
  if(length(f) == 0) stop("There are no eligable ", type, " files",
                          call. = FALSE)
  readr::read_rds(f)$db
}
