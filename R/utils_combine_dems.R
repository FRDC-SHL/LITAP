#' Combine flow and form output dems
#'
#' `flow_mapper()` and `form_mapper()` each provide output information per cell
#' of a dem file. This function takes the fill dem from `flow_mapper()` as well
#' as the length and weti dem files from `form_mapper()` and merges them
#' together into a complete dem file with all information. This file is saved
#' to the project folder.
#'
#' @param folder Character. Folder with previous LITAP runs (i.e. where output
#'   of `flow_mapper()` etc. are)
#' @param out_format Character. Output format (rds or csv) that merged file
#'   should be saved as (if different from the rest; by default uses the format
#'   of the other LITAP output files)
#'
#' @examples
#'
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'             grid = 5, nrow = 90, ncol = 90, out_folder = "./testELEV/")
#'
#' # Then run form_mapper()
#' form_mapper(folder = "./testELEV/")
#'
#'
#' # Now merge together
#' merge_flow_form(folder = "./testELEV")
#'
#' # If you prefer this file as CSV
#' merge_flow_form(folder = "./testELEV", out_format = "csv")
#'
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#'
#' @export

merge_flow_form <- function(folder, out_format = NULL) {

  # Get current out format
  ext <- get_format(folder, where = "flow")
  if(!is.null(out_format)) {
    check_out_format(out_format)
    ext <- out_format
  }

  flow <- get_previous(folder, step = "fill", where = "flow")
  length <- get_previous(folder, step = "length", where = "form")
  weti <- get_previous(folder, step = "weti", where = "form")

  combo <- dplyr::left_join(flow, length,
                            by = c("seqno", "x", "y", "row", "col", "elev")) %>%
    dplyr::left_join(weti,
                     by = c("seqno", "x", "y", "row", "col",
                            "elev", "drec", "upslope"))

  name <- paste0("dem_flow_form_merged.", ext)
  if(ext == "rds") readr::write_rds(combo, file.path(folder, name))
  if(ext == "csv") readr::write_csv(combo, file.path(folder, name))
  combo
}

