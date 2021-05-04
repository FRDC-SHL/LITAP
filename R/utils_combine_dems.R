#' Combine flow and form output dems
#'
#' `flow_mapper()` and `form_mapper()` each provide output information per cell
#' of a dem file. This function takes the fill dem from `flow_mapper()` as well
#' as the form, length, and weti dem files from `form_mapper()` and merges them
#' together into a complete dem file with all information.
#'
#' @param folder Character. Folder with previous LITAP runs (i.e. where output
#'   of `flow_mapper()` etc. are)
#' @param format Character. Output format (rds or csv) that files are saved as.
#'
#' @examples
#'
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 150, ncol = 150)
#'
#' # Then run form_mapper()
#' form_mapper(folder = "./testELEV/", grid = 5)
#'
#'
#' # Now merge together
#' merge_flow_form(folder = "./testELEV")
#'
#' # Clean up (remove all output)
#' unlink("./testELEV/", recursive = TRUE)
#'
#'
#' @export

merge_flow_form <- function(folder, format = "rds") {

  if(format == "rds") {
    flow <- readRDS(file.path(folder, "flow", "dem_fill.rds"))
    form <- readRDS(file.path(folder, "form", "dem_form.rds"))
    length <- readRDS(file.path(folder, "form", "dem_length.rds"))
    weti <- readRDS(file.path(folder, "form", "dem_weti.rds"))
  } else if (format == "csv") {
    flow <- readr::read_csv(file.path(folder, "flow", "dem_fill.csv"))
    form <- readr::read_csv(file.path(folder, "form", "dem_form.csv"))
    length <- readr::read_csv(file.path(folder, "form", "dem_length.csv"))
    weti <- readr::read_csv(file.path(folder, "form", "dem_weti.csv"))
  }

  dplyr::left_join(flow, form,
                   by = c("seqno_buffer", "row", "col", "buffer", "seqno")) %>%
    dplyr::left_join(length,
                     by = c("seqno_buffer", "elev", "row", "col", "buffer",
                            "seqno", "fill_shed")) %>%
    dplyr::left_join(weti,
                     by = c("seqno_buffer", "drec_buffer", "upslope",
                            "elev", "row", "col", "buffer", "seqno", "drec",
                            "local_shed", "fill_shed",
                            "slope_pct", "slope_deg", "aspect", "prof", "plan"))
}

