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
#' @noRd

# old_merge <- function(folder, out_format = NULL) {
#
#   # Get current out format
#   ext <- get_format(folder, where = "flow")
#   if(!is.null(out_format)) {
#     check_out_format(out_format)
#     ext <- out_format
#   }
#
#
#
#   combo <- dplyr::left_join(flow, inv, by = "seqno") %>%
#     dplyr::left_join(length,
#                      by = c("seqno", "x", "y", "row", "col", "elev")) %>%
#     dplyr::left_join(weti,
#                      by = c("seqno", "x", "y", "row", "col",
#                             "elev", "drec", "upslope"))
#
#   peak <- inv_stats %>%
#     dplyr::rename(seqno = pit_seqno, row = pit_row, col = pit_col,
#                   peak_shedno = shedno, peak_edge_pit = edge_pit) %>%
#     dplyr::left_join(dplyr::select(combo, -"pit_elev"),
#                      by = c("seqno", "row", "col")) %>%
#     dplyr::select(seqno, x, y, row, col, dplyr::everything()) %>%
#     dplyr::arrange(seqno)
#
#   name <- paste0("all_points.", ext)
#   if(ext == "rds") readr::write_rds(combo, file.path(folder, name))
#   if(ext == "csv") readr::write_csv(combo, file.path(folder, name), progress = FALSE)
#   combo
# }
