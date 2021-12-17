# args ------------------

#' Common arguments for [flow_mapper()], [form_mapper()], [facet_mapper()], a
#' nd [wepp_mapper()]
#'
#' @param grid Numeric. Grid size in m of the input DEM file
#' @param resume Character. From which stage should the run be resumed? (see
#' @param clean Logical. Remove all output files from previous runs in this
#'   folder?
#' @param report Logical. Create html report of results?
#' @param log Logical. Create log file recording progress?
#' @param verbose Logical. Output extra progress messages.
#' @param quiet Logical. Suppress all messages.
#' @param debug Logical. If TRUE, output files contain intermediate columns
#'   useful for debugging (e.g., 'buffer', 'seqno_buffer', etc.) Default FALSE.
#'
#' @keywords internal
#' @name args
NULL
