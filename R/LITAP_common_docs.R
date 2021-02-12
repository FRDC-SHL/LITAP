# args ------------------
#' Common arguments for [flow_mapper()] and [form_mapper()]
#'
#' @param resume Character. From which stage should the run be resumed? (see
#'   Details below)
#' @param end Character. If ending a run after a particular step, which step
#'   (see Details below)
#' @param out_format Character. What format should the data be output as? "csv"
#'   for Comma-separated values, "dbf" for dbf database files, or "rds" for R
#'   data format.
#' @param clean Logical. Remove all output files from previous runs in this
#'   folder?
#' @param report Logical. Create html report of results?
#' @param log Logical. Create log file recording progress?
#' @param verbose Logical. Output extra progress messages.
#' @param quiet Logical. Suppress all messages.
#'
#' @keywords internal
#' @name args
NULL
