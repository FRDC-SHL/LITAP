#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>% %T>%
#' @usage lhs \%>\% rhs
NULL


na_omit <- function(x) return(x[!is.na(x)])


max_na <- function(x) {
  if(sum(!is.na(x)) > 0) y <- max(x, na.rm = TRUE) else y <- as.numeric(NA)
  y
}

log_setup <- function(folder, which, log) {
  if(log) {
    log_file <- file.path(folder, paste0(basename(folder), "_", which, ".log"))
    unlink(log_file)
  } else log_file <- FALSE
  log_file
}

log_write <- function(..., log) {
  if(log != FALSE) write(paste0(...), file = log, append = TRUE)
}

log_start <- function(task, time, log) {
  log_write("Started ", task, " at: ", time, log = log)
}

log_time <- function(time, log) {
  log_write("  Total time: ",
            round(difftime(Sys.time(), time, units = "min"), 2),
            log = log)
}

announce <- function(task, quiet) {
  if(!quiet) message(toupper(task))
}

skip_task <- function(task, log_file, quiet) {
  if(!quiet) message("SKIPPING ", toupper(task))
  log_write("Skipping ", task, log = log_file)
}

check_out_format <- function(out_format){
  if(!out_format %in% c("csv", "rds")) {
    stop("'out_format' must be one of 'csv' or 'rds'", call. = FALSE)
  }
}

check_resume <- function(resume, resume_options) {
  if(!resume %in% resume_options) {
    stop("resume must be one of 'NULL' (no resume), '",
         paste0(resume_options[-1], collapse = "', '"), "'", call. = FALSE)
  }
}

check_grid <- function(grid) {
  if(missing(grid) || !is.numeric(grid) || grid < 0){
    stop("'grid' must be a number greater than 0", call. = FALSE)
  }
}

calc_grid <- function(db) {
  x <- sort(unique(db$x))
  median(x - dplyr::lag(x), na.rm = TRUE)
}

run_time <- function(start, log_file, quiet) {
  stop <- Sys.time()
  runtime <- round(difftime(stop, start, units = "min"), 2)
  if(!quiet) message("Run took: ", runtime, " min")
  log_write("\nTotal run time: ", runtime, " min", log = log_file)
}

trunc_dec <- function(x, digits) {
 trunc(x * 10^(digits)) / 10^(digits)
}

stats <- function(df) {
  df %>%
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = list(p99 = ~stats::quantile(., 0.99, na.rm = TRUE),
                    p95 = ~stats::quantile(., 0.95, na.rm = TRUE),
                    p90 = ~stats::quantile(., 0.90, na.rm = TRUE),
                    p85 = ~stats::quantile(., 0.85, na.rm = TRUE),
                    p80 = ~stats::quantile(., 0.80, na.rm = TRUE),
                    p75 = ~stats::quantile(., 0.75, na.rm = TRUE),
                    p70 = ~stats::quantile(., 0.70, na.rm = TRUE),
                    p65 = ~stats::quantile(., 0.65, na.rm = TRUE),
                    p60 = ~stats::quantile(., 0.60, na.rm = TRUE),
                    p55 = ~stats::quantile(., 0.55, na.rm = TRUE),
                    p50 = ~stats::quantile(., 0.50, na.rm = TRUE),
                    p45 = ~stats::quantile(., 0.45, na.rm = TRUE),
                    p40 = ~stats::quantile(., 0.40, na.rm = TRUE),
                    p35 = ~stats::quantile(., 0.35, na.rm = TRUE),
                    p30 = ~stats::quantile(., 0.30, na.rm = TRUE),
                    p25 = ~stats::quantile(., 0.25, na.rm = TRUE),
                    p20 = ~stats::quantile(., 0.20, na.rm = TRUE),
                    p15 = ~stats::quantile(., 0.15, na.rm = TRUE),
                    p10 = ~stats::quantile(., 0.10, na.rm = TRUE),
                    p05 = ~stats::quantile(., 0.05, na.rm = TRUE),
                    p01 = ~stats::quantile(., 0.01, na.rm = TRUE),
                    avg = ~mean(., na.rm = TRUE),
                    sd  = ~sd(., na.rm = TRUE),
                    min = ~min(., na.rm = TRUE),
                    max = ~max(., na.rm = TRUE),
                    n   = ~sum(!is.na(.)))))
}


percentiles_format <- function(perc) {
  perc %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "name_orig", values_to = "value") %>%
    dplyr::mutate(
      name = stringr::str_extract(.data$name_orig, "[^_]+$"),
      parameter = stringr::str_remove(.data$name_orig, paste0("_", .data$name))) %>%
    dplyr::select(-"name_orig") %>%
    tidyr::pivot_wider(names_from = "parameter", values_from = "value") %>%
    dplyr::mutate(
      name = stringr::str_replace(.data$name, "^p[0]*([0-9]{1,2})", "\\1%"),
      name = factor(.data$name, levels = c("n", "avg", "sd", "min", "1%",
                                           paste0(seq(5,95,5), "%"),
                                           "99%", "max"))) %>%
    dplyr::arrange(.data$name)
}

