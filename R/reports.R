report_final <- function(file, report_loc = NULL, out_files, run, max_area, max_depth, nrow, ncol, rlim, clim) {
  if(is.null(report_loc)) report_loc <- getwd()
  rmarkdown::render(system.file("reports", "final_report.Rmd", package = "LITAP"),
                    params = list(file = file,
                                  out_files = out_files,
                                  run = run,
                                  max_area = max_area,
                                  max_depth = max_depth,
                                  nrow = nrow,
                                  ncol = ncol,
                                  rlim = rlim,
                                  clim = clim),
                    output_file = paste0(run, "_final_report.html"),
                    output_dir = report_loc,
                    envir = new.env(),
                    quiet = TRUE)
}
