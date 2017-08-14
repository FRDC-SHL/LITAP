#' @import magrittr
complete_run <- function(file, nrow, ncol, missing_value = -9999,
                         max_area = 10, max_depth = 0.5,
                         run = NULL, folder_out = "./",
                         xlim = NULL, ylim = NULL,
                         continue = NULL, parallel = FALSE,
                         end = NULL) {

  if(is.null(continue)) continue <- ""
  if(!(continue %in% c("", "watersheds", "local_sheds", "global_sheds"))) stop("continue must be one of 'watersheds', local_sheds' or 'global_sheds'")


  f <- basename(file) %>%
    stringr::str_split(stringr::regex("Elev", ignore_case = TRUE)) %>%
    unlist(.) %>%
    .[1]

  if(!dir.exists(folder_out)) dir.create(folder_out)
  file_out <- paste0(folder_out, "/", f)
  if(!is.null(run)) file_out <- paste0("_", run)

  start <- Sys.time()

  if(continue == "") {
    db_start <- foreign::read.dbf(paste0(file)) %>%
      file_prep(nrows = nrow,
                ncols = ncol,
                missing_value = missing_value)  #DO FILE_PREP

    # if subset
    if(!is.null(xlim) || !is.null(ylim)) {
      db_start <- db_start %>%
        dplyr::filter(row >= ylim[1] & row <= ylim[2] & col >= xlim[1] & col <= xlim[2]) %>%
        dplyr::mutate(seqno = 1:length(seqno),
                      row = row - min(row) + 1,
                      col = col - min(col) + 1)
    }
  }

  message("CALCULATING DIRECTIONS")
  if(continue == "") {
    db_dir <- calc_ddir(db_start, verbose = FALSE, parallel = parallel)
    save_shed(file_out, db_dir, "backup_db_dir")
    save_shed(file_out, remove_buffer(db_dir), "db_dir")
  }

  message("CALCULATING WATERSHEDS")
  if(continue == "") {
    db_shed <- calc_shed(db_dir)
    save_shed(file_out, db_shed, "backup_db_shed")
    save_shed(file_out, remove_buffer(db_shed), "db_shed")
  }

  # Calculate upslope -------------------------------------------------------
  message("CALCULATING WATERSHEDS AREA")
  if(continue == "") {
    db_ups <- calc_ups(db_shed)
    w_stat <- pit_stat(db_ups, parallel = parallel)
    save_shed(file_out, db_ups, "backup_db_ups")
    save_shed(file_out, remove_buffer(db_ups), "db_ups")
    save_shed(file_out, remove_buffer(db_ups, w_stat), "w_stats_ups")
  }


  # Remove initial pits --------------------------------------------------------
  message("REMOVING INITIAL PITS")
  if(continue == "watersheds") db_ups <- read_shed(file_out, "backup_db_ups")

  if(!(continue %in% c("local_sheds", "global_sheds"))) {
    db_local_shed <- first_pitr(db_ups, max_area = max_area, max_depth = max_depth)
    w_stats <- pit_stat(db_local_shed, parallel = parallel)
    save_shed(file_out, db_local_shed, "backup_db_local_shed")
    save_shed(file_out, remove_buffer(db_local_shed), "db_local_shed")
    save_shed(file_out, remove_buffer(db_local_shed, w_stats), "w_stats_local_shed")
  }


  # Calc Global Sheds ---------------------------------------------------------
  message("CALCULATING GLOBAL WATERSHEDS")
  if(continue == "local_sheds") db_local_shed <- read_shed(file_out, "backup_db_local_shed")

  if(continue != "global_sheds") {
    db_global_shed <- second_pitr(db_local_shed)
    #w_stats <- pit_stat(db_global_shed, parallel = parallel)
    save_shed(file_out, db_global_shed, "backup_db_global_shed")
    save_shed(file_out, remove_buffer(db_global_shed$db), "db_global_shed")
    save_shed(file_out, remove_buffer(db_global_shed$db, db_global_shed$stats), "w_stats_global_shed")
  }

  if(!is.null(end) && end == "global_sheds") return()

  # Calc fill sheds ---------------------------------------------------------
  if(continue == "global_sheds") {
    db_local_shed <- read_shed(file_out, "backup_db_local_shed")
    db_global_shed <- read_shed(file_out, "backup_db_global_shed")
  }

  # Add global sheds to local sheds
  g_shed <- db_global_shed$db %>%
    dplyr::select(local_shed, global_shed) %>%
    dplyr::distinct()

  db_local_shed <- dplyr::left_join(db_local_shed, g_shed, by = "local_shed")

  message("CALCULATING FILL PATTERNS")
  db_fill <- third_pitr(db_local_shed)
  save_shed(file_out, db_fill, "backup_db_fill")
  save_shed(file_out, remove_buffer(db_fill$db), "db_fill")
  save_shed(file_out, remove_buffer(db_fill$db, db_fill$stats), "w_stats_fill")

  #readr::write_rds(remove_buffer(db_fill$db, db_fill$stats), path = paste0(folder_out, "../w_stats_fill_", file, file_out))

  stop <- Sys.time()

  message("Total run took: ", round(difftime(stop, start, units = "min"), 2), "min")

}
