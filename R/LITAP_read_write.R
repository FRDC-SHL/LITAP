save_basic <- function(data, name, locs, out_format, where) {
  file_out <- locs[[where]]
  name <- paste0(name, ".", out_format)
  if(stringr::str_detect(name, ".rds$")) readr::write_rds(data, file.path(file_out, name))
  if(stringr::str_detect(name, ".csv$")) readr::write_csv(data, file.path(file_out, name), progress = FALSE)
}

save_output <- function(data, stats = NULL, name, locs, out_format, where,
                        add_db = NULL, dynamic_cols = FALSE, debug) {

  # Remove lists to create flat files
  lsts <- names(data)[sapply(data, is.list)]
  data <- dplyr::select(data, -dplyr::all_of(lsts))

  if(!is.null(add_db)) data <- dplyr::left_join(data, add_db, by = "seqno")

  if(!is.null(stats)) {  # Save stats

    stats <- remove_buffer(data, stats)
    cols <- cols_order_stats[[where]]

    if(dynamic_cols) stats <- dplyr::select(stats, dplyr::any_of(cols),
                                           dplyr::everything())
    if(!dynamic_cols) stats <- dplyr::select(stats, dplyr::any_of(cols))

    f <- file_name(locs[[where]], name, "stats", out_format)
    save_shed(f, stats)

  } else { # Save dem files

    data <- remove_buffer(data)
    cols <- cols_order[[where]]
    #cols <- c(cols, "n_rnds")  # Only for debugging
    if(!debug) data <- dplyr::select(data, -dplyr::contains("buffer"))

    if(dynamic_cols) data <- dplyr::select(data, dplyr::any_of(cols),
                                           dplyr::everything())

    if(!dynamic_cols) data <- dplyr::select(data, dplyr::any_of(cols))

    f <- file_name(locs[[where]], name, "dem", out_format)
    save_shed(f, data)
  }
}

remove_output <- function(locs, out_format, where) {
  locs[[where]] %>%
    file.path(glue::glue("{debug_files[[where]]}.{out_format}")) %>%
    unlink()
}

file_name <- function(loc, name, type = "dem", out_format) {
 file.path(loc, glue::glue("{type}_{name}.{out_format}"))
}



save_shed <- function(file_name, obj, clean = FALSE){
  if(clean) {
    obj <- remove_buffer(obj)
    obj <- obj[, lapply(obj, class) != "list"] # remove lists
  }

  if(stringr::str_detect(file_name, ".rds$")) readr::write_rds(obj, file_name)
  if(stringr::str_detect(file_name, ".csv$")) readr::write_csv(obj, file_name, progress = FALSE)
}

read_shed <- function(file_out, name){
  readr::read_rds(file.path(file_out, paste0(name , ".rds")))
}



convert_orig <- function(data, type) {
  if(type == "dem") {
    data$edge = FALSE
    data$missing = is.na(data$elev)
    if(!("vol2fl" %in% names(data))) data$vol2fl = 0
    if(!("mm2fl" %in% names(data))) data$mm2fl = 0
    if(!("parea" %in% names(data))) data$parea = 0

    if("fill_shed" %in% names(data)) {
      data <- dplyr::select(data,
                            SeqNo = seqno, Row = row, Col = col, Elev = elev,
                            Ddir = ddir, Drec = drec, UpSlope = upslope,
                            ShedNo = local_shed, ShedNow = fill_shed, Missing = missing,
                            Edge = edge, Vol2Fl = vol2fl, Mm2Fl = mm2fl, PArea = parea)
    } else {
      data <- dplyr::select(data,
                            SeqNo = seqno, Row = row, Col = col, Elev = elev,
                            Ddir = ddir, Drec = drec, UpSlope = upslope,
                            ShedNo = initial_shed, ShedNow = local_shed, Missing = missing,
                            Edge = edge, Vol2Fl = vol2fl, Mm2Fl = mm2fl, PArea = parea)
    }
  }
  if(type == "stats") {
    if(!("end_pit" %in% names(data))) data$end_pit <- 0
    if(!("stage" %in% names(data))) data$stage <- 0
    if(!("visited" %in% names(data))) data$visited <- FALSE
    if(!("next_pit" %in% names(data))) data$next_pit <- 0
    if(!("becomes" %in% names(data))) data$becomes <- 0
    if(!("final" %in% names(data))) data$final <- FALSE
    if(!("removed" %in% names(data))) data$removed <- FALSE

    data <- dplyr::select(data,
                          ShedNo = shedno,
                          Edge = edge_pit, Final = final, EndPit = end_pit, ShedArea = shed_area,
                          PitRow = pit_row, PitCol = pit_col, PitRec = pit_seqno,
                          PitElev = pit_elev,
                          PourElev = pour_elev, PreVol = pre_vol,
                          PitVol = pit_vol, Varatio = varatio, PitArea = pit_area,
                          DrainsTo = drains_to, NextPit = next_pit,
                          Becomes = becomes, Removed = removed, InRow = in_row,
                          InCol = in_col, InRec = in_seqno,
                          InElev = in_elev, OutRow = out_row, OutCol = out_col,
                          OutRec = out_seqno, OutElev = out_elev,
                          Stage = stage, Visited = visited)
  }
  return(data)

}


remove_buffer <- function(db, stats = NULL) {
  # Get index of seqno buffer
  index <- dplyr::select(db, "seqno_buffer" = "seqno")

  # replace seqno
  db <- db %>%
    dplyr::filter(!.data$buffer) %>%
    dplyr::arrange(.data$row, .data$col) %>%
    dplyr::rename("seqno_buffer" = "seqno")

  if("drec" %in% names(db)) db <- dplyr::rename(db, "drec_buffer" = "drec")

  #if("upslope" %in% names(db)) db <- dplyr::rename(db, upslope_buffer = upslope)

  db <- dplyr::mutate(db, seqno = 1:length(.data$row))

  # Correct rows and columns
  for(i in stringr::str_subset(names(db), "(^row)|(^col)|(_row)|(_col)")) {
    db <- dplyr::mutate(db, !!i := !!rlang::sym(i) - 1)
  }

  # Get index of seqno replacements
  index <- dplyr::left_join(index, dplyr::select(db, "seqno", "seqno_buffer"),
                            by = "seqno_buffer")

  # Stats
  if(!is.null(stats)){
    stats <- stats %>%
      dplyr::mutate(dplyr::across(dplyr::matches("(^row)|(^col)|(_row)|(_col)"), ~ . - 1)) %>%
      dplyr::mutate(dplyr::across(dplyr::contains("seqno"), ~replace(., . == 0, as.numeric(NA)))) %>%
      dplyr::mutate(dplyr::across(dplyr::contains("seqno"), ~index$seqno[.]))
    return(stats)
  } else {
    # Replace drec and upslope with correct cell numbers

    if("drec_buffer" %in% names(db)) {
      db <- dplyr::mutate(db, drec = index$seqno[.data$drec_buffer])
    }
      #upslope = purrr::map(upslope_buffer, ~ rename_seqno(.x, index)))
    return(db)
  }

}

locs_create <- function(out_folder, which, clean) {
  out_locs <- list()
  for(i in which) out_locs[i] <- file.path(out_folder, i)
  if(clean) lapply(out_locs, unlink, recursive = TRUE)
  lapply(out_locs, function(x) {if(!dir.exists(x)) dir.create(x)})
  out_locs
}


#' Load previously created files
#'
#' @param folder Location of Project
#' @param where backup, Flow, Form, etc.
#' @param step Fill, pond, etc.
#' @param type Some files have both "dem" and "stats" extract which one?
#'
#'
#' @noRd
get_previous <- function(folder, where, step, type = "dem", quiet = TRUE) {

  check_folder(folder, fun = stop)
  f <- list_previous(folder, step, where, type)

  ext <- get_format(folder, where)

  if(ext == "rds") r <- readr::read_rds(f)
  if(ext == "csv") r <- readr::read_csv(f, col_types = readr::cols(), progress = !quiet)

  dplyr::select(r, -dplyr::contains("_buffer"))
}

list_previous <- function(folder, step, where, type = "dem", check_only = FALSE) {
  f <- list.files(file.path(folder, where), pattern = paste0("_", step),
                  recursive = TRUE, full.names = TRUE)
  f <- f[stringr::str_detect(basename(f), type)]

  # return TRUE/FALSE with no errors
  if(check_only) f <- length(f) == 1

  # Otherwise error
  if(length(f) > 1) stop("There is more than one eligable ", step, " for type ",
                         type, "\n(",
                         paste0(f, collapse = "\n"), ")", call. = FALSE)
  if(length(f) == 0)  stop("Cannot find ", where, " ", paste0(type, "_", step), " files. ",
                           "Did you run `", where, "_mapper()`?",
                           call. = FALSE)

  f
}


#' Guess format from previous files
#'
#' @param folder Location of Project
#' @param where backup, Flow, Form, etc.
#'
#' @noRd
get_format <- function(folder, where) {

  ext <- list.files(file.path(folder, where), recursive = TRUE, full.names = TRUE) %>%
    stringr::str_extract("[a-z]{3,4}$") %>%
    unique()

  if(length(ext) > 1) stop(
    "There is more than one eligable output format ",
    "for `", where, "`.\n",
    "Consider re-running `flow_mapper()` with the argument `clean = TRUE`\n",
    "(this will remove all files before starting)", call. = FALSE)
  if(length(ext) == 0) stop("There are no eligable output formats. ",
                          "Did the `", where, "` step complete successfully?",
                          call. = FALSE)
  ext
}
