save_basic <- function(data, name, locs, out_format, where) {
  file_out <- locs[[where]]
  name <- paste0(name, ".", out_format)
  if(stringr::str_detect(name, ".rds$")) readr::write_rds(data, file.path(file_out, name))
  if(stringr::str_detect(name, ".csv$")) readr::write_csv(data, file.path(file_out, name))
  if(stringr::str_detect(name, ".dbf$")) foreign::write.dbf(data, file.path(file_out, name))
}

save_output <- function(data, stats = NULL, name, locs, out_format, where,
                         add_db = NULL) {
  if(!is.null(add_db)) {
    data <- dplyr::left_join(data, add_db, by = "seqno")
  }

  if(!is.null(stats)) {
    stats <- remove_buffer(data, stats)
    save_shed(locs[[where]], stats, paste0("stats_", name, ".", out_format))
  } else {
    data <- remove_buffer(data)
    save_shed(locs[[where]], data, paste0("dem_", name, ".", out_format))
  }
}

save_shed <- function(file_out, obj, name, clean = FALSE){
  if(clean) {
    obj <- remove_buffer(obj)
    obj <- obj[, lapply(obj, class) != "list"] # remove lists
  }

  if(stringr::str_detect(name, ".rds$")) readr::write_rds(obj, file.path(file_out, name))
  if(stringr::str_detect(name, ".csv$")) readr::write_csv(obj, file.path(file_out, name))
  if(stringr::str_detect(name, ".dbf$")) foreign::write.dbf(obj, file.path(file_out, name))
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
#' @param step Fill, pond, etc.
#' @param where backup, Flow, Form, etc.
#' @param type Some files have both "db" and "stats" extract which one?
#'
#'
#' @noRd
get_previous <- function(folder, step, where, type = "dem") {

  if(!dir.exists(folder)) stop("This folder doesn't exist: ", folder, call. = FALSE)

  f <- list.files(file.path(folder, where), pattern = step,
                  recursive = TRUE, full.names = TRUE)
  f <- f[stringr::str_detect(basename(f), type)]

  if(length(f) > 1) stop("There is more than one eligable ", step, " for type ",
                         type, "\n(",
                         paste0(f, collapse = "\n"), ")", call. = FALSE)
  if(length(f) == 0) stop("There are no eligable ", step, " for type ", type, " files",
                          call. = FALSE)
  r <- readr::read_rds(f) %>%
    dplyr::select(-dplyr::contains("_buffer"))

  if(any(class(r) == "list")) {
    type <- dplyr::if_else(type == "dem", "db", type)
    r <- r[[type]]
  }
  r
}
