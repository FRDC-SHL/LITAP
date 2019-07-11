save_backup <- function(locs, data, name) {
  if(is.data.frame(data) || (is.list(data) && all(names(data) == c("db", "stats")))) {
    save_shed(locs$backup, data, paste0(name, ".rds"))
  } else if(names(data) == "db") {
    save_shed(locs$backup, data$db, paste0(name, ".rds"))
  }
}

save_output <- function(locs, out_format,
                        which = c("local", "pond", "fill", "pit", "ilocal"),
                        where = "flow", add_db = NULL) {

  for(name in which) {
    if(file.exists(file.path(locs[["backup"]], paste0(name , ".rds")))) {
      data <- read_shed(locs[["backup"]], name)

      if(name %in% c("fill", "ilocal", "form", "weti", "relief", "length")) {
        if("db" %in% names(data)) db <- data$db else db <- data
        if(!is.null(add_db)) {
          suppressMessages(db <- dplyr::left_join(db, add_db))
        }
        save_shed(locs[[where]], db,
                  paste0("dem_", name, ".", out_format), clean = TRUE)
      }

      if("stats" %in% names(data) && nrow(data$stats) > 0) {
        s <- remove_buffer(data$db, data$stats)
        save_shed(locs[[where]], s, paste0("stats_", name, ".", out_format))
      }
    }
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
                            Ddir = ldir, Drec = drec, UpSlope = upslope,
                            ShedNo = local_shed, ShedNow = fill_shed, Missing = missing,
                            Edge = edge, Vol2Fl = vol2fl, Mm2Fl = mm2fl, PArea = parea)
    } else {
      data <- dplyr::select(data,
                            SeqNo = seqno, Row = row, Col = col, Elev = elev,
                            Ddir = ldir, Drec = drec, UpSlope = upslope,
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

  # replace seqno
  db <- db %>%
    dplyr::filter(!buffer) %>%
    dplyr::arrange(row, col) %>%
    dplyr::rename(seqno_buffer = seqno)

  if("drec" %in% names(db)) db <- dplyr::rename(db, drec_buffer = drec)

  #if("upslope" %in% names(db)) db <- dplyr::rename(db, upslope_buffer = upslope)

  db <- dplyr::mutate(db, seqno = 1:length(row))

  # Correct rows and columns
  for(i in stringr::str_subset(names(db), "row|col")) {
    db <- dplyr::mutate(db, !!i := !!rlang::sym(i) - 1)
  }


  # Get index of seqno replacements
  index <- dplyr::select(db, seqno, seqno_buffer)

  # Stats
  if(!is.null(stats)){
    stats <- dplyr::mutate(stats,
                           pit_row = pit_row - 1, pit_col = pit_col - 1,
                           pit_seqno = rename_seqno(pit_seqno, index),
                           out_row = out_row - 1, out_col = out_col - 1,
                           out_seqno = rename_seqno(out_seqno, index),
                           in_row = in_row - 1, in_col = in_col - 1,
                           in_seqno = rename_seqno(in_seqno, index),
                           pit_seqno_out = rename_seqno(pit_seqno_out, index))
    return(stats)
  } else {
    # Replace drec and upslope with correct cell numbers

    if("drec_buffer" %in% names(db)) {
      db <- db %>%
        dplyr::mutate(drec = rename_seqno(drec_buffer, index))
    }
      #upslope = purrr::map(upslope_buffer, ~ rename_seqno(.x, index)))
    return(db)
  }

}

locs_create <- function(out_folder, which = c("backup", "flow")) {
  out_locs <- list()
  for(i in which) out_locs[i] <- file.path(out_folder, i)
  lapply(out_locs, function(x) {if(!dir.exists(x)) dir.create(x)})
  out_locs
}
