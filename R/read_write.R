save_all <- function(locs, data, name) {

  if(all(names(data) == c("db", "stats"))) {
    save_shed(locs$backup_out, data, paste0("backup_", name, ".rds"))
  } else if(names(data) == "db") {
    save_shed(locs$backup_out, data$db, paste0("backup_", name, ".rds"))
  }

  if(name %in% c("initial", "local", "pond", "fill", "pit", "ilocal")) {
    if("db" %in% names(data)) {
      d <- remove_buffer(data$db)
      d <- d[, lapply(d, class) != "list"] # remove lists

      save_shed(locs$final_out, d, paste0("dem_", name, ".rds"))
      save_shed(locs$final_out, d, paste0("dem_", name, ".csv"))

      if(name %in% c("fill", "ilocal")) {
        dbf <- convert_orig(d, type = "dem")
        name_dbf <- name
        if(name == "fill") name_dbf <- "dem"
        if(name == "ilocal") name_dbf <- "idem"
        save_shed(locs$dbf_out, as.data.frame(dbf), paste0(name_dbf, ".dbf"))
      }
    }
    if("stats" %in% names(data)) {
      if(nrow(data$stats) > 0) {
        s <- remove_buffer(data$db, data$stats)
        save_shed(locs$final_out, s, paste0(name, ".rds"))
        save_shed(locs$final_out, s, paste0(name, ".csv"))

        if(name %in% c("local", "pond", "fill", "pit", "ilocal")) {
          name_dbf <- name
          if(name == "ilocal") name_dbf <- "ipit"
          dbf <- convert_orig(s, type = "stats")
          if(any(is.na(dbf))) dbf <- dplyr::mutate_if(dbf, dplyr::funs(all(is.na(.))), dplyr::funs(return(0)))
          save_shed(locs$dbf_out, as.data.frame(dbf), paste0(name_dbf, ".dbf"))
        }
      }
    }
  }
}

save_shed <- function(file_out, obj, name){
  if(stringr::str_detect(name, ".rds$")) readr::write_rds(obj, paste0(file_out, "_", name))
  if(stringr::str_detect(name, ".csv$")) readr::write_csv(obj, paste0(file_out, "_", name))
  if(stringr::str_detect(name, ".dbf$")) foreign::write.dbf(obj, paste0(file_out, "_", name))
}

read_shed <- function(file_out, name){
  readr::read_rds(paste0(file_out, "_", name , ".rds"))
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

  db <- db %>%
    dplyr::mutate(row = row - 1, col = col -1,
                  seqno = 1:length(row))

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

locs_create <- function(folder_out, f) {
  out_locs <- list("backup_out" = paste0(folder_out, "/backup/"),
                   "final_out" = paste0(folder_out, "/final/"),
                   "dbf_out" = paste0(folder_out, "/dbf/"))
  lapply(out_locs, function(x) {if(!dir.exists(x)) dir.create(x)})
  lapply(out_locs, function(x) paste0(x, f))
}
