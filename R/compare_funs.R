#' @export
load_ostat <- function(file) {
  f <- foreign::read.dbf(file) %>%
    dplyr::mutate(PitVol = round(PitVol, 3),
                  Varatio = round(Varatio, 2),
                  PourElev = round(PourElev, 4),
                  PitElev = round(PitElev, 4),
                  OutElev = round(OutElev, 4),
                  InElev = round(InElev, 4)) %>%
    tibble::as_tibble()
}

#' @export
load_odem <- function(file) {
  f <- foreign::read.dbf(file) %>%
    dplyr::mutate(Mm2Fl = round(Mm2Fl, 3),
                  Vol2Fl = round(Vol2Fl, 3),
                  PArea = round(PArea, 3)) %>%
    tibble::as_tibble()
}

#' @export
load_ndem1 <- function(file) {
  f <- foreign::read.dbf(file) %>%
    dplyr::mutate(PitVol = round(PitVol, 3),
                  Varatio = round(Varatio, 2),
                  PourElev = round(PourElev, 4),
                  PitElev = round(PitElev, 4),
                  OutElev = round(OutElev, 4),
                  InElev = round(InElev, 4)) %>%
    tibble::as_tibble()
}

#' @export
load_ndem2 <- function(file) {
  f <- foreign::read.dbf(file) %>%
    dplyr::mutate(Vol2Fl = round(Vol2Fl, 3),
                  Mm2Fl = round(Mm2Fl, 3)) %>%
    tibble::as_tibble()
}

#' @export
load_orig <- function(file) {
  if(stringr::str_detect(file, stringr::regex("dem.dbf", ignore_case = TRUE))) {
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(ldir = ddir, upslope_n = upslope, local_shed = shedno, fill_shed = shednow) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, local_shed, fill_shed)
  } else if(stringr::str_detect(file, stringr::regex("(ipit.dbf)", ignore_case = TRUE))){
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev = pourelev, shed_area = shedarea, pit_area = pitarea,
                    pit_vol = pitvol, pit_elev = pitelev, varatio,
                    pit_seqno = pitrec, pit_row = pitrow, pit_col = pitcol,
                    out_shed = drainsto,
                    out_elev = outelev, out_row = outrow, out_col = outcol,
                    in_row = inrow, in_col = incol,
                    out_seqno = outrec, in_seqno = inrec)
  } else if (stringr::str_detect(file, stringr::regex("(pit.dbf)|(pond.dbf)", ignore_case = TRUE))){
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev = pourelev, shed_area = shedarea, pit_area = pitarea,
                    pit_vol = pitvol, pit_elev = pitelev, varatio,
                    pit_seqno = pitrec, pit_row = pitrow, pit_col = pitcol,
                    out_shed = drainsto,
                    out_elev = outelev, out_row = outrow, out_col = outcol,
                    in_row = inrow, in_col = incol,
                    out_seqno = outrec, in_seqno = inrec,
                    next_pit = nextpit, becomes, edge, final)
  } else if (stringr::str_detect(file, stringr::regex("fill.dbf", ignore_case = TRUE))){
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev = pourelev, shed_area = shedarea, pit_area = pitarea,
                    pit_vol = pitvol, pit_elev = pitelev, varatio,
                    pit_seqno = pitrec, pit_row = pitrow, pit_col = pitcol,
                    out_shed = drainsto, out_elev = outelev,
                    out_row = outrow, out_col = outcol,
                    in_row = inrow, in_col = incol,
                    out_seqno = outrec, in_seqno = inrec,
                    edge_pit = edge, next_pit = nextpit, becomes, end_pit = endpit, removed, stage, final)
  }
  return(f)
}

#' @export
load_new <- function(file){
  if(stringr::str_detect(file, stringr::regex("dem_pond.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, initial_shed, local_shed, pond_shed)
  } else if(stringr::str_detect(file, stringr::regex("dem_fill.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, initial_shed, local_shed, pond_shed, fill_shed)
  } else if(stringr::str_detect(file, stringr::regex("dem_ups.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, initial_shed)
  } else if(stringr::str_detect(file, stringr::regex("(dem_local)|(dem_ilocal)", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, initial_shed, local_shed)
  } else if(stringr::str_detect(file, stringr::regex("initial", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev,
                    varatio, pit_seqno, pit_row, pit_col,
                    out_shed, out_elev, out_row, out_col, in_row, in_col) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3), varatio = round(varatio, 2))
  } else if(stringr::str_detect(file, stringr::regex("(ipit)", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev,
                    varatio, pit_seqno, pit_row, pit_col,
                    out_elev, out_row, out_col, in_row, in_col,
                    out_seqno, in_seqno) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3), varatio = round(varatio, 2),
                    pour_elev = round(pour_elev, 3),
                    pit_elev = round(pit_elev, 3),
                    out_elev = round(out_elev, 3))
  } else if(stringr::str_detect(file, stringr::regex("(pond)|(pit)", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev,
                    varatio, pit_seqno, pit_row, pit_col,
                    out_shed, out_elev, out_row, out_col, in_row, in_col,
                    out_seqno, in_seqno, next_pit, becomes, final) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3), varatio = round(varatio, 2))
  } else if(stringr::str_detect(file, stringr::regex("fill", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev, varatio,
                    pit_seqno, pit_row, pit_col,
                    out_shed, out_elev, out_row, out_col, in_row, in_col,
                    out_seqno, in_seqno,
                    edge_pit, next_pit, becomes, end_pit, final) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3),
                    varatio = round(varatio, 2))
  }
  return(f)
}

#' @export
compare <- function(orig, new) {
  z <- list()
  small <- vector()
  all_cols <- intersect(names(orig), names(new))
  all_cols <- all_cols[!(all_cols %in% c("PitRow", "PitCol","OutRow", "OutCol", "InRow", "InCol"))]
  all_cols <- all_cols[!(all_cols %in% c("PreVol", "Stage", "Visited", "Removed"))]
  for(col_name in all_cols){
    if(!all(dplyr::pull(orig, col_name) == dplyr::pull(new, col_name))) {
      msg <- paste0("Not all ", col_name, " the same")
      if(col_name == "ShedArea") if(abs(max(orig$ShedArea - new$ShedArea)) < 5) {msg <- "Shed area not the same, but within 5 cells"; small <- c(small, col_name)}
      if(col_name == "PitArea") if(abs(max(orig$PitArea - new$PitArea)) < 5) {msg <- "Pit area not the same, but within 5 cells"; small <- c(small, col_name)}
      if(col_name == "PitVol") if(abs(max(orig$PitVol - new$PitVol)) < 3) {msg <- "Pit Vol not the same, but within 3"; small <- c(small, col_name)}
      if(col_name == "Varatio") if(abs(max(orig$Varatio - new$Varatio)) < 5) msg <- "Varatio not the same, but within 5"
      if(col_name == "OutRec") {
        if(all(abs(orig$OutRow - new$OutRow) < 2) &
           all(abs(orig$OutCol - new$OutCol) < 2)) msg <- "Out Rec not all the same, but all within 1 cell"}
      if(col_name == "InRec") {
        if(all(abs(orig$InRow - new$InRow) < 2) &
           all(abs(orig$InCol - new$InCol) < 2)) msg <- "In Rec not all the same, but all within 1 cell"}
      if(col_name == "PitRec") {
        if(all(abs(orig$PitRow - new$PitRow) < 2) &
           all(abs(orig$PitCol - new$PitCol) < 2)) msg <- "Pit Rec not all the same, but all within 1 cell"}
      if(col_name == "OutElev") if(abs(max(orig$OutElev - new$OutElev)) < 0.01) {msg <- "Out elev not all the same, but within 0.01"; small <- c(small, col_name)}
      message(msg)
      z[[col_name]] <- which(dplyr::pull(orig, col_name) != dplyr::pull(new, col_name))
    }
  }
  if(length(z) == 0) message("Identical")
  return(z)
}

shedno_update <- function(w_stats) {
  w_stats <- w_stats %>%
    arrange(pit_elev, varatio) %>%
    mutate(shedno_orig = shedno,
           shedno = 1:n(),
           next_pit_orig = next_pit,
           becomes_orig = becomes) %>%
    select(-next_pit, -becomes)

  w_stats <- w_stats %>%
    left_join(select(w_stats, shedno_orig, becomes = shedno),
              by = c("becomes_orig" = "shedno_orig")) %>%
    mutate_cond(is.na(becomes), becomes = 0) %>%
    left_join(select(w_stats, shedno_orig, next_pit = shedno),
              by = c("next_pit_orig" = "shedno_orig")) %>%
    mutate_cond(is.na(next_pit), next_pit = 0) %>%
    select(-next_pit_orig, -becomes_orig, -shedno_orig)

  return(w_stats)
}

#' @export
shedno_update2 <- function(w_stats, dem = NULL) {

  if(is.null(dem)) {
    w_stats <- w_stats %>%
      mutate(sort = 1:n()) %>%
      arrange(ShedArea, PitArea, PitElev) %>%
      mutate(shedno_orig = ShedNo,
             ShedNo = 1:n())
  } else {
    sheds <- tibble::tibble(ShedNo = unique(dem$ShedNo)) %>%
      mutate(i = 1:n())

    w_stats <- w_stats %>%
      mutate(sort = 1:n()) %>%
      left_join(sheds, by = "ShedNo") %>%
      mutate(shedno_orig = ShedNo,
             ShedNo = i) %>%
      select(-i)
  }

  index <- w_stats

  if("Becomes" %in% names(w_stats)){
    w_stats <- mutate(w_stats, becomes_orig = Becomes) %>%
      select(-Becomes) %>%
      left_join(select(index, shedno_orig, Becomes = ShedNo),
                by = c("becomes_orig" = "shedno_orig")) %>%
      mutate_cond(is.na(Becomes), Becomes = 0) %>%
      select(-becomes_orig)
  }

  if("DrainsTo" %in% names(w_stats)) {
    w_stats <- mutate(w_stats, out_shed_orig = DrainsTo) %>%
      select(-DrainsTo) %>%
      left_join(select(index, shedno_orig, DrainsTo = ShedNo),
                by = c("out_shed_orig" = "shedno_orig")) %>%
      mutate_cond(is.na(DrainsTo), DrainsTo = 0) %>%
      select(-out_shed_orig)
  }

  if("EndPit" %in% names(w_stats)) {
    w_stats <- mutate(w_stats, end_pit_orig = EndPit) %>%
      select(-EndPit) %>%
      left_join(select(index, shedno_orig, EndPit = ShedNo),
                by = c("end_pit_orig" = "shedno_orig")) %>%
      mutate_cond(is.na(EndPit), EndPit = 0) %>%
      select(-end_pit_orig)
  }

  w_stats <- arrange(w_stats, sort) %>%
    select(-shedno_orig, -sort)

  return(w_stats)
}
