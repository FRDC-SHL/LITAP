load_orig <- function(file) {
  if(stringr::str_detect(file, stringr::regex("dem.dbf", ignore_case = TRUE))) {
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(ldir = ddir, upslope_n = upslope, local_shed = shedno, global_shed = shednow) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, local_shed, global_shed)
  } else if (stringr::str_detect(file, stringr::regex("(pit.dbf)|(pond.dbf)", ignore_case = TRUE))){
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev = pourelev, shed_area = shedarea, pit_area = pitarea,
                    pit_vol = pitvol, pit_elev = pitelev, varatio,
                    pit_seqno = pitrec, pit_row = pitrow, pit_col = pitcol,
                    out_elev = outelev, out_row = outrow, out_col = outcol,
                    in_row = inrow, in_col = incol,
                    out_shed = drainsto,
                    nextpit, becomes, endpit, edge, final, removed)
  } else if (stringr::str_detect(file, stringr::regex("fill.dbf", ignore_case = TRUE))){
    f <- foreign::read.dbf(file) %>%
      tibble::as_tibble() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev = pourelev, shed_area = shedarea, pit_area = pitarea,
                    pit_vol = pitvol, pit_elev = pitelev, varatio,
                    pit_seqno = pitrec, pit_row = pitrow, pit_col = pitcol,
                    out_elev = outelev, out_row = outrow, out_col = outcol,
                    in_row = inrow, in_col = incol,
                    out_shed = drainsto,
                    nextpit, becomes, endpit, edge, final)
  }
  return(f)
}

load_new <- function(file){
  if(stringr::str_detect(file, stringr::regex("db_global_shed.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, local_shed, global_shed)
  } else if(stringr::str_detect(file, stringr::regex("db_fill.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, local_shed, global_shed, fill_shed)
  } else if(stringr::str_detect(file, stringr::regex("db_ups.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, initial_shed)
  } else if(stringr::str_detect(file, stringr::regex("db_local_shed.rds", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::select(seqno, row, col, elev, ldir, drec, upslope_n, local_shed)
  } else if(stringr::str_detect(file, stringr::regex("w_stats_global", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev,
                    varatio, pit_seqno, pit_row, pit_col,
                    out_shed, out_elev, out_seqno, in_row, in_col, becomes,
                    removed, nextpit, edge_pit) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3), varatio = round(varatio, 2))
  } else if(stringr::str_detect(file, stringr::regex("w_stats_fill", ignore_case = TRUE))) {
    f <- readr::read_rds(file) %>%
      dplyr::ungroup() %>%
      dplyr::rename_all(tolower) %>%
      # dplyr::select(shedno, pour_elev, shed_area, pit_area, pit_vol, pit_elev,
      #               varatio, curr_vol, curr_mm2fl, pit_seqno, pit_row, pit_col,
      #               out_shed, out_elev, out_seqno, in_row, in_col, edge_pit, becomes) %>%
      dplyr::mutate(pit_vol = round(pit_vol, 3),
                    varatio = round(varatio, 2),
                    curr_vol = round(curr_vol, 3),
                    curr_mm2fl = round(curr_mm2fl, 3))
  }
  return(f)
}

compare <- function(orig, new) {
  z <- list()
  for(n in intersect(names(orig), names(new))){
    if(!all(dplyr::pull(orig, n) == dplyr::pull(new, n))) {
      msg <- paste0("Not all ", n, " the same")
      if(n == "pit_area") if(abs(max(orig$pit_area - new$pit_area)) < 3) msg <- "Pit area not the same, but within 3 cells"
      if(n == "pit_vol") if(abs(max(orig$pit_vol - new$pit_vol)) < 0.1) msg <- "Pit Vol not the same, but within 0.10"
      if(n == "varatio") if(abs(max(orig$varatio - new$varatio)) < 0.2) msg <- "Varatio not the same, but within 0.20"
      message(msg)
      z[[n]] <- which(dplyr::pull(orig, n) != dplyr::pull(new, n))
    }
  }
  if(length(z) == 0) message("Identical")
  return(z)
}
