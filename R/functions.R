trace_flow <- function(cell, db) {
  track <- cell
  end <- FALSE
  if(!is.na(cell)){
    while(!end){
      cell <- db$drec[db$seqno == cell] # Get next cells
      if(!is.na(cell)) { # Otherwise is an edge cell
        if(cell %in% track) end <- TRUE # In a circular track or PIt
        if(cell != track[length(track)]) track <- c(track, cell) # If not simply starting and ending with the same pit cell, keep final cell
        #if(shed && !is.na(db$shedno[db$seqno == cell])) end <- TRUE # If looking for watersheds, end if meet with one
      } else end <- TRUE
    }
    return(track)
  }
}

trace_flow2 <- function(cell, db) {
  track <- cell
  end <- FALSE
  if(!is.na(cell)){
    while(!end){
      cell <- db$drec[cell] # Get next cells (db must be sorted by seqno!)
      if(!is.na(cell)) { # Otherwise is an edge cell
        if(cell %in% track) end <- TRUE # In a circular track or Pit
        if(cell != track[length(track)]) track <- c(track, cell) # If not simply starting and ending with the same pit cell, keep final cell
        #if(shed && !is.na(db$shedno[db$seqno == cell])) end <- TRUE # If looking for watersheds, end if meet with one
      } else end <- TRUE
    }
    return(track)
  }
}

trace_flow3 <- function(cell, db) {
  track <- cell
  end <- FALSE
  if(!is.na(cell)){
    while(!end){
      cell <- db$drec_shed[cell] # Get next cells (db must be sorted by seqno!)
      if(!is.na(cell)) { # Otherwise is an edge cell
        if(cell %in% track) end <- TRUE # In a circular track or Pit
        if(cell != track[length(track)]) track <- c(track, cell) # If not simply starting and ending with the same pit cell, keep final cell
        #if(shed && !is.na(db$shedno[db$seqno == cell])) end <- TRUE # If looking for watersheds, end if meet with one
      } else end <- TRUE
    }
    return(track)
  }
}

trace_pits <- function(shedno, w_stats) {
  track <- shedno
  end <- FALSE
  while(!end){
    shedno <- w_stats$out_shed[w_stats$shedno == shedno] # Get next cells
    if(shedno %in% track) end <- TRUE # In a circular track
    if(shedno != track[length(track)]) track <- c(track, shedno) # If not simply starting and ending with the same shed
  }
  return(track)
}




rename_seqno <- function(x, index) {
  if(length(x) > 0){
    x <- dplyr::as_tibble(x) %>%
      dplyr::rename(seqno_buffer = value) %>%
      dplyr::left_join(index, by = "seqno_buffer") %>%
      dplyr::pull(seqno)
  }
  return(x)
}

save_shed <- function(file_out, obj, name){
  if(stringr::str_detect(name, ".rds$")) readr::write_rds(obj, paste0(file_out, "_", name))
  if(stringr::str_detect(name, ".csv$")) readr::write_csv(obj, paste0(file_out, "_", name))
  if(stringr::str_detect(name, ".dbf$")) foreign::write.dbf(obj, paste0(file_out, "_", name))
}

read_shed <- function(file_out, name){
  readr::read_rds(paste0(file_out, "_", name , ".rds"))
}

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
          if(any(is.na(dbf))) dbf <- dplyr::mutate_if(dbf, is.na, ~0)
          save_shed(locs$dbf_out, as.data.frame(dbf), paste0(name_dbf, ".dbf"))
        }
      }
    }
  }
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
                          DrainsTo = out_shed, NextPit = next_pit,
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
    dplyr::rename(seqno_buffer = seqno,
                  drec_buffer = drec)

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
    db <- db %>%
      dplyr::mutate(drec = rename_seqno(drec_buffer, index))
      #upslope = purrr::map(upslope_buffer, ~ rename_seqno(.x, index)))
    return(db)
  }

}

adj <- function(cell, nrows, ncols, index = NULL) {
  a <- c(cell + seq(-ncols-1, -ncols+1, 1),
         cell + seq(-1, 1, 1),
         cell + seq(ncols-1, ncols+1, 1))
  if(any(a <= 0) | any(a > (nrows * ncols))) a <- NA
  if(!is.null(index)) a <- a[c(7,8,9,4,5,6,1,2,3)][index]
  return(a)
}

neighbours <- function(a, db) {
  if(all(!is.na(a))) {
    n <- db[db$seqno %in% a, ]
    if(nrow(n) == 9){
      if(!("index" %in% names(n))) {
        n$index <- c(7, 8, 9, 4, 5, 6, 1, 2, 3)
      }
      return(tibble::as.tibble(n))
    } else return(tibble::tibble())
  } else return(tibble::tibble())
}

# Cell on the edge of a watershed?
shed_edge <- function(a, db, w) {
  any(db$shedno[a] != w)
}

# Cell on the edge of the map?
edge_pit <- function(a, db) {
  any(is.na(db$elev[a]))
}

nb_values <- function(db, max_cols, col = "elev", db_sub = NULL, format = "long") {

  if(is.null(db_sub)) db_sub <- db

  for(i in 1:9){
    if(i == 1) seqno <- db_sub$seqno + (max_cols - 1)
    if(i == 2) seqno <- db_sub$seqno + max_cols
    if(i == 3) seqno <- db_sub$seqno + (max_cols + 1)
    if(i == 4) seqno <- db_sub$seqno - 1
    if(i == 5) seqno <- db_sub$seqno
    if(i == 6) seqno <- db_sub$seqno + 1
    if(i == 7) seqno <- db_sub$seqno - (max_cols + 1)
    if(i == 8) seqno <- db_sub$seqno - max_cols
    if(i == 9) seqno <- db_sub$seqno - (max_cols - 1)


    seqno[seqno < 1 | seqno > max(db$seqno)] <- NA

    for(a in col) db_sub[, paste0(a, "_n", i)] <- db[seqno, a]
  }

  if(format == "long") {
    db_sub <- tidyr::gather(db_sub, n, value, dplyr::matches(paste0("(", paste0(col, collapse = "_n[0-9]{1})|("), "_n[0-9]{1})")))
    db_sub <- tidyr::separate(db_sub, n, into = c("type", "n"), sep = -2, convert = TRUE)
    db_sub <- dplyr::mutate(db_sub, n = as.numeric(n))
    db_sub <- tidyr::spread(db_sub, type, value)
  }

  return(db_sub)
}

calc_seq <- function(d, max_cols) {
  if(is.na(d)) return(NA)
  if(d == 1) return(max_cols - 1)
  if(d == 2) return(max_cols)
  if(d == 3) return(max_cols + 1)
  if(d == 4) return(-1)
  if(d == 5) return(0)
  if(d == 6) return(1)
  if(d == 7) return(-(max_cols + 1))
  if(d == 8) return(-max_cols)
  if(d == 9) return(-(max_cols - 1))
}

flow_values <- function(db, max_cols, col = "elev", db_sub = NULL) {
  if(is.null(db_sub)) db_sub <- db
  for(a in col) db_sub[, paste0(a, "_next")] <- db[db_sub$seqno + sapply(db_sub$ldir, calc_seq, max_cols = max_cols), a]
  return(db_sub)
}


finddir2 <- function(db) {
  db %>%
    dplyr::mutate(elev_diff = elev - elev_n) %>%
    dplyr::mutate(elev_diff = dplyr::if_else(n %in% c(1, 3, 7, 9),
                                             elev_diff/sqrt(2), elev_diff)) %>%
    dplyr::group_by(seqno) %>%
    dplyr::mutate(max_slope = max(elev_diff, na.rm = TRUE),
                  ldir = n[elev_diff == max_slope & max_slope > 0 & !is.na(elev_diff)][1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-elev_n, -max_slope, -n, -elev_diff) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ldir = as.numeric(stringr::str_extract(ldir, "[0-9]{1}")),
                  ldir = replace(ldir, is.na(ldir) & !is.na(elev), 5),
                  flatcell = (ldir == 5))
}


# Calculate the direction of flow for an individual cell given its neighbours
finddir <- function(n, verbose = FALSE){

  if(nrow(n) > 0 && !is.na(n$elev[n$index == 5])) {
    if(verbose) matrix(n$seqno, nrow = 3, ncol = 3, byrow = TRUE)
    if(verbose) matrix(n$elev, nrow = 3, ncol = 3, byrow = TRUE)
    if(verbose) matrix(n$index, nrow = 3, ncol = 3, byrow = TRUE)

    # Compare elevations, Reduce diagonal slopes by square-root(2)
    n$slope <- n$elev[n$index == 5] - n$elev
    n$slope[n$index %in% c(1, 3, 7, 9)] <- n$slope[n$index %in% c(1, 3, 7, 9)] / sqrt(2)
    n <- n[!is.na(n$slope) & n$slope > 0,]

    # Take local direction (ldir) with steepest slope
    if(nrow(n) > 0) {
      ldir <- n$index[n$slope == max(n$slope, na.rm = TRUE)]
    } else ldir <- 5

    # What to do with more than one possible flow direction?
    # For now, assign to lowest direction code (ldir)
    ldir <- min(ldir)

    return(ldir)

  } else {
    # If on an edge, return NA
    return(NA)
  }
}

flatout2 <- function(db, verbose = FALSE){

  # Confirm cell is NOT missing (else return NA)
  if(nrow(n) > 0 && !is.na(n$elev[5])){

    # Add in local flow directions
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ldir), by = "seqno")

    # Confirm that IS flat (else return old ldir)
    if(n$ldir[5] == 5) {

      n <- n[!is.na(n$elev), ]
      n$slope <- n$elev[n$index == 5] - n$elev

      # Find FIRST neighbour with same elevation and valid flow direction that isn't right back
      new_n <- n[n$slope == 0 & n$ldir !=5 & n$index != (10 - n$ldir) & n$index != 5, ][1, ]

      # new_n <- n %>%
      #   dplyr::filter(slope == 0,          # Same elevation
      #                 ldir != 5,           # Isn't flat itself
      #                 index != 10 - ldir,  # Doesn't flow directly back
      #                 index != 5)          # Isn't the cell of interest
      # new_n <- new_n[1, ]

      # If there is a candidate, return the new option, else return the original
      if(nrow(new_n) > 0 && !is.na(new_n$index)) return(new_n$index)  else return(n$ldir[n$index == 5])
    } else return(n$ldir[n$index == 5])
  } else {
    # If on an edge, return NA
    return(NA)
  }
}

flatout <- function(n, db, verbose = FALSE){

  # Confirm cell is NOT missing (else return NA)
  if(nrow(n) > 0 && !is.na(n$elev[5])){

    # Add in local flow directions
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ldir), by = "seqno")

    # Confirm that IS flat (else return old ldir)
    if(n$ldir[5] == 5) {

      n <- n[!is.na(n$elev), ]
      n$slope <- n$elev[n$index == 5] - n$elev

      # Find FIRST neighbour with same elevation and valid flow direction that isn't right back
      new_n <- n[n$slope == 0 & n$ldir !=5 & n$index != (10 - n$ldir) & n$index != 5, ][1, ]

      # new_n <- n %>%
      #   dplyr::filter(slope == 0,          # Same elevation
      #                 ldir != 5,           # Isn't flat itself
      #                 index != 10 - ldir,  # Doesn't flow directly back
      #                 index != 5)          # Isn't the cell of interest
      # new_n <- new_n[1, ]

      # If there is a candidate, return the new option, else return the original
      if(nrow(new_n) > 0 && !is.na(new_n$index)) return(new_n$index)  else return(n$ldir[n$index == 5])
    } else return(n$ldir[n$index == 5])
  } else {
    # If on an edge, return NA
    return(NA)
  }
}

# PROCEDURE flatin
# ************************************************************************
#   * Procedure to assign a logical flow direction to flat cells within
# * depressions. Such cells have no possible flow paths to an outlet cell
# * which flows to a lower elevation. The procedure works by going to each
# * neighbor cell of a current flat cell to determine if any of the
# * neighbor cells:
#   * (a) are at the same elevation as the current flat cell
# * (i.e. are also flat)
# * (b) if yes to (a) then does the flat neighbor cell point back into the
# * current flat cell?
# * (c) if no to (b) then the current flat cell can be assigned a local
# * flow direction pointing into the flat neighbor cell if (d) below is
# * also true.
# * (d) are there any other neighbor cells that have a valid flow direction
# * that points into the current flat cell.
# * This last check ensures that flow into depressions starts at the edge
# * of the depression where cells with valid flow directions flow into the
# * flat cells at the edge of the depression.  Flow directions are assigned
# * progressively to other flat cells closer to the centre of a depression
# * as outer flat cells are assigned valid flow directions.

flatin <- function(n, db, verbose = FALSE) {

  if(nrow(n) > 0 && !is.na(n$elev[5])){  # Confirm cell is NOT missing (else return NA)

    # Add in local flow directions
    n <- dplyr::left_join(n, db, by = "seqno")

    # Confirm that IS flat (else return old ldir)
    if(n$ldir[5] == 5) {

      n$slope <- n$elev[n$index == 5] - n$elev
      n$newdir = (n$slope == 0 & n$index != (10 - n$ldir))
      n$pit = (n$ldir == 5)
      n$flowin = (n$index == (10 - n$ldir))

      # n <- n %>%
      #   dplyr::mutate(slope = elev[index == 5] - elev) %>%
      #   dplyr::mutate(newdir = slope == 0 & index != 10 - ldir,   # same elev and neighbour doesn't point back in
      #                 pit = ldir == 5,                            # neighbour is pit (unncessary?)
      #                 flowin = index == 10 - ldir)   # same OR higher elev neighbour DOES point in

      n2 <- n[!is.na(n$newdir) & !is.na(n$pit) & !is.na(n$flowin) & n$index != 5, ]
      # n2 <- dplyr::filter(n, !is.na(newdir), !is.na(pit), !is.na(flowin), index != 5)

      # If this flat cell points to another flat cell AND other cells point in, we can connect the flow
      if(any(n2$pit) & any(n2$flowin) & any(n2$newdir)){
        # If more than one, take the first
        ldir <- n$index[n$newdir == TRUE & !is.na(n$newdir)][1]
        # ldir <- dplyr::filter(n, newdir == TRUE) %>%
        #   .$index
        # return(ldir[1])
        return(ldir)
      } else return(n$ldir[n$index == 5])
    } else return(n$ldir[n$index == 5])
  } else return(NA)
}

#' @import magrittr
neighbour_pit <- function(n, db, verbose = FALSE) {

  # Add in local flow directions
  n <- dplyr::left_join(n, db, by = "seqno")

  # Confirm that IS flat (else return old ldir)
  n <- n[n$index != 5 & n$ldir == 5 & !is.na(n$index) & !is.na(n$ldir), ]
  # n <- n %>%
  #   dplyr::filter(index != 5, ldir == 5)

  if(nrow(n) != 0) return(n[1,])
}

calc_upslope <- function(cell, db) {
  new_upslope <- db %>%
    dplyr::filter(drec == cell) %>%
    .$upslope %>%
    list(., cell) %>%
    unlist() %>%
    unique()
  return(new_upslope)
}

# G. Grothendieck
# https://stackoverflow.com/a/34096575/3362144

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}


find_lowest <- function(w, w_stats, final_pits, removed, verbose = FALSE) {

  # # Mark final pits
  # w_stats <- dplyr::mutate(w_stats,
  #                          final = shedno %in% final_pits,
  #                          removed = shedno %in% removed)

  # Starting point

  w_pits <- trace_pits(w, w_stats)
  wp <- w_pits[1]
  lowest <- dplyr::filter(w_stats, shedno == wp)
  visited <- vector()
  end <- FALSE

  while(!end) {
    if(!(wp %in% visited)) {
      visited <- c(visited, wp)

      pit1 <- dplyr::filter(w_stats, shedno == wp) %>%
        dplyr::mutate(at_final = FALSE)
      pit2 <- dplyr::filter(w_stats, shedno == pit1$out_shed) %>%
        dplyr::mutate(at_final = FALSE)

      if(pit1$pit_elev < lowest$pit_elev) lowest <- pit1

      if(verbose) message("  Current pit: ", wp)
      if(verbose) message("    Pit 1: ", pit1$shedno)
      if(verbose) message("    Pit 2: ", pit2$shedno)

      wp <- pit2$shedno

      if(pit2$final) {
        lowest <- pit1
        end <- TRUE
        if(verbose) message("    Pit 2 already FINAL pit")
      } else {

        if(pit2$shedno %in% visited) {
          end <- TRUE
        } else {
          # Technically this line here makes it impossible to go more than one pit down...
          visited <- c(visited, pit2$shedno)

          if(pit2$pit_elev < lowest$pit_elev) lowest <- pit2

          if(pit2$pour_elev < pit1$pour_elev) {
            lowest <- pit2
          } else {

            if(lowest$final) {
              # If lowest pit already final pit, use current pit
              lowest <- dplyr::filter(w_stats, shedno == w_pits[1]) %>%
                dplyr::mutate(at_final = TRUE)
            } else {
              lowest <- lowest %>%
                dplyr::mutate(at_final = FALSE)
            }

            end <- TRUE
          }

        }
      }
    } else end <- TRUE
  }
  if(verbose) message("    Lowest pit: ", lowest$shedno)
  return(lowest)
}

divide <- function(db, size = 12){

  col_g <- cut(db$col, ceiling(max(db$col) / size), labels = FALSE)
  row_g <- cut(db$row, ceiling(max(db$col) / size), labels = FALSE)

  db$group1 <- row_g + (ceiling(max(db$row) / size) * (col_g - 1))
  db$group2 <- NA
  db$group2[(db$row > (size/2))] <- db$group1[db$row < (max(db$row) - ((size/2) - 1))]
  db$group3 <- NA
  db$group3[(db$col > (size/2))] <- db$group1[db$col < (max(db$col) - ((size/2) - 1))]
  db$group4 <- NA
  db$group4[(db$row > (size/2)) & (db$col > (size/2))] <- db$group1[db$row < (max(db$row) - ((size/2) - 1)) & db$col < (max(db$col) - ((size/2) - 1))]


  # ggplot(db, aes(x = row, y = col)) +
  #   geom_rect(xmin = 0, xmax = max(db$row), ymax = 0, ymin = max(db$col)) +
  #   geom_raster(aes(fill = group1)) +
  #   geom_point(x = 10, y = 72)
  #
  #  ggplot(db, aes(x = row, y = col)) +
  #    geom_rect(xmin = 0, xmax = max(db$row), ymax = 0, ymin = max(db$col)) +
  #    geom_raster(aes(fill = group2)) +
  #    geom_point(x = 10, y = 72)


  db_group1 <- db %>%
    dplyr::select(group1, seqno, elev) %>%
    dplyr::group_by(group1) %>%
    tidyr::nest(.key = "db_sub1")

  db_group2 <- db %>%
    dplyr::select(group2, seqno, elev) %>%
    dplyr::group_by(group2) %>%
    tidyr::nest(.key = "db_sub2")

  db_group3 <- db %>%
    dplyr::select(group3, seqno, elev) %>%
    dplyr::group_by(group3) %>%
    tidyr::nest(.key = "db_sub3")

  db_group4 <- db %>%
    dplyr::select(group4, seqno, elev) %>%
    dplyr::group_by(group4) %>%
    tidyr::nest(.key = "db_sub4")

  db_test <- db %>%
    dplyr::left_join(db_group1, by = "group1") %>%
    dplyr::left_join(db_group2, by = "group2") %>%
    dplyr::left_join(db_group3, by = "group3") %>%
    dplyr::left_join(db_group4, by = "group4") %>%
    dplyr::mutate(n1 = purrr::map2(adjacent, db_sub1, ~neighbours(a = .x, db = .y)),
                  n2 = purrr::map2(adjacent, db_sub2, ~neighbours(a = .x, db = .y)),
                  n3 = purrr::map2(adjacent, db_sub3, ~neighbours(a = .x, db = .y)),
                  n4 = purrr::map2(adjacent, db_sub4, ~neighbours(a = .x, db = .y)),
                  n = purrr::pmap(list(n1, n2, n3, n4), most_n)) %>%
                  #n_nrow = purrr::map_dbl(n, nrow)) %>%
    dplyr::select(elev, seqno, row, col, missing, buffer, adjacent, n)
  return(db_test)
}

most_n <- function(n1, n2, n3, n4) {
 return(list(n1, n2, n3, n4)[which.max(lapply(list(n1, n2, n3, n4), nrow))][[1]])
}


#' @import magrittr
get_run <- function(file) {
  basename(file) %>%
    stringr::str_split(stringr::regex("Elev", ignore_case = TRUE)) %>%
    unlist(.) %>%
    .[1]
}
