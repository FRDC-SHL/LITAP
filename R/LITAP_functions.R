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

trace_flow2 <- function(cell, drec) {
  track <- cell
  end <- FALSE
  if(!is.na(cell)){
    while(!end){
      # cell1 <- dplyr::pull(db, drec)[cell] # slooooow
      # cell2 <- dplyr::slice(db, cell) %>% dplyr::pull(drec) # slooooow
      # cell3 <- db[['drec']][cell] # slooooow
      cell <- drec[cell] # Get next cells (drec must be sorted by seqno!)
      #cell <- db$drec[cell] # Get next cells (db must be sorted by seqno!)
      if(!is.na(cell)) { # Otherwise is an edge cell
        if(cell %in% track) end <- TRUE # In a circular track or Pit
        if(cell != track[length(track)]) track <- c(track, cell) # If not simply starting and ending with the same pit cell, keep final cell
        #if(shed && !is.na(db$shedno[db$seqno == cell])) end <- TRUE # If looking for watersheds, end if meet with one
      } else end <- TRUE
    }
    return(track)
  }
}

trace_flow_fast <- function(cell, drec) {
  track <- vector()
  i <- 1
  track[i] <- cell
  repeat {
    i <- i + 1
    #if(track[i-1] == 1) browser()
    #cat(track[i-1], sep = "\n")
    #cat(drec[track[i-1]], sep = "\n")
    if(track[i-1] == drec[track[i-1]]) break
    if(drec[track[i-1]] %in% track) break
    track[i] <- drec[track[i-1]]
  }
  track
}

trace_flow_fast_circ <- function(cell, drec) {
  track <- vector()
  i <- 1
  track[i] <- cell

  repeat {
    i <- i + 1
    if(track[i-1] == drec[track[i-1]]) break
    if(drec[track[i-1]] %in% track) return(TRUE)
    track[i] <- drec[track[i-1]]
  }

  FALSE
}


trace_flow_all <- function(cells, drec) {
  m <- matrix(ncol = length(cells))
  m[1, ] <- cells

  i <- 1
  repeat {
    i <- i + 1
    if(all(m[i-1, ] == drec[m[i-1, ]], na.rm = TRUE)) break
    m <- rbind(m, drec[m[i-1, ]])
  }
  m
}


# Setup function to trace_flow_fast for single cells (loop)
# - v are variaible variables hahah
# - s are static variables
trace_single <- function(seqno, drec, loop_func, s, v, ...) {

  for(i in seqno) {
    t <- trace_flow_fast(cell = i, drec = drec)
    #message(i)
    #if(t[1] == 562) browser()
    #cat(t, sep = ", ")

    v <- loop_func(t, s, v, ...)
  }
  v
}

# Setup function to trace_flow_all for matrix of cells (loop over group)
trace_matrix <- function(seqno, drec, loop_func, s, v, ...) {

  n_cells <- 250
  seqno_track_groups <- seq(n_cells, length(seqno) + n_cells - 1, by = n_cells)
  for(a in seqno_track_groups) {
    seqno_sub <- seqno[(a-(n_cells-1)):a]
    # Remove extra cells on last batch
    if(a == seqno_track_groups[length(seqno_track_groups)]) {
      seqno_sub <- seqno_sub[!is.na(seqno_sub)]
    }

    track_group <- trace_flow_all(cells = seqno_sub, drec)

    for(b in seq_along(seqno_sub)){
      t <- track_group[, b]
      v <- loop_func(t, s, v, ...)
    }
  }
  v
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
    shedno <- w_stats$drains_to[w_stats$shedno == shedno] # Get next cells
    if(shedno %in% track) end <- TRUE # In a circular track
    if(shedno != track[length(track)]) track <- c(track, shedno) # If not simply starting and ending with the same shed
  }
  return(track)
}



# Relabel seqno
rename_seqno <- function(x, index) {
  if(length(x) > 0){
    x <- tibble::enframe(x, name = NULL) %>%
      dplyr::rename(seqno_buffer = value) %>%
      dplyr::left_join(index, by = "seqno_buffer") %>%
      dplyr::pull(seqno)
  }
  return(x)
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

#' Get data from neighbouring cells
#'
#' Data MUST be arranged by seqno
#'
#' Note that this isn't always the way the original LandMapR program defined
#' neighbours (it depends on the specific step)
#'
#' 1 2 3
#' 4 5 6
#' 7 8 9
#' @noRd
#'
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
    db_sub <- tidyr::separate(db_sub, n, into = c("type", "n"), sep = -1, convert = TRUE)
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
  for(a in col) db_sub[, paste0(a, "_next")] <- db[db_sub$seqno + sapply(db_sub$ddir, calc_seq, max_cols = max_cols), a]
  return(db_sub)
}


finddir2 <- function(db) {
  db %>%
    dplyr::mutate(elev_diff = as.double(elev - elev_n)) %>%
    dplyr::mutate(elev_diff = dplyr::if_else(n %in% c(1, 3, 7, 9),
                                             elev_diff/sqrt(2),
                                             elev_diff)) %>%
    dplyr::group_by(seqno) %>%
    dplyr::mutate(elev_diff = tidyr::replace_na(elev_diff, 0),
                  max_slope = max(elev_diff, na.rm = TRUE),
                  ddir = n[elev_diff == max_slope & max_slope > 0][1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-elev_n, -max_slope, -n, -elev_diff) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ddir = as.numeric(stringr::str_extract(ddir, "[0-9]{1}")),
                  ddir = replace(ddir, is.na(ddir) & !is.na(elev), 5),
                  flatcell = (ddir == 5))
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

    # Take local direction (ddir) with steepest slope
    if(nrow(n) > 0) {
      ddir <- n$index[n$slope == max(n$slope, na.rm = TRUE)]
    } else ddir <- 5

    # What to do with more than one possible flow direction?
    # For now, assign to lowest direction code (ddir)
    ddir <- min(ddir)

    return(ddir)

  } else {
    # If on an edge, return NA
    return(NA)
  }
}

flatout2 <- function(db, verbose = FALSE){

  # Confirm cell is NOT missing (else return NA)
  if(nrow(n) > 0 && !is.na(n$elev[5])){

    # Add in local flow directions
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ddir), by = "seqno")

    # Confirm that IS flat (else return old ddir)
    if(n$ddir[5] == 5) {

      n <- n[!is.na(n$elev), ]
      n$slope <- n$elev[n$index == 5] - n$elev

      # Find FIRST neighbour with same elevation and valid flow direction that isn't right back
      new_n <- n[n$slope == 0 & n$ddir !=5 & n$index != (10 - n$ddir) & n$index != 5, ][1, ]

      # new_n <- n %>%
      #   dplyr::filter(slope == 0,          # Same elevation
      #                 ddir != 5,           # Isn't flat itself
      #                 index != 10 - ddir,  # Doesn't flow directly back
      #                 index != 5)          # Isn't the cell of interest
      # new_n <- new_n[1, ]

      # If there is a candidate, return the new option, else return the original
      if(nrow(new_n) > 0 && !is.na(new_n$index)) return(new_n$index)  else return(n$ddir[n$index == 5])
    } else return(n$ddir[n$index == 5])
  } else {
    # If on an edge, return NA
    return(NA)
  }
}

flatout <- function(n, db, verbose = FALSE){

  # Confirm cell is NOT missing (else return NA)
  if(nrow(n) > 0 && !is.na(n$elev[5])){

    # Add in local flow directions
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ddir), by = "seqno")

    # Confirm that IS flat (else return old ddir)
    if(n$ddir[5] == 5) {

      n <- n[!is.na(n$elev), ]
      n$slope <- n$elev[n$index == 5] - n$elev

      # Find FIRST neighbour with same elevation and valid flow direction that isn't right back
      new_n <- n[n$slope == 0 & n$ddir !=5 & n$index != (10 - n$ddir) & n$index != 5, ][1, ]

      # new_n <- n %>%
      #   dplyr::filter(slope == 0,          # Same elevation
      #                 ddir != 5,           # Isn't flat itself
      #                 index != 10 - ddir,  # Doesn't flow directly back
      #                 index != 5)          # Isn't the cell of interest
      # new_n <- new_n[1, ]

      # If there is a candidate, return the new option, else return the original
      if(nrow(new_n) > 0 && !is.na(new_n$index)) return(new_n$index)  else return(n$ddir[n$index == 5])
    } else return(n$ddir[n$index == 5])
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

    # Confirm that IS flat (else return old ddir)
    if(n$ddir[5] == 5) {

      n$slope <- n$elev[n$index == 5] - n$elev
      n$newdir = (n$slope == 0 & n$index != (10 - n$ddir))
      n$pit = (n$ddir == 5)
      n$flowin = (n$index == (10 - n$ddir))

      # n <- n %>%
      #   dplyr::mutate(slope = elev[index == 5] - elev) %>%
      #   dplyr::mutate(newdir = slope == 0 & index != 10 - ddir,   # same elev and neighbour doesn't point back in
      #                 pit = ddir == 5,                            # neighbour is pit (unncessary?)
      #                 flowin = index == 10 - ddir)   # same OR higher elev neighbour DOES point in

      n2 <- n[!is.na(n$newdir) & !is.na(n$pit) & !is.na(n$flowin) & n$index != 5, ]
      # n2 <- dplyr::filter(n, !is.na(newdir), !is.na(pit), !is.na(flowin), index != 5)

      # If this flat cell points to another flat cell AND other cells point in, we can connect the flow
      if(any(n2$pit) & any(n2$flowin) & any(n2$newdir)){
        # If more than one, take the first
        ddir <- n$index[n$newdir == TRUE & !is.na(n$newdir)][1]
        # ddir <- dplyr::filter(n, newdir == TRUE) %>%
        #   .$index
        # return(ddir[1])
        return(ddir)
      } else return(n$ddir[n$index == 5])
    } else return(n$ddir[n$index == 5])
  } else return(NA)
}

neighbour_pit <- function(n, db, verbose = FALSE) {

  # Add in local flow directions
  n <- dplyr::left_join(n, db, by = "seqno")

  # Confirm that IS flat (else return old ddir)
  n <- n[n$index != 5 & n$ddir == 5 & !is.na(n$index) & !is.na(n$ddir), ]
  # n <- n %>%
  #   dplyr::filter(index != 5, ddir == 5)

  if(nrow(n) != 0) return(n[1,])
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
      pit2 <- dplyr::filter(w_stats, shedno == pit1$drains_to) %>%
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

  db_group1 <- db %>%
    dplyr::select(group1, seqno, elev) %>%
    dplyr::group_by(group1) %>%
    tidyr::nest(db_sub1 = tidyr::everything())

  db_group2 <- db %>%
    dplyr::select(group2, seqno, elev) %>%
    dplyr::group_by(group2) %>%
    tidyr::nest(db_sub2 = tidyr::everything())

  db_group3 <- db %>%
    dplyr::select(group3, seqno, elev) %>%
    dplyr::group_by(group3) %>%
    tidyr::nest(db_sub3 = tidyr::everything())

  db_group4 <- db %>%
    dplyr::select(group4, seqno, elev) %>%
    dplyr::group_by(group4) %>%
    tidyr::nest(db_sub4 = tidyr::everything())

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


get_run <- function(file) {
  basename(file) %>%
    stringr::str_split(stringr::regex("Elev", ignore_case = TRUE)) %>%
    unlist(.) %>%
    .[1]
}

# Return closes direction leading to a particular cell
get_dir <- function(row, col, row_f, col_f, ddir_opts = 1:9) {
  h <- col_f - col
  v <- row_f - row
  a <- atan(abs(h)/abs(v)) * 180/pi

  if(h == 0 & v == 0) {
    l <- 5
  } else if(h > 0 & v <= 0) { # Move to Upper right
    if(a >= 67.5) l <- 6
    if(a >= 22.5 & a < 67.5) l <- 9
    if(a < 22.5) l <- 8
  } else if(h > 0 & v > 0) { # Lower right
    if(a >= 67.5) l <- 6
    if(a >= 22.5 & a < 67.5) l <- 3
    if(a < 22.5) l  <- 2
  } else if(h <= 0 & v <= 0) { # Upper left
    if(a >= 67.5) l  <- 4
    if(a >= 22.5 & a < 67.5) l <- 7
    if(a < 22.5) l <- 8
  } else if(h <=0 & v >= 0) { # Lower left
    if(a >= 67.5) l <- 4
    if(a >= 22.5 & a < 67.5) l <- 1
    if(a < 22.5) l <- 2
  }

  # If best direction not an option, get next best
  # Prioritorize smaller seqno
  if(!(l %in% ddir_opts) & l != 5) {
    closest <- list("1" = c(4, 2, 7, 3, 8, 6, 9),
                    "2" = c(1, 3, 4, 6, 7, 9, 8),
                    "3" = c(6, 2, 9, 1, 8, 4, 7),
                    "4" = c(7, 1, 8, 2, 9, 3, 6),
                    "6" = c(9, 3, 8, 2, 7, 1, 4),
                    "7" = c(8, 4, 9, 1, 6, 2, 3),
                    "8" = c(7, 9, 4, 6, 1, 3, 2),
                    "9" = c(8, 6, 7, 3, 4, 2, 1))

    c <- closest[as.character(l)][[1]]
    l <- c[c %in% ddir_opts][1]
  }
  return(l)
}

