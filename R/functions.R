flow_plot <- function(db, type = "relief", dir = FALSE, seqno = FALSE, shed = FALSE, shed_type = "shedno",
                      upslope_threshold = NULL,
                      cells = NULL, xlim = NULL, ylim = NULL, upslope = NULL,
                      pits = NULL, p = FALSE) {

  if(!is.null(xlim)) db <- dplyr::filter(db, col >= xlim[1], col <= xlim[2])
  if(!is.null(ylim)) db <- dplyr::filter(db, row >= ylim[1], row <= ylim[2])
  if(!is.null(upslope)) {
    a <- db$upslope[db$seqno == upslope][[1]]
    db <- dplyr::mutate(db, area = seqno %in% a)
  }

  if(type == "relief") {
    r <- raster::raster(nrows=max(db$row), ncols=max(db$col), vals = db$elev)
    r <- raster::terrain(r, opt = c("slope", "aspect"))
    r <- raster::hillShade(slope = r$slope, aspect = r$aspect, angle = 1)
    db$relief <- as.vector(r)
  }

  if(shed == TRUE){
    if(shed_type == "initial" & "initial_shed" %in% names(db)) db$shedno <- db$initial_shed
    if(shed_type == "local" & "local_shed" %in% names(db)) db$shedno <- db$local_shed
    if(shed_type == "global" & "global_shed" %in% names(db)) db$shedno <- db$global_shed
    if(shed_type == "fill" & "fill_shed" %in% names(db)) db$shedno <- db$fill_shed
  }

  if(!is.null(pits)) {
   pits <- dplyr::select(pits, shedno, pit_cells) %>%
     tidyr::unnest(pit_cells) %>%
     dplyr::group_by(shedno) %>%
     dplyr::mutate(pour_point = c(rep(FALSE, length(shedno)-1), TRUE))
  }

  if("ldir" %in% names(db)) db <- dplyr::mutate(db, elev = replace(elev, ldir == 5, NA))
  if(dir) {
    if(is.null(upslope_threshold)) upslope_threshold <- 0
    db_dir <- db %>%
      dplyr::filter(upslope_n >= upslope_threshold) %>%
      dplyr::mutate(xloc = ifelse(ldir %in% c(1,4,7), -1, ifelse(ldir %in% c(3,6,9), 1, 0)),
                    xend = col + xloc,
                    yloc = ifelse(ldir %in% c(7, 8, 9), -1, ifelse(ldir %in% c(1,2,3), 1, 0)),
                    yend = row + yloc)

    if(!is.null(cells)){
      cells <- na_omit(cells)
      s <- unique(unlist(lapply(cells, trace_flow, db = db)))
      db_dir <- db_dir %>%
        dplyr::filter(seqno %in% s)
    }
  }

  # Main plot
  g <- ggplot2::ggplot(data = db, ggplot2::aes(x = col, y = row)) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed()

  if(!dir & shed) {
    labs <- db %>%
      dplyr::group_by(shedno) %>%
      dplyr::summarize(row = median(row), col = median(col))

    g <- g +
      geom_raster(aes(fill = factor(shedno))) +
      ggplot2::scale_fill_discrete(name = "Watershed") +
      ggplot2::geom_text(data = labs, aes(label = shedno))

  } else if(type == "relief") {
    g <- g + ggplot2::geom_raster(aes(alpha = relief)) +
      ggplot2::scale_alpha_continuous(range = c(1, 0), guide = FALSE)
  } else if(type == "elevation") {
    g <- g + ggplot2::geom_raster(aes(alpha = elev)) +
      ggplot2::scale_alpha_continuous(range = c(0, 1), guide = FALSE)
  }

  # Add cell labels
  if(seqno == TRUE) g <- g + ggplot2::geom_text(aes(label = seqno), size = 2.5, vjust = -1)

  # Add upslope area
  if(!is.null(upslope)){
    g <- g +
      geom_raster(aes(alpha = area), fill = "black") +
      scale_alpha_manual(name = "Upslope area", values = c(0, 0.5))
  } else if(!is.null(pits)){
   g <- g +
     geom_raster(data = pits, alpha = 0.5, fill = "black") +
     geom_point(data = db[db$ldir == 5,], colour = "black") +
     geom_point(data = pits[pits$pour_point == TRUE,], colour = "red")
  }

  # Add directions
  if(dir & shed) {
    g <- g +
      #geom_point(data = db_dir, size = 1) +
      # geom_segment(data = db_dir, aes(xend = xend, yend = yend, colour = factor(shedno)),
      #              arrow = arrow(length = unit(1.5, "mm")))
      geom_segment(data = db_dir, aes(xend = xend, yend = yend, colour = factor(shedno)))
  } else if(dir & !shed) {
    g <- g +
      #geom_point(data = db_dir, size = 1) +
      geom_segment(data = db_dir, aes(xend = xend, yend = yend),
                   arrow = arrow(length = unit(1.5, "mm")))
  }

  # Add lowest point
  if(p) g <- g + geom_point(data = db[db$ldir == 5,], colour = "black")

  return(g)
}

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

file_prep <- function(db, nrows, ncols, missing_value) {

  if(nrows * ncols != length(db$ELEV)){
    stop("Number of rows and columns does not match the total number of cells in the data base, Try again!")
  }

  tibble::data_frame(elev = db$ELEV,
                     seqno = 1:length(elev),
                     row = sort(rep(1:nrows, length(elev)/nrows)),
                     col = rep(1:ncols, length(elev)/ncols),
                     missing = elev == missing_value) %>%
    dplyr::mutate(elev = replace(elev, missing, NA))
}

add_buffer <- function(db) {
  ncols = max(db$col)
  nrows = max(db$row)

  buffers <- tibble::data_frame(row = c(rep(1, ncols+2),        #top
                                        1:(nrows+2),            #left
                                        1:(nrows+2),            #right
                                        rep(nrows+2, ncols+2)), #bottom
                                col = c(1:(ncols+2),            #top
                                        rep(1, nrows+2),        #left
                                        rep(ncols+2, nrows+2),  #right
                                        1:(ncols+2)),           #bottom
                                buffer = TRUE) %>%

    dplyr::distinct()

  db %>%
    dplyr::mutate(row = row + 1, col = col + 1, buffer = FALSE) %>%
    dplyr::bind_rows(buffers) %>%
    dplyr::arrange(row, col) %>%
    dplyr::mutate(seqno = 1:length(row))
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
  readr::write_rds(obj, paste0(file_out, "_", name , ".rds"))
}

read_shed <- function(file_out, name){
  readr::read_rds(paste0(file_out, "_", name , ".rds"))
}

remove_buffer <- function(db, stats = NULL) {

  # replace seqno
  db <- db %>%
    dplyr::filter(!buffer) %>%
    dplyr::arrange(row, col) %>%
    dplyr::rename(seqno_buffer = seqno,
                  drec_buffer = drec)

  if("upslope" %in% names(db)) db <- dplyr::rename(db, upslope_buffer = upslope)

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
  n <- dplyr::slice(db, a)
  if(nrow(n) > 0) {
    if(!("index" %in% names(n))) {
      n <- dplyr::mutate(n, index = c(7, 8, 9, 4, 5, 6, 1, 2, 3))
    }
    return(n)
  } else return(tibble::tibble())

}


# Calculate the direction of flow for an individual cell given its neighbours
finddir <- function(n, verbose = FALSE){

  if(nrow(n) > 0 && !is.na(n$elev[n$index == 5])) {
    if(verbose) matrix(n$seqno, nrow = 3, ncol = 3, byrow = TRUE)
    if(verbose) matrix(n$elev, nrow = 3, ncol = 3, byrow = TRUE)
    if(verbose) matrix(n$index, nrow = 3, ncol = 3, byrow = TRUE)

    # Compare elevations, Reduce diagonal slopes by square-root(2)
    n <- n %>%
      dplyr::mutate(slope = elev[index == 5] - elev,
                    slope = dplyr::if_else(index %in% c(1, 3, 7, 9),
                                           slope/sqrt(2), slope)) %>%
      dplyr::filter(slope > 0)

    # Take local direction (ldir) with steepest slope
    if(nrow(n) > 0) {
      ldir <- dplyr::filter(n, slope == max(slope, na.rm = TRUE)) %>%
        .$index
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

flatout <- function(n, db, verbose = FALSE){

  # Confirm cell is NOT missing (else return NA)
  if(nrow(n) > 0 && !is.na(n$elev[5])){

    # Add in local flow directions
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ldir), by = "seqno")

    # Confirm that IS flat (else return old ldir)
    if(n$ldir[5] == 5) {
      if(verbose) matrix(n$seqno, nrow = 3, ncol = 3, byrow = TRUE)
      if(verbose) matrix(n$elev, nrow = 3, ncol = 3, byrow = TRUE)
      if(verbose) matrix(n$index, nrow = 3, ncol = 3, byrow = TRUE)
      if(verbose) matrix(n$ldir, nrow = 3, ncol = 3, byrow = TRUE)

      n <- n %>%
        dplyr::filter(!is.na(elev)) %>%
        dplyr::mutate(slope = elev[index == 5] - elev) #necessary?

      # Find FIRST neighbour with same elevation and valid flow direction that isn't right back
      new_n <- n %>%
        dplyr::filter(slope == 0,          # Same elevation
                      ldir != 5,           # Isn't flat itself
                      index != 10 - ldir,  # Doesn't flow directly back
                      index != 5) %>%      # Isn't the cell of interest
        dplyr::slice(1)

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
    n <- dplyr::left_join(n, dplyr::select(db, seqno, ldir), by = "seqno")

    # Confirm that IS flat (else return old ldir)
    if(n$ldir[5] == 5) {
      n <- n %>%
        dplyr::mutate(slope = elev[index == 5] - elev) %>%
        dplyr::mutate(newdir = slope == 0 & index != 10 - ldir,   # same elev and neighbour doesn't point back in
                      pit = ldir == 5,                            # neighbour is pit (unncessary?)
                      flowin = index == 10 - ldir)   # same OR higher elev neighbour DOES point in

      n2 <- dplyr::filter(n, !is.na(newdir), !is.na(pit), !is.na(flowin), index != 5)

      # If this flat cell points to another flat cell AND other cells point in, we can connect the flow
      if(any(n2$pit) & any(n2$flowin) & any(n2$newdir)){
        # If more than one, take the first
        ldir <- dplyr::filter(n, newdir == TRUE) %>%
          dplyr::slice(1) %>%
          .$index
        return(ldir)
      } else return(n$ldir[n$index == 5])
    } else return(n$ldir[n$index == 5])
  } else return(NA)
}

#' @import magrittr
neighbour_pit <- function(n, db, verbose = FALSE) {

  # Add in local flow directions
  n <- dplyr::left_join(n, dplyr::select(db, seqno, ldir), by = "seqno")

  # Confirm that IS flat (else return old ldir)
  n <- n %>%
    dplyr::filter(index != 5, ldir == 5)

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


find_lowest <- function(w, w_stats, final_pits) {

  # Mark final pits
  w_stats <- dplyr::mutate(w_stats, final = shedno %in% final_pits)

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

      message("Current pit: ", wp)
      message("  Pit 1: ", pit1$shedno)
      message("  Pit 2: ", pit2$shedno)

      wp <- pit2$shedno

      if(pit2$final) {
        lowest <- pit1
        end <- TRUE
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
  message("  Lowest pit: ", lowest$shedno)
  return(lowest)
}
