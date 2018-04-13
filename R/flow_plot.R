#' Create plots of water flow
#'
#' Plots water flow and watersheds
#'
#' @param db Data frame. Cell by cell data on the elevation of the watershed.
#'   Output by LITAP's \code{complete_run()} function.
#' @param type Character. Either relief or elevation. Defaults to relief.
#' @param dir Logical. Include flow directions?
#' @param seqno Logical. Include cell numbering?
#' @param highlight Logical. Highlight selected cells?
#' @param shed Logical. Show watersheds?
#' @param pits Logical. Show watershed pits (lowest point)
#' @param shed_type Character. Which type of watershed, must be included as a
#'   column in the data frame. Can be one of 'initial', 'local', 'pond', or
#'   'fill'.
#' @param upslope_threshold Numeric. If dir = TRUE, only show flow directions
#'   for cells with >= this many cells which drain to it.
#' @param cells Vector. Which cells to show
#' @param clim Numeric vector. Column limits in format c(0, 100)
#' @param rlim Numeric vector. Row limits in format c(0, 100)
#' @param stats Data frame. Data frame of watershed stats to highlight pour
#'   points.
#' @param missing Character. What is the value of missing data? Defaults to NA

#'
#' @import ggplot2
#' @export
flow_plot <- function(db, type = "relief", dir = FALSE, seqno = FALSE, highlight = FALSE,
                      shed = FALSE, shed_type = "shedno", pits = FALSE,
                      upslope_threshold = NULL,
                      cells = NULL, clim = NULL, rlim = NULL,
                      stats = NULL, missing = NA) {

  db_orig <- db

  if(!is.na(missing)) db <- mutate_cond(db, elev == missing, elev = NA)

  if(!is.null(clim)) db <- dplyr::filter(db, col >= clim[1], col <= clim[2])
  if(!is.null(rlim)) db <- dplyr::filter(db, row >= rlim[1], row <= rlim[2])

  if(!is.null(stats)) {
    if(!is.null(clim)) stats <- dplyr::filter(stats, out_col >= clim[1], out_col <= clim[2], in_col >= clim[1], in_col <= clim[2])
    if(!is.null(rlim)) stats <- dplyr::filter(stats, out_row >= rlim[1], out_row <= rlim[2], in_row >= rlim[1], in_row <= rlim[2])
  }

  if(type == "relief") {
    r <- raster::raster(nrows = length(unique(db$row)),
                        ncols = length(unique(db$col)),
                        vals = db$elev)
    r <- raster::terrain(r, opt = c("slope", "aspect"))
    r <- raster::hillShade(slope = r$slope, aspect = r$aspect, angle = 1)
    db$relief <- as.vector(r)
  }

  if(shed == TRUE){
    if(shed_type == "initial" & "initial_shed" %in% names(db)) db$shedno <- db$initial_shed
    if(shed_type == "local" & "local_shed" %in% names(db)) db$shedno <- db$local_shed
    if(shed_type == "pond" & "pond_shed" %in% names(db)) db$shedno <- db$pond_shed
    if(shed_type == "fill" & "fill_shed" %in% names(db)) db$shedno <- db$fill_shed
  }



  #if("ldir" %in% names(db)) db <- dplyr::mutate(db, elev = replace(elev, ldir == 5, NA))
  if(dir) {
    if(is.null(upslope_threshold)) upslope_threshold <- 0
    if(!("upslope" %in% names(db))) db$upslope <- Inf
    db_dir <- db %>%
      dplyr::filter(upslope >= upslope_threshold) %>%
      dplyr::mutate(xloc = ifelse(ldir %in% c(1,4,7), -1, ifelse(ldir %in% c(3,6,9), 1, 0)),
                    xend = col + xloc,
                    yloc = ifelse(ldir %in% c(7, 8, 9), -1, ifelse(ldir %in% c(1,2,3), 1, 0)),
                    yend = row + yloc)

    if(!is.null(cells)){
      cells <- na_omit(cells)
      s <- unique(unlist(lapply(cells, trace_flow, db = db_orig)))
      db_dir <- db_dir %>%
        dplyr::filter(seqno %in% s)
    }
  }

  # Main plot
  g <- ggplot(data = db, aes(x = col, y = row)) +
    theme_classic() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "pt")) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    coord_fixed()

  if(!dir & shed) {
    labs <- db %>%
      dplyr::group_by(shedno) %>%
      dplyr::summarize(row = median(row), col = median(col))

    g <- g +
      geom_raster(aes(fill = factor(shedno))) +
      scale_fill_discrete(name = "Watershed", guide = FALSE) +
      geom_text(data = labs, aes(label = shedno))

  } else if(type == "relief") {
    g <- g + geom_raster(aes(alpha = relief)) +
      scale_alpha_continuous(range = c(1, 0), guide = FALSE)
  } else if(type == "elevation") {
    g <- g + geom_raster(aes(alpha = elev)) +
      scale_alpha_continuous(range = c(0, 1))
  }

  # Add cell labels
  if(seqno == TRUE) {
    if(highlight) {
      seqnos <- dplyr::filter(db, seqno %in% cells)
      g <- g + geom_text(data = seqnos, aes(label = seqno), size = 2.5, vjust = -1)
    } else {
      g <- g + geom_text(aes(label = seqno), size = 2.5, vjust = -1)
    }
  }

  # Add upslope area
  # if(!is.null(upslope)){
  #   g <- g +
  #     geom_raster(aes(alpha = area), fill = "black") +
  #     scale_alpha_manual(name = "Upslope area", values = c(0, 0.5))
  # } else
    if(!is.null(stats)){
    g <- g +
      #geom_point(data = stats, aes(shape = dir)) +
      geom_curve(data = stats, aes(x = in_col, xend = out_col, y = in_row, yend = out_row),
                 arrow = arrow(length = unit(0.01, "npc")))
      #scale_shape_manual(values = c(20,21))
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
  if(pits) g <- g + geom_point(data = db[db$ldir == 5,], colour = "black")

  return(g)
}
