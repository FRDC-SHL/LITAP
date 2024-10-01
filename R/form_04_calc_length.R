#calc_len3

calc_length <- function(db, relz, grid, verbose){

  relz <- dplyr::left_join(relz, dplyr::select(db, seqno, row, col),
                           by = c("seqno", "row", "col"))

  relz <- relz %>%
    dplyr::mutate(
      l2pit = sqrt((.data$pit_col - .data$col)^2 + (.data$pit_row - .data$row)^2) * .env$grid,
      l2pit = sqrt(.data$l2pit^2 + (.data$z2pit * .env$grid)^2),

      l2peak = sqrt((.data$peak_col - .data$col)^2 + (.data$peak_row - .data$row)^2) * .env$grid,
      l2peak = sqrt(.data$l2peak^2 + (.data$z2peak * .env$grid)^2),

      l2str = sqrt((.data$st_col - .data$col)^2 + (.data$st_row - .data$row)^2) * .env$grid,
      l2str = sqrt(.data$l2str^2 + (.data$z2st * .env$grid)^2),

      l2div = sqrt((.data$cr_col - .data$col)^2 + (.data$cr_row - .data$row)^2) * .env$grid,
      l2div = sqrt(.data$l2div^2 + (.data$z2cr * .env$grid)^2),
      # In C++ version, z2peak, but in foxitpro, z2cr. USE z2cr which I think is more correct

      lpit2peak = .data$l2pit + .data$l2peak,
      lstr2div = .data$l2str + .data$l2div,

      ppit2peakl = trunc((.data$l2pit / .data$lpit2peak) * 100),
      pstr2divl = trunc((.data$l2str / .data$lstr2div) * 100)) %>%
    dplyr::mutate_at(dplyr::vars("l2pit", "l2peak", "lpit2peak", "l2str", "l2div", "lstr2div"),
                     ~ round(., 1))

  relz$ppit2peakl[relz$lpit2peak == 0] <- 0
  relz$pstr2divl[relz$lstr2div == 0] <- 0

  relz
}
