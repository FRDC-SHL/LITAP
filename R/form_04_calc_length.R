#calc_len3

calc_length <- function(db, relz, grid, verbose){

  relz <- dplyr::left_join(relz, dplyr::select(db, seqno, row, col),
                           by = c("seqno", "row", "col"))

  relz <- relz %>%
    dplyr::mutate(
      l2pit = sqrt((pit_col - col)^2 + (pit_row - row)^2) * grid,
      l2pit = sqrt(l2pit^2 + (z2pit * grid)^2),

      l2peak = sqrt((peak_col - col)^2 + (peak_row - row)^2) * grid,
      l2peak = sqrt(l2peak^2 + (z2peak * grid)^2),

      l2str = sqrt((str_col - col)^2 + (str_row - row)^2) * grid,
      l2str = sqrt(l2str^2 + (z2st * grid)^2),

      l2div = sqrt((cr_col - col)^2 + (cr_row - row)^2) * grid,
      l2div = sqrt(l2div^2 + (z2cr * grid)^2),
      # In C++ version, z2peak, but in foxitpro, z2cr. USE z2cr which I think is more correct

      lpit2peak = l2pit + l2peak,
      lstr2div = l2str + l2div,

      ppit2peakl = trunc((l2pit / lpit2peak) * 100),
      pstr2divl = trunc((l2str / lstr2div) * 100)) %>%
    dplyr::mutate_at(dplyr::vars("l2pit", "l2peak", "lpit2peak", "l2str", "l2div", "lstr2div"),
                     ~ round(., 1))

  relz$ppit2peakl[relz$lpit2peak == 0] <- 0
  relz$pstr2divl[relz$lstr2div == 0] <- 0

  relz
}
