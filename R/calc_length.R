#calc_len3

calc_length <- function(db, relz, grid = 5, verbose = FALSE){

  relz <- dplyr::left_join(relz, dplyr::select(db, seqno, row, col),
                           by = c("seqno", "row", "col"))

  relz %>%
    dplyr::mutate(
      l2pit = sqrt((pit_col - col)^2 + (pit_row - row)^2) * grid,
      l2pit = sqrt(l2pit^2 + (z2pit * grid)^2),

      l2peak = sqrt((peak_col - col)^2 + (peak_row - row)^2) * grid,
      l2peak = sqrt(l2peak^2 + (z2peak * grid)^2),

      l2str = sqrt((str_col - col)^2 + (str_row - row)^2) * grid,
      l2str = sqrt(l2str^2 + (z2st * grid)^2),

      l2div = sqrt((cr_col - col)^2 + (cr_row - row)^2) * grid,
      l2div = sqrt(l2div^2 + (z2cr * grid)^2),

      lpit2peak = l2pit + l2peak,
      lstr2div = l2str + l2div,

      ppit2peakl = round((l2pit / lpit2peak) * 100),
      pstr2divl = round((l2str / lstr2div) * 100))

}
