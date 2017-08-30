invert <- function(db) {
  max_elev <- max(db$elev, na.rm = TRUE)
  db <- dplyr::mutate(db, elev = max_elev - elev)
  return(db)
}
