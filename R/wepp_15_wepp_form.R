#' Compute slope and aspect for WEPP
#'
#' From original FoxPro code:
#'
#' "Procedure to compute slope gradient and aspect for use in WEPP"
#'
#' @noRd
#'

wepp_form <- function(db, grid) {

  # Get aspect from form_calc_form.r

  form <- db %>%
    dplyr::select("seqno", "elev", "row", "col", "buffer") %>%
    nb_values(max_cols = max(db$col), format = "wide") %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("elev_n")),
                     ~ . * 100) %>%
    dplyr::mutate(sum_elev = purrr::pmap_dbl(
      list(.data$elev_n1, .data$elev_n2, .data$elev_n3, .data$elev_n4, .data$elev_n5,
           .data$elev_n6, .data$elev_n7, .data$elev_n8, .data$elev_n9),
      ~sum(is.na(c(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9))))) %>%
    #dplyr::filter(sum_elev == 0) %>%
    dplyr::mutate(slope_x = (.data$elev_n6 - .data$elev_n4) / (2 * .env$grid),
                  slope_y = (.data$elev_n2 - .data$elev_n8) / (2 * .env$grid),
                  slope_pct = sqrt(.data$slope_x^2 + .data$slope_y^2),
                  slope_deg = rad_deg(atan(.data$slope_pct/100)),
                  aspect = aspect(.data$slope_x, .data$slope_y, .data$slope_pct)) %>%
    dplyr::select("seqno", "row", "col", "slope_pct", "slope_deg",
                  "aspect", "buffer") %>%
    dplyr::mutate(slope_pct = round(.data$slope_pct, 3),
                  slope_deg = round(.data$slope_deg, 3),
                  aspect = round(.data$aspect))


  # First/last rows and cols get adjacent values
  vals <- c("slope_pct", "slope_deg", "aspect")

  # Note that first and last row over write corners (assume buffer)
  form[form$col == 2, vals] <- form[form$col == 3, vals] # Left Column
  form[form$col == max(form$col) - 1, vals] <- form[form$col == max(form$col) - 2, vals]   # Right Column
  form[form$row == 2, vals] <- form[form$row == 3, vals] # First row
  form[form$row == max(form$row) - 1, vals] <- form[form$row == max(form$row) - 2, vals]   # Last row

  dplyr::left_join(db, form, by = c("seqno", "row", "col", "buffer"))
}
