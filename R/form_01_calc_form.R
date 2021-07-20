

# Based on FormMapR calc_form: Computes slope, aspect & curvatures
calc_form <- function(db, grid, verbose) {

    # slope, aspect, prof, plan
  form <- db %>%
    dplyr::select("seqno", "elev", "row", "col", "buffer") %>%
    nb_values(max_cols = max(db$col), format = "wide") %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("elev_n")),
                     ~ . * 100) %>%
    dplyr::mutate(sum_elev = purrr::pmap_dbl(
      list(elev_n1, elev_n2, elev_n3, elev_n4, elev_n5,
           elev_n6, elev_n7, elev_n8, elev_n9),
      ~sum(is.na(c(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9))))) %>%
    #dplyr::filter(sum_elev == 0) %>%
    dplyr::mutate(slope_x = dplyr::if_else(sum_elev > 0, NA_real_,
                                           (elev_n6 - elev_n4) / (2 * grid)),
                  slope_y = dplyr::if_else(sum_elev > 0, NA_real_,
                                           (elev_n2 - elev_n8) / (2 * grid)),
                  slope_pct = sqrt(slope_x^2 + slope_y^2),
                  slope_deg = rad_deg(atan(slope_pct/100)),
                  aspect = dplyr::if_else(sum_elev > 0 & !is.na(elev), 360,
                                          aspect(slope_x, slope_y, slope_pct)),
                  aspect = round(aspect),
                  prof_aspect = dplyr::if_else(aspect > 180, aspect - 180, aspect),
                  plan_aspect = dplyr::if_else((prof_aspect + 90) > 180,
                                               prof_aspect + 90 - 180,
                                               prof_aspect + 90),
                  prof = dplyr::if_else(sum_elev > 0, NA_real_,
                                        prof_plan(prof_aspect, elev_n1, elev_n2,
                                                  elev_n3, elev_n4, elev_n5,
                                                  elev_n6, elev_n7, elev_n8,
                                                  elev_n9, grid, slope_pct)),
                  plan = dplyr::if_else(sum_elev > 0, NA_real_,
                                        prof_plan(plan_aspect, elev_n1, elev_n2,
                                                  elev_n3, elev_n4, elev_n5,
                                                  elev_n6, elev_n7, elev_n8,
                                                  elev_n9, grid, slope_pct))) %>%
    dplyr::select("seqno", "elev", "row", "col", "slope_pct", "slope_deg",
                  "aspect", "prof", "plan", "buffer") %>%
    dplyr::mutate(slope_pct = round(slope_pct, 3),
                  slope_deg = round(slope_deg, 3),
                  aspect = round(aspect),
                  prof = round(prof, 3),
                  plan = round(plan, 3))


  # First/last rows and cols get adjacent values
  vals <- c("slope_pct", "slope_deg", "aspect", "prof", "plan")

  # Note that first and last row over write corners (assume buffer)
  form[form$col == 2, vals] <- form[form$col == 3, vals] # Left Column
  form[form$col == max(form$col) - 1, vals] <- form[form$col == max(form$col) - 2, vals]   # Right Column
  form[form$row == 2, vals] <- form[form$row == 3, vals] # First row
  form[form$row == max(form$row) - 1, vals] <- form[form$row == max(form$row) - 2, vals]   # Last row

  # Make missing values 0 where required
  form %>%
    dplyr::mutate(
      prof = replace(.data$prof, is.na(.data$prof) & !is.na(.data$elev), 0),
      plan = replace(.data$plan, is.na(.data$plan) & !is.na(.data$elev), 0),
      slope_pct = replace(.data$slope_pct, is.na(.data$slope_pct) & !is.na(.data$elev), 0),
      slope_deg = replace(.data$slope_deg, is.na(.data$slope_deg) & !is.na(.data$elev), 0))
}

aspect <- function(slope_x, slope_y, slope_pct) {
  local_angle <- rad_deg(acos(abs(slope_x)/slope_pct))
  dplyr::case_when(slope_pct <= 0 ~ 360,
                   slope_x > 0 & slope_y > 0 ~ 270 + local_angle,
                   slope_x > 0 & slope_y < 0 ~ 270 - local_angle,
                   slope_x < 0 & slope_y > 0 ~ 90 - local_angle,
                   slope_x < 0 & slope_y < 0 ~ 90 + local_angle,
                   slope_x < 0 & slope_y == 0 ~ 90,
                   slope_x > 0 & slope_y == 0 ~ 270,
                   slope_x == 0 & slope_y < 0 ~ 180,
                   slope_x == 0 & slope_y > 0 ~ 360,
                   TRUE ~ as.numeric(NA))
}

prof_plan <- function(aspect, elev_n1, elev_n2, elev_n3,
                       elev_n4, elev_n5, elev_n6, elev_n7, elev_n8, elev_n9,
                       grid, slope_pct){

  x1 <- 2 + sin(deg_rad(aspect))
  y1 <- 2 - cos(deg_rad(aspect))
  x2 <- 2 - sin(deg_rad(aspect))
  y2 <- 2 + cos(deg_rad(aspect))

  z1 <- dplyr::case_when(
    aspect <= 90 ~ (2 - y1) * ((elev_n9 * (x1 - 2)) + (elev_n8 * (3 - x1))),
    aspect > 90 ~ ((3 - y1) * ((elev_n6 * (x1 - 2)) + (elev_n5 * (3 - x1)))))

  z1 <- dplyr::case_when(
    aspect <= 90 ~ z1 + ((y1 - 1) * ((elev_n6 * (x1 - 2)) + (elev_n5 * (3 - x1)))),
    aspect > 90 ~ z1 + ((y1 - 2) * ((elev_n3 * (x1 - 2)) + (elev_n2 * (3 - x1)))))

  z2 <- dplyr::case_when(
    aspect <= 90 ~ (3 - y2) * ((elev_n5 * (x2 - 1)) + (elev_n4 * (2 - x2))),
    aspect > 90 ~ (2 - y2) * ((elev_n8 * (x2 - 1)) + (elev_n7 * (2 - x2))))

  z2 <- dplyr::case_when(
    aspect <= 90 ~ z2 + ((y2 - 2) * ((elev_n2 * (x2 - 1)) + (elev_n1 * (2 - x2)))),
    aspect > 90 ~ z2 + ((y2 - 1) * ((elev_n5 * (x2 - 1)) + (elev_n4 * (2 - x2)))))

 dplyr::case_when(
    slope_pct <= 0 ~ 0,
    TRUE ~ rad_deg(atan((((2 * elev_n5) - z1 - z2) / (grid * grid)))))
}


rad_deg <- function(x) (x * 180) / pi
deg_rad <- function(x) (x * pi) / 180
