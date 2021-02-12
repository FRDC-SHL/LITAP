#' Compute slope and aspect for WEPP
#'
#' From original FoxPro code:
#'
#' "Procedure to process the ID#WEPP file after all hillslopes and their
#'  morphometric attributes have been computed to create and store
#'  statistical descriptions of notional hillslope profiles as required
#'  for input into the WEPP model.
#'
#'  Each hillslope has a notional hillslope profile computed for it.
#'  Each hillslope profile is described by a series of up to 20 data points
#'  Each data point is described in terms of its distance from the origin
#'  of the profile (the top most point at the beginning of the hillslope)
#'  and the slope (in m/m) at each profile point.
#'
#'  The program writes data for each hillslope to 2 different files
#'
#'  The hill_file (ID#HILL) stores data that describe the entire hillslope
#'  profile as a single object.  This includes the length, width and area
#'  of the hillslope, the dominant aspect of the hillslope and the number
#'  of individual hillslope profile points used to describe the form of
#'  the hillslope profile for this hillslope.  A character field (PROFILE)
#'  is used to store a concatenated description of the hillslope profile
#'  intended to represent the form of the profile for this hillslope
#'
#'  The prof_file (ID#PROF) stores data that identify and describe each of
#'  up to 20 individual data points along a hillslope profile from top to
#'  bottom of the profile.  This file identifies for each hillslope number,
#'  the relative distance from the top of the profile (given as percent of
#'  the total computed hill slope distance) for each point along with the
#'  computed slope (m/m) and aspect for that notional profile point.
#'  Slope is computed as the median slope for all cells within the length
#'  interval.  Aspect is computed as the dominant aspect class using 5%
#'  aspect class intervals"
#'
#' @noRd
#'

hill_stats <- function(db, segs, grid) {

  hill <- db %>%
    dplyr::select("hill_no", "shed_side", "slope_pct", "aspect", "n2st") %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(hill_no)) %>%
    dplyr::group_by(hill_no) %>%
    dplyr::mutate(slope = .data$slope_pct / 100,
                  hill_area = 1:dplyr::n(),
                  x_vector = cumsum(sin(.data$aspect * pi/180)),
                  y_vector = cumsum(cos(.data$aspect * pi/180)),
                  xovery = x_vector / y_vector,
                  temp_aspect = atan(xovery) * 180 / pi,
                  hill_circ = dplyr::case_when(.data$y_vector <= 0 ~ 180 + .data$temp_aspect,
                                               .data$x_vector >= 0 ~ .data$temp_aspect,
                                               .data$x_vector <= 0 ~ 360 + .data$temp_aspect,
                                               TRUE ~ as.numeric(NA)),
                  max_n2st = max(n2st, na.rm = TRUE),
                  rel_n2st = dplyr::if_else(max_n2st > 1,
                                            1 - ((n2st - 1) / (max_n2st - 1)),
                                            1))

  # How are <21 points calculate, across all points? Or just for min n2st points (i.e. rel_n2st == 1)

  prof <- hill %>%
    dplyr::select("hill_no", "n2st", "slope", "aspect", "rel_n2st", "max_n2st") %>%
    dplyr::arrange(.data$hill_no, .data$n2st, .data$slope) %>%
    dplyr::mutate(curr_rel = purrr::map_dbl(rel_n2st, ~sum(. >= seq(1, 0, -0.06))),
                  curr_rel = replace(curr_rel, max_n2st < 21, rel_n2st)) %>%
    dplyr::group_by(.data$hill_no, .data$curr_rel) %>%
    dplyr::summarize(slope = round(sum(slope) / dplyr::n(), 3),
                     x_vector = sum(sin(.data$aspect * pi/180)),
                     y_vector = sum(cos(.data$aspect * pi/180)),
                     rel_dist = round(min(rel_n2st), 2),
                     xovery = x_vector/y_vector,
                     temp_aspect = atan(xovery) * 180 / pi,
                     aspect = dplyr::case_when(.data$y_vector <= 0 ~ 180 + .data$temp_aspect,
                                               .data$x_vector >= 0 ~ .data$temp_aspect,
                                               .data$x_vector <= 0 ~ 360 + .data$temp_aspect,
                                               TRUE ~ as.numeric(NA)),
                     aspect = round(aspect)) %>%
    dplyr::mutate(n = dplyr::n(),
                  rel_n2st = rel_dist,
                  ofe_num = 0, soil_id = 0, manage_id = 0)

  prof_string <- prof %>%
    dplyr::group_by(hill_no) %>%
    dplyr::summarize(profile = glue::glue_collapse(glue::glue("{round(rel_dist, 3)}, {round(slope, 3)}"), sep = " "),
                     num_points = unique(n), .groups = "drop")

  prof <- prof %>%
    dplyr::select("hill_no", "distance" = "rel_dist", "slope",
                  "aspect", "rel_n2st", "ofe_num", "soil_id", "manage_id") %>%
    dplyr::arrange(.data$hill_no, dplyr::desc(.data$distance))

  h <- hill %>%
    dplyr::select("hill_no") %>%
    dplyr::distinct() %>%
    dplyr::left_join(dplyr::select(segs, "len_cells", "left_hill"), by = c("hill_no" = "left_hill")) %>%
    dplyr::left_join(dplyr::select(segs, "len_cells", "right_hill"), by = c("hill_no" = "right_hill")) %>%
    dplyr::left_join(dplyr::select(segs, "len_cells", "top_hill"), by = c("hill_no" = "top_hill")) %>%
    dplyr::filter(!(is.na(.data$len_cells.x) & is.na(.data$len_cells.y) & is.na(.data$len_cells))) %>%
    dplyr::mutate(hill_width = stats::na.omit(c(.data$len_cells.x, .data$len_cells.y, .data$len_cells)))

  hill <- hill %>%
    dplyr::select("hill_no", "hill_area", "hill_circ", "shed_side", "max_n2st") %>%
    dplyr::group_by(.data$hill_no, .data$shed_side, .data$max_n2st) %>%
    dplyr::summarize(hill_area = max_na(hill_area),
                     aspect = max_na(hill_circ), .groups = "drop") %>%
    dplyr::filter(!is.na(.data$hill_no)) %>%
    dplyr::left_join(dplyr::select(h, "hill_no", "hill_width"), by = "hill_no") %>%
    dplyr::mutate(max_len = .data$max_n2st * grid,
                  hill_area = .data$hill_area * (grid^2),
                  hill_width = dplyr::if_else(.data$shed_side == 1,
                                              .data$hill_area/.data$max_len,
                                              .data$hill_width * grid),
                  wepp_len = .data$hill_area/.data$hill_width) %>%
    dplyr::left_join(prof_string, by = "hill_no") %>%
    dplyr::select("hill_no", "hill_width", "hill_area", "max_len", "wepp_len",
                  "num_points", "aspect", "profile")

  list(hill = hill, prof = prof)
}
