#' Calculate slope gradient and curvature
#'
#' Computes row (east/west) and column (north/south) slope gradients and curvatures.
#'
#' Assume the following cells, and the calculations on elevation for point 5:
#'
#'  c7  c8  c9
#'  c4  c5  c6
#'  c1  c2  c3
#'
#'  - Row slope gradient towards east (sgre): (c4 - c6) / (2 * grid)
#'      - Positive slope is downslope (and east-facing)
#'      - Negative slope is upslope (and west-facing)
#'  - Row slope gradient (sgr): abs(sgre)
#'  - Column slope gradient towards north (sgcn): (c2 - c8) / (2 * grid)
#'      - Positive slope is downslope (and north-facing)
#'      - Negative slope is upslope (and south-facing)
#'  - Column slope gradient (sgc): abs(sgcn)
#'  - Row slope curvature (scr): (2 * c5 - c4 - c6) / (grid^2)
#'  - Column slope curvature (scc): (2 * c5 - c2 - c8) / (grid^2)
#'
#'  Where missing neighbours, assume same elevation as central point (i.e.
#'  assume an extension of the field).
#'
#' @param db Dataframe dem
#'
#' @return
#'
#' @examples
#'
#' d <- slope_gc(test_dem)
#'
#' library(ggplot2)
#' flow_plot(d, type = "elevation") +
#'   geom_point(aes(colour = factor(hill_r_n)))
#'
#' @export

slope_gc <- function(db, grid = 1) {

  if(!"buffer" %in% names(db)) db <- add_buffer(db)

  # Calculate Gradients and Curvatures
  db_slopes <- db %>%
    nb_values(max_cols = max(.$col), format = "wide") %>%
    dplyr::mutate(elev_n2 = dplyr::if_else(is.na(elev_n2), elev_n5, elev_n2),
                  elev_n4 = dplyr::if_else(is.na(elev_n4), elev_n5, elev_n4),
                  elev_n6 = dplyr::if_else(is.na(elev_n6), elev_n5, elev_n6),
                  elev_n8 = dplyr::if_else(is.na(elev_n8), elev_n5, elev_n8),
                  sgre = (elev_n4 - elev_n6) / (2 * grid), # Gradient towards east
                  sgr = abs(sgre),
                  sgcn = (elev_n2 - elev_n8) / (2 * grid), # Gradient towards north
                  sgc = abs(sgcn),
                  scr = (2 * elev - elev_n4 - elev_n6) / grid^2,
                  scc = (2 * elev - elev_n2 - elev_n8) / grid^2) %>%
    dplyr::select(-dplyr::contains("_n")) %>%

    # Fix zero values for sgr and sgc
    dplyr::mutate(sgr = dplyr::if_else(sgr == 0, 0.00001, sgr),
                  sgc = dplyr::if_else(sgc == 0, 0.00001, sgc)) %>%

    # Fix zero values for sgre
    nb_values(max_cols = max(.$col), format = "wide", col = "sgre") %>%
    dplyr::mutate(s = dplyr::if_else(!is.na(sgre_n4), sign(sgre_n4), sign(sgre_n6)),
                  sgre = dplyr::if_else(sgre == 0, 0.00001 * s, sgre)) %>%
    dplyr::select(-dplyr::contains("_n")) %>%

    # Fix zero values for sgcn
    nb_values(max_cols = max(.$col), format = "wide", col = "sgcn") %>%
    dplyr::mutate(s = dplyr::if_else(!is.na(sgcn_n2), sign(sgcn_n2), sign(sgcn_n8)),
                  sgcn = dplyr::if_else(sgcn == 0, 0.00001 * s, sgcn)) %>%
    dplyr::select(-dplyr::contains("_n"), -"s")

  # Hillslope records
  db_slopes %>%
    # Get east/west or north/south facing
    dplyr::mutate(hill_r_dir = dplyr::if_else(sgre > 0, 1, 4),      # 1 = east, 4 = west
                  hill_c_dir = dplyr::if_else(sgcn > 0, 1, 3)) %>% # 1 = north, 3 = south

    # Label east/west hillslopes
    dplyr::group_by(row) %>%
    dplyr::mutate(lag_s = dplyr::lag(sign(sgre)),
                  hill_r_n = cumsum(sign(sgre) != lag_s | is.na(lag_s)),
                  hill_r_n = dplyr::if_else(is.na(elev), NA_integer_, hill_r_n),
                  hill_r_n = as.numeric(factor(hill_r_n))) %>%

    # Label east/west cells within a hillslope
    dplyr::group_by(row, hill_r_n) %>%
    dplyr::mutate(hill_r_cell = 1:dplyr::n()) %>%

    # Label north/south hillslopes
    dplyr::group_by(col) %>%
    dplyr::mutate(lag_s = dplyr::lag(sign(sgcn)),
                  hill_c_n = cumsum(sign(sgcn) != lag_s | is.na(lag_s)),
                  hill_c_n = dplyr::if_else(is.na(elev), NA_integer_, hill_c_n),
                  hill_c_n = as.numeric(factor(hill_c_n))) %>%

    # Label north/south cells within a hillslope
    dplyr::group_by(col, hill_c_n) %>%
    dplyr::mutate(hill_c_cell = 1:dplyr::n()) %>%

    # Remove labels on missing values
    dplyr::ungroup() %>%
    dplyr::mutate(hill_r_cell = dplyr::if_else(is.na(elev), NA_integer_, hill_r_cell),
                  hill_c_cell = dplyr::if_else(is.na(elev), NA_integer_, hill_c_cell)) %>%
    dplyr::select(-"lag_s")

}
