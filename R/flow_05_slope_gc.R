#' Calculate slope gradient and curvature, and hills
#'
#' Computes row (east/west) and column (north/south) slope gradients and
#' curvatures. Also calculates hill slopes as points when slope gradients switch
#' directions
#'
#' @details
#' Assume the following cells, and the calculations on elevation for focal point
#' 5:
#'
#'  c7  c8  c9
#'
#'  c4  c5  c6
#'
#'  c1  c2  c3
#'
#'  **Slope gradients and curvature**
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
#'  Zero values replaced with arbitrarily small value, 0.00001
#'
#'  For gradients, zero values replaced with arbitrarily small value (0.00001)
#'  but with the sign of the previous point (i.e. to left (n4, west) if row; to
#'  bottom (n2, south) if column). If that previous point is missing, look to
#'  next point (i.e. n6 or n8). If cannot resolve by working through points,
#'  assign to positive.
#'
#'  **Hill slopes**
#'
#'  - hill_r_n: Number of hillslopes in that row. It goes from 1 to whatever it
#'  ends. Hillslope number changes when it goes from downslope to upslope or
#'  from upslope to downslope (SGRE changes sign)
#'  - hill_r_dir: Direction the slope is facing, east facing as 2 and west
#'  facing as 4, all cells with the same hillslope no. have same hillslope
#'  direction
#'  - hill_r_cell: Order number of the cell in the hillslope, starts from 1 to
#'  how many cells there are in a given hillslope
#'
#'  - hill_c_n: Same as for row, changes when SGCN changes sign
#'  - hill_c_dir: Same as for row, north facing as 1 and south facing as 3
#'  - hill_c_cell: Same as for row.
#'
#'  Directions:
#'
#'  N (1) -> E (2) -> S (3) -> W (4)
#'
#' @param db Dataframe dem
#' @param grid Numeric. Grid size for the original dem
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
    dplyr::mutate(elev_n2 = dplyr::if_else(is.na(.data$elev_n2), .data$elev_n5, .data$elev_n2),
                  elev_n4 = dplyr::if_else(is.na(.data$elev_n4), .data$elev_n5, .data$elev_n4),
                  elev_n6 = dplyr::if_else(is.na(.data$elev_n6), .data$elev_n5, .data$elev_n6),
                  elev_n8 = dplyr::if_else(is.na(.data$elev_n8), .data$elev_n5, .data$elev_n8),
                  sgre = (.data$elev_n4 -.data$ elev_n6) / (2 * .env$grid), # Gradient towards east
                  sgr = abs(.data$sgre),
                  sgcn = (.data$elev_n2 - .data$elev_n8) / (2 * .env$grid), # Gradient towards north
                  sgc = abs(.data$sgcn),
                  scr = (2 * .data$elev - .data$elev_n4 - .data$elev_n6) / .env$grid^2,
                  scc = (2 * .data$elev - .data$elev_n2 - .data$elev_n8) / .env$grid^2) %>%
    dplyr::select(-dplyr::contains("_n")) %>%

    # Fix zero values for sgr and sgc and start for sgre and sgcn
    dplyr::mutate(sgr = dplyr::if_else(.data$sgr == 0, 0.00001, .data$sgr),
                  sgc = dplyr::if_else(.data$sgc == 0, 0.00001, .data$sgc))

  # Fix sign on small values for sgre - iterate until all solved
  n_sgre <- sum(db_slopes$sgre == 0, na.rm = TRUE)
  while(n_sgre > 0) {
    db_slopes <- db_slopes %>%
      nb_values(max_cols = max(.$col), format = "wide", col = "sgre") %>%
      # If n4 0, iterate over
      dplyr::mutate(s = dplyr::if_else(is.na(.data$sgre_n4), sign(.data$sgre_n6), sign(.data$sgre_n4)),
                    sgre = dplyr::if_else(.data$sgre == 0, 0.00001 * .data$s, .data$sgre)) %>%
      dplyr::select(-dplyr::contains("_n"), -"s")
    if(n_sgre == sum(db_slopes$sgre == 0, na.rm = TRUE)){
      # If cannot resolve, assign all remaining to positive
      db_slopes <- db_slopes %>%
        dplyr::mutate(sgre = dplyr::if_else(.data$sgre == 0, 0.00001, .data$sgre))
      n_sgre <- 0
    } else n_sgre <- sum(db_slopes$sgre == 0, na.rm = TRUE)
  }


  # Fix zero values for sgcn - iterate until all solved
  n_sgcn <- sum(db_slopes$sgcn == 0, na.rm = TRUE)
  while(n_sgcn > 0) {
    db_slopes <- db_slopes %>%
      nb_values(max_cols = max(.$col), format = "wide", col = "sgcn") %>%
      dplyr::mutate(s = dplyr::if_else(is.na(.data$sgcn_n2), sign(.data$sgcn_n8), sign(.data$sgcn_n2)),
                    sgcn = dplyr::if_else(.data$sgcn == 0, 0.00001 * .data$s, .data$sgcn)) %>%
      dplyr::select(-dplyr::contains("_n"), -"s")
    if(n_sgcn == sum(db_slopes$sgcn == 0, na.rm = TRUE)){
      # If cannot resolve, assign all remaining to positive
      db_slopes <- db_slopes %>%
        dplyr::mutate(sgcn = dplyr::if_else(.data$sgcn == 0, 0.00001, .data$sgcn))
      n_sgcn <- 0
    } else n_sgcn <- sum(db_slopes$sgcn == 0, na.rm = TRUE)
  }

  # Hillslope records
  db_slopes %>%
    # Get east/west or north/south facing
    dplyr::mutate(hill_r_dir = dplyr::if_else(.data$sgre > 0, 2, 4),      # 2 = east, 4 = west
                  hill_c_dir = dplyr::if_else(.data$sgcn > 0, 1, 3)) %>%  # 1 = north, 3 = south

    # Label east/west hillslopes
    dplyr::group_by(row) %>%
    dplyr::mutate(lag_s = dplyr::lag(sign(.data$sgre)),
                  hill_r_n = cumsum(sign(.data$sgre) != .data$lag_s | is.na(.data$lag_s)),
                  hill_r_n = dplyr::if_else(is.na(.data$elev), NA_integer_, .data$hill_r_n),
                  hill_r_n = as.numeric(factor(.data$hill_r_n))) %>%

    # Label east/west cells within a hillslope
    dplyr::group_by(.data$row, .data$hill_r_n) %>%
    dplyr::mutate(hill_r_cell = 1:dplyr::n()) %>%

    # Label north/south hillslopes
    dplyr::group_by(col) %>%
    dplyr::mutate(lag_s = dplyr::lag(sign(.data$sgcn)),
                  hill_c_n = cumsum(sign(.data$sgcn) != .data$lag_s | is.na(.data$lag_s)),
                  hill_c_n = dplyr::if_else(is.na(.data$elev), NA_integer_, .data$hill_c_n),
                  hill_c_n = as.numeric(factor(.data$hill_c_n))) %>%

    # Label north/south cells within a hillslope
    dplyr::group_by(.data$col, .data$hill_c_n) %>%
    dplyr::mutate(hill_c_cell = 1:dplyr::n()) %>%

    # Remove labels on missing values
    dplyr::ungroup() %>%
    dplyr::mutate(hill_r_cell = dplyr::if_else(is.na(.data$elev), NA_integer_, .data$hill_r_cell),
                  hill_c_cell = dplyr::if_else(is.na(.data$elev), NA_integer_, .data$hill_c_cell)) %>%
    dplyr::select(-"lag_s")

}
