

#' Calculate form
#'
#' This function takes backup data frame output from [complete_run()] and
#' calculates form, wetness indices, reflief and stream/crest lengths (among
#' other metrics). It is based on FormMapR.
#'
#' @param db   Data frame. Backup db pond file
#' @param idb  Data frame. Backup inverted db local file
#' @param grid Numeric.    Grid size
#' @param resume Character. From which stage should the run be resumed?
#' @param verbose Logical. Print progress messages?
#'
#' @details Output files are saved the working directory.
#'
#' @examples
#'
#' \dontrun{
#' db <- readr::read_rds("./backup/021_backup_pond.rds")$db
#' idb <- readr::read_rds("./backup/021_backup_ilocal.rds")$db
#'
#' form_mapper(db, idb, grid = 5)
#' }
#'
#' @export

form_mapper <- function(db, idb, grid, resume = NULL, verbose = TRUE) {




  if(is.null(resume)){
    message("Form")
    db_form <- calc_form(db, grid)
    message("Weti")
    db_weti <- calc_weti(db, grid, verbose = verbose)
    db_form <- dplyr::full_join(db_form, db_weti)
    readr::write_csv(db_form, "form.csv")
    resume <- "relz"
  }

  if(resume == "relz"){
    message("Relz")
    db_relz <- calc_relz(db, idb, verbose = verbose)
    readr::write_csv(db_relz, "relz.csv")
    resume <- "length"
  }

  if(resume == "length") {
    message("Length")
    db_relz <- readr::read_csv("relz.csv")
    db_length <- calc_length(db, db_relz)
    readr::write_csv(db_length, "len.csv")
  }
}

# Based on FormMapR calc_form: Computes slope, aspect & curvatures
calc_form <- function(db, grid = 10) {

  # slope, aspect, prof, plan
  details <- db %>%
    nb_values(max_cols = max(db$col)) %>%
    dplyr::mutate(elev_n = elev_n * 100) %>%
    dplyr::group_by(seqno) %>%
    dplyr::filter(sum(is.na(elev_n)) == 0) %>%
    dplyr::summarize(slope_x = (elev_n[6] - elev_n[4]) / (2 * grid),
                     slope_y = (elev_n[2] - elev_n[8]) / (2 * grid),
                     slope_pct = sqrt(slope_x^2 + slope_y^2),
                     slope_deg = rad_deg(atan(slope_pct/100)),
                     aspect = aspect(slope_x, slope_y, slope_pct),
                     prof_aspect = dplyr::if_else(aspect > 180, aspect - 180, aspect),
                     plan_aspect = dplyr::if_else((prof_aspect + 90) > 180,
                                                  prof_aspect + 90 - 180,
                                                  prof_aspect + 90),
                     prof = prof_plan(prof_aspect, elev_n, grid),
                     plan = prof_plan(plan_aspect, elev_n, grid)) %>%
    dplyr::select(seqno, slope_pct, slope_deg, aspect, prof, plan)

  # Join back in to db
  db <- dplyr::left_join(db, details, by = "seqno")

  # First/last rows and cols get adjacent values
  vals <- c("slope_pct", "slope_deg", "aspect", "prof", "plan")

  # Note that first and last row over write corners
  db[db$col == 2, vals] <- db[db$col == 3, vals] # Left Column
  db[db$col == max(db$col) - 1 , vals] <- db[db$col == max(db$col) - 2, vals]   # Right Column
  db[db$row == 2, vals] <- db[db$row == 3, vals] # First row
  db[db$row == max(db$row) - 1, vals] <- db[db$row == max(db$row) - 2, vals]   # Last row

  db
}


aspect <- function(slope_x, slope_y, slope_pct) {
  local_angle <- rad_deg(acos(abs(slope_x)/slope_pct))
  dplyr::case_when(slope_x > 0 & slope_y > 0 ~ 270 + local_angle,
                   slope_x > 0 & slope_y < 0 ~ 270 - local_angle,
                   slope_x < 0 & slope_y > 0 ~ 90 - local_angle,
                   slope_x < 0 & slope_y < 0 ~ 90 + local_angle,
                   slope_x < 0 & slope_y == 0 ~ 90,
                   slope_x > 0 & slope_y == 0 ~ 270,
                   slope_x == 0 & slope_y < 0 ~ 180,
                   slope_x == 0 & slope_y > 0 ~ 360,
                   TRUE ~ as.numeric(NA))
}

prof_plan <- function(aspect, n, grid){
  x1 <- 2 + sin(deg_rad(aspect))
  y1 <- 2 - cos(deg_rad(aspect))
  x2 <- 2 - sin(deg_rad(aspect))
  y2 <- 2 + cos(deg_rad(aspect))

  if(!is.na(aspect)){
    if(aspect <= 90) {
      z1 <- ((2 - y1) * ((n[9] * (x1 - 2)) + (n[8] * (3 - x1)))) +
        ((y1 - 1) * ((n[6] * (x1 - 2)) + (n[5] * (3 - x1))))

      z2 <- ((3 - y2) * ((n[5] * (x2 - 1)) + (n[4] * (2 - x2)))) +
        ((y2 - 2) * ((n[2] * (x2 - 1)) + (n[1] * (2 - x2))))
    } else {
      z1 <- ((3 - y1) * ((n[6] * (x1 - 2)) + (n[5] * (3 - x1)))) +
        ((y1 - 2) * ((n[3] * (x1 - 2)) + (n[2] * (3 - x1))))

      z2 <- ((2 - y2) * ((n[8] * (x2 - 1)) + (n[7] * (2 - x2)))) +
        ((y2 - 1) * ((n[5] * (x2 - 1)) + (n[4] * (2 - x2))))
    }

    p <- rad_deg(atan((((2 * n[5]) - z1 - z2) / (grid * grid))))
  } else p <- NA
  p
}


rad_deg <- function(x) (x * 180) / pi
deg_rad <- function(x) (x * pi) / 180
