edge_pit <- function(n) {
  if(length(n) > 0) {
    return(any(is.na(n$elev)))
  } else return(FALSE)
}

#' @import magrittr
get_pour_point <- function(db_w, w, db, verbose = FALSE){

  if(verbose)  message("  - Watershed number ", w)

  pour_elev <- 1000000000
  overspill <- FALSE
  i <- 1

  while(!overspill & i <= nrow(db_w)) {
    focal <- db_w[i, ]

    n <- neighbours(db_w$adjacent[[i]], db) %>%
      dplyr::filter(shedno != w)

    if(nrow(n) > 0){
      min_elev <- min(n$elev, na.rm = TRUE)

      for(a in 1:nrow(n)) {
        if(n$elev[a] < pour_elev && focal$elev <= pour_elev) {
          #message("Cell = ", i)
          pour_elev <- max(n$elev[a], focal$elev)
          pp <- tibble::tibble(out_elev = n$elev[a],
                               out_seqno = n$seqno[a],
                               out_shed = n$shedno[a],
                               out_row = n$row[a],
                               out_col = n$col[a],
                               in_seqno = focal$seqno,
                               in_elev = focal$elev,
                               in_row = focal$row,
                               in_col = focal$col,
                               pour_elev = pour_elev,
                               edge_pit = any(db_w$edge_cell[1:i]),
                               pit_area = i)
        }
      }

      # min_elev only applies to current focal cell
      if(min_elev > pour_elev & focal$elev > pour_elev) {
        overspill <- TRUE
        pits <- db_w[1:pp$pit_area,] %>%
          dplyr::mutate(pour_elev = pour_elev)
        pour_point <- pp
      }
    }
    i <- i + 1
  }

  #browser()
  return(tibble::tibble(pits = list(pits), pour_point = list(pour_point)))
}

#' @import magrittr
pit_stat <- function(db, parallel = TRUE, verbose = FALSE, n_clusters = 7) {
  db <- db %>%
    dplyr::mutate(edge_cell = purrr::map_lgl(n, edge_pit))

  db_pit <- db %>%
    dplyr::filter(buffer == FALSE) %>%
    dplyr::group_by(shedno) %>%
    dplyr::mutate(shed_area = length(shedno)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(shedno, elev, upslope_n)
    #dplyr::arrange(dplyr::desc(elev)) %>%
    #dplyr::arrange(shedno, elev, dplyr::desc(upslope_n), col)
    #dplyr::arrange(shedno, elev, seqno)
#
#   browser()
#
#   db_w <- dplyr::filter(db_pit, shedno == 7)
#   which(db_w$row == 164 & db_w$col == 37)
#   db_w %>% dplyr::slice(1480:1490)
#
#   get_pour_point(db_w = db_pit[db_pit$shedno == 7,], w = 7, db = db)$pour_point

  # To confirm that arrange matches indexing in original file:
  # db_pit %>%
  #   dplyr::mutate(sort = (shedno*10^18) + (elev*10^12) + (1000000-upslope_n)) # To compare with foxpro indexing (bottomup)
  # all(db_pit$sort == sort(db_pit$sort)) # TRUE if arranging is same as indexing bottomup)=

  # For each watershed IN ORDER, calculate the pour_point (details of the point at which tip to another watershed)

  pour_point <- tibble::tibble()
  pits <- tibble::tibble()

  if(verbose) message("Assessing pour points:")

  stats <- db_pit %>%
    tidyr::nest(- shedno, .key = "db_w")

  if(!parallel) {
    stats <- stats %>%
      dplyr::group_by(shedno) %>%
      dplyr::mutate(pp = purrr::map2(db_w, shedno, ~ get_pour_point(.x, .y, db = db, verbose = verbose)))
  } else {

    cluster <- multidplyr::create_cluster(n_clusters) %>%
      multidplyr::cluster_library("magrittr") %>%
      multidplyr::cluster_assign_value("get_pour_point", get_pour_point) %>%
      multidplyr::cluster_assign_value("db", db)

    suppressWarnings({
      stats <- stats %>%
        dplyr::mutate(sort = purrr::map_dbl(db_w, nrow)) %>%
        dplyr::arrange(dplyr::desc(sort)) %>%
        dplyr::select(-sort) %>%
        multidplyr::partition(shedno, cluster = cluster) %>%
        dplyr::mutate(pp = purrr::map2(db_w, shedno,
                                       ~ get_pour_point(.x, .y, db = db, verbose = FALSE))) %>%
        dplyr::collect()
    })
  }

  stats <- stats %>%
    tidyr::unnest(pp) %>%
    dplyr::mutate(pits = purrr::map(pits,
                                    ~ dplyr::summarize(.x,
                                                       pit_vol = sum(pour_elev - elev),
                                                       pit_elev = elev[ldir == 5],
                                                       pit_seqno = seqno[ldir == 5],
                                                       pit_row = row[ldir == 5],
                                                       pit_col = col[ldir == 5],
                                                       shed_area = unique(shed_area)))) %>%
    tidyr::unnest(pits) %>%
    tidyr::unnest(pour_point) %>%
    dplyr::mutate(pre_vol = 0,
                  varatio = dplyr::if_else(shed_area > 0,
                                           pit_vol / shed_area * 1000, 0))

  stats <- stats %>%
    dplyr::left_join(dplyr::select(stats, shedno, edge_pit, pit_elev, pit_seqno, pour_elev),
                     by = c("out_shed" = "shedno"), suffix = c("", "_out")) %>%
    dplyr::mutate(db_w = purrr::map(db_w, ~ dplyr::mutate(.x, pour_elev = pour_elev)),
                  vol = purrr::map(db_w, ~ calc_vol2fl(db = .x))) %>%
    tidyr::unnest(vol) %>%
    dplyr::arrange(shedno) %>%
    dplyr::ungroup()

  if("global_shed" %in% names(db)) {
    stats <- dplyr::left_join(stats, dplyr::select(db, shedno, global_shed) %>%
                                dplyr::distinct(), by = "shedno")
  }

  return(stats)
}


# pit_stat <- function(db, verbose = FALSE) {
#   db_pit <- db %>%
#     dplyr::mutate(border = is.na(elev),
#                   edge_cell = purrr::map_lgl(n, edge_pit)) %>%
#     dplyr::group_by(shedno) %>%
#     dplyr::mutate(edge_pit = any(edge_cell),
#                   shed_area = length(shedno)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(border == FALSE) %>%
#     dplyr::arrange(shedno, elev, desc(upslope_n))
#
#   # To confirm that arrange matches indexing in original file:
#   # db_pit %>%
#   #   dplyr::mutate(sort = (shedno*10^18) + (elev*10^12) + (1000000-upslope_n)) # To compare with foxpro indexing (bottomup)
#   # all(db_pit$sort == sort(db_pit$sort)) # TRUE if arranging is same as indexing bottomup)=
#
#   # For each watershed IN ORDER, calculate the pour_point (details of the point at which tip to another watershed)
#
#   pour_point <- tibble::tibble()
#   pits <- tibble::tibble()
#
#   if(verbose) message("Assessing pour points:")
#
#   for(w in unique(db_pit$shedno)) {
#     db_w <- dplyr::filter(db_pit, shedno == w)
#
#     if(verbose)  message("  - Watershed number ", w)
#
#     pour_elev <- 1000000000
#     overspill <- FALSE
#     i <- 1
#
#     while(!overspill & i <= nrow(db_w)) {
#       focal <- db_w[i, ]
#       n <- neighbours(db_w$adjacent[[i]], db) %>%
#         dplyr::filter(shedno != w)
#
#       if(nrow(n) > 0){
#         min_elev <- min(n$elev)
#
#         for(a in 1:nrow(n)) {
#
#           if(focal$elev <= pour_elev) {
#             # message("watershed = ", w, "; focal = ", i, "; n = ", a, ": pour_elev = ",
#             #        pour_elev, ", focal elev = ", focal$elev, ", neighbour elev = ", n$elev[a])
#
#             if(n$elev[a] < pour_elev) {
#               pour_elev <- max(n$elev[a], focal$elev)
#               pp <- tibble::tibble(shedno = w,
#                                    out_elev = n$elev[a],
#                                    out_seqno = n$seqno[a],
#                                    out_shed = n$shedno[a],
#                                    out_row = n$row[a],
#                                    out_col = n$col[a],
#                                    in_seqno = focal$seqno,
#                                    in_elev = focal$elev,
#                                    in_row = focal$row,
#                                    in_col = focal$col,
#                                    pour_elev = pour_elev,
#                                    pit_area = i)
#             }
#           }
#         }
#         if(min_elev > pour_elev & focal$elev > pour_elev) {
#           overspill <- TRUE
#           pits <- dplyr::bind_rows(pits, db_w[1:pp$pit_area,])
#           pour_point <- dplyr::bind_rows(pour_point, pp)
#         }
#       }
#       i <- i + 1
#     }
#   }
#
#   s <- pits %>%
#     dplyr::left_join(pour_point, by = "shedno") %>%
#     dplyr::group_by(shedno) %>%
#     dplyr::summarize(pit_vol = sum(pour_elev - elev),
#                      edge_pit = unique(edge_pit),
#                      pit_elev = elev[ldir == 5],
#                      pit_seqno = seqno[ldir == 5],
#                      pit_row = row[ldir == 5],
#                      pit_col = col[ldir == 5],
#                      shed_area = unique(shed_area)) %>%
#     dplyr::ungroup() %>%
#     dplyr::left_join(pour_point, by = "shedno") %>%
#     dplyr::mutate(pre_vol = 0, VARATIO = 0,
#                   VARATIO = replace(VARATIO, shed_area > 0,
#                                     pit_vol / shed_area[shed_area > 0] * 1000)) %>%
#     dplyr::left_join(tidyr::nest(pits, -shedno, .key = pit_cells), by = "shedno")
#
#   s <- s %>%
#     dplyr::left_join(dplyr::select(s, shedno, edge_pit, pit_elev, pit_seqno, pour_elev),
#                      by = c("out_shed" = "shedno"), suffix = c("_in", "_out")) %>%
#     dplyr::rename(edge_pit = edge_pit_in, pit_elev = pit_elev_in, pit_seqno = pit_seqno_in, pour_elev = pour_elev_in)
#
#   s <- dplyr::left_join(s, calc_vol2fl(db = db, stats = s), by = c("shedno", "shed_area"))
#
#   return(s)
# }

#dplyr::filter(db_pit, overspill == TRUE)

# Equivalent to calculate min_elev as in iteration, but slower:

# Calculate shed_area and overflow point based on min_elev and elev values
# get_min_elev <- function(n) {
#   s <- n$shedno[n$index == 5]
#   n <- n %>%
#     dplyr::filter(index != 5, shedno != s)
#   if(nrow(n) > 0) return(min(n$elev, na.rm = TRUE))
#   if(nrow(n) == 0) return(NA)
# }
# temp <- db_pit %>%
#   dplyr::select(elev, seqno, adjacent, n, shedno) %>%
#   dplyr::left_join(pour_point, by = "shedno") %>%
#   dplyr::mutate(n = purrr::map(adjacent, ~ neighbours(.x, db = db[, c("seqno", "elev", "shedno")]))) %>%
#   dplyr::mutate(min_elev = purrr::map(n, min_elev)) %>%
#   tidyr::unnest(min_elev) %>%
#   dplyr::group_by(shedno) %>%
#   dplyr::summarize(overspill = seqno[!is.na(min_elev) & elev > pour_elev & min_elev > pour_elev][1],
#                    shed_area = which(seqno == overspill))


# for each watershed look at slices of elevations, calculate the volumes and add together
#' @import magrittr
calc_vol2fl <- function(db) {

  if(any(db$shed_area <= 0)) message("Shed area <= 0, is this reasonable?")

  db %>%
    #dplyr::arrange(shedno, elev, desc(upslope_n)) %>%
    dplyr::arrange(elev, desc(upslope_n)) %>%
    #dplyr::left_join(stats, by = "shedno") %>%
    #dplyr::group_by(shedno, shed_area) %>%
    dplyr::filter(elev <= pour_elev) %>%
    #dplyr::group_by(shedno, shed_area, elev) %>%
    dplyr::group_by(shed_area, elev) %>%
    dplyr::summarize(n = length(elev)) %>%
    dplyr::mutate(last_elev = dplyr::lag(elev),
                  elev_diff = (elev - last_elev) * 1000,
                  curr_vol = elev_diff * n,
                  curr_vol = replace(curr_vol, is.na(curr_vol), 0.1)) %>%
    dplyr::summarize(#n = sum(n),
                     curr_vol = sum(curr_vol)) %>%
    dplyr::mutate(curr_mm2fl = dplyr::if_else(shed_area > 0,
                                              curr_vol/shed_area,
                                              curr_vol/1)) %>%
    dplyr::select(-shed_area)
}
