
#' @export
calc_shed <- function(db) {
  db <- db %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    dplyr::mutate(shedno = NA,
                  trackno = NA,
                  meets_cell = NA,
                  upslope = NA)

  #trib <- tibble::tibble()

  pb <- progress::progress_bar$new(
    format = "  Processing cells [:bar] :percent eta: :eta",
    total = length(db$seqno), clear = FALSE, width = 80)

  shed_count <- 1
  track_count <- 1
  for(cell in db$seqno){
    pb$tick()
    if(is.na(db$shedno[db$seqno == cell])){ # If no shed number assigned
      track <- trace_flow(cell, db)

      if(length(track) > 1) {

        # Any of these cells already assigned?
        shed <- dplyr::filter(db, seqno %in% track, !is.na(shedno)) %>%
          .$shedno

        if(length(shed) > 0) {
          shed <- shed[1]
        } else {
          shed <- shed_count
          shed_count <- shed_count + 1
        }

        # List upslope cells and concatenate to data frame
        up <- tibble::tibble(seqno = track, upslope_new = lapply(1:length(track), function(x) track[1:which(track == track[x])])) %>%
          dplyr::left_join(db, by = "seqno") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(upslope = list(unique(na_omit(c(unlist(upslope), unlist(upslope_new)))))) %>%   ###SLOW!
          dplyr::ungroup() %>%
          dplyr::select(-upslope_new)

        #up <- lapply(1:length(track), function(x) unique(c(na_omit(unlist(db$upslope[db$seqno == track[x]])), unlist(up[[x]]))))

        # Assign shed number and continue
        db <- db %>%
          dplyr::anti_join(up, by = "seqno") %>%
          dplyr::bind_rows(up) %>%
          dplyr::mutate(shedno = replace(shedno, seqno %in% track, shed),
                        trackno = replace(trackno, is.na(trackno) & (seqno %in% track), track_count),
                        meets_cell = replace(meets_cell, is.na(meets_cell) & (seqno %in% track), track[length(track)])) %>%
          dplyr::arrange(dplyr::desc(elev), seqno)


         #db$upslope[db$seqno == track[length(track)]] <- list(c(unlist(db$upslope[db$seqno == track[length(track)]]), track[-length(track)]))


        # #line <- sp::Line(db[db$seqno %in% track, c("row", "col"),])
        # trib <- dplyr::bind_rows(trib, tibble::tibble(db = list(db[db$seqno %in% track,]),
        #                                               trackno = track_count,
        #                                               shed = shed))
        track_count <- track_count + 1
      }
    }
  }

  # trib <- tidyr::unnest(trib)
  #
  # flow_plot(db) +
  #   geom_point() +
  #   geom_path(data = trib,
  #             aes(x = cols, y = rows, group = trackno, colour = factor(shed)), size = 1, inherit.aes = FALSE) +
  #   geom_point(data = db[db$ldir == 5,], size = 4)

  # Save as initial shed
  db <- dplyr::mutate(db, initial_shed = shedno)

  return(db)
}

calc_ups <- function(db) {
  db %>%
    dplyr::rowwise() %>%
    dplyr::mutate(upslope_n = length(upslope)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(seqno)
}
