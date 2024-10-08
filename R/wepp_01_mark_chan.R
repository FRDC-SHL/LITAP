mark_chan <- function(db, upslope_threshold) {

  channels <- db %>%
    dplyr::filter(.data$upslope > .env$upslope_threshold) %>%
    dplyr::arrange(dplyr::desc(.data$elev)) %>%
    dplyr::mutate(chan_no = 0,
                  seed_type = 0)

  # Check if neighbouring cells are channels (higher elev, lower seqno)
  chan <- check_neighbours(db, channels, upslope_threshold) %>%
    dplyr::filter(!.data$neighbour_channel)

  chan <- get_chan(seqno = chan$seqno, drec = db$drec,
                   upslope = db$upslope, elev = db$elev, ddir = db$ddir,
                   upslope_threshold = upslope_threshold) %>%
    dplyr::rename("elev" = "new_elev")

  dplyr::bind_cols(dplyr::rename(db, "orig_elev" = "elev"), chan)
}

check_neighbours <- function(db, channels, upslope_threshold) {
  db %>%
    nb_values(max_cols = max(db$col), db_sub = channels,
              format = "wide", col = c("upslope", "elev", "seqno")) %>%
    dplyr::mutate(neighbour_channel = purrr::pmap_lgl(
      list(.data$elev_n1, .data$elev_n2, .data$elev_n3, .data$elev_n4, .data$elev_n5,
           .data$elev_n6, .data$elev_n7, .data$elev_n8, .data$elev_n9,
           .data$upslope_n1, .data$upslope_n2, .data$upslope_n3, .data$upslope_n4, .data$upslope_n5,
           .data$upslope_n6, .data$upslope_n7, .data$upslope_n8, .data$upslope_n9,
           .data$seqno_n1, .data$seqno_n2, .data$seqno_n3, .data$seqno_n4, .data$seqno_n5,
           .data$seqno_n6, .data$seqno_n7, .data$seqno_n8, .data$seqno_n9),
      mark_neighbours, upslope_threshold = upslope_threshold))
}

mark_neighbours <- function(..., upslope_threshold) {
  #Check which at a higher elevation
  elev <- c(...)[1:9]
  upslope <- c(...)[10:18]
  seqno <- c(...)[19:27]
  # If neighbouring upslope greater than the threshold AND neighbour at a higher elevation
  # OR if at same elevation, is at a lower seqno, so would be evaluated first
  # THEN neighbour is the true channel cell.
  any(upslope > upslope_threshold &
        ((elev > elev[5]) | (elev == elev[5] & seqno < seqno[5])), na.rm = TRUE)
}


get_chan <- function(seqno, drec, upslope, elev, upslope_threshold, ddir, fix_elev = TRUE) {
  variable <- list(chan_no = rep(0, length = length(drec)),
                   seedtype = rep(0, length = length(drec)),
                   new_elev = elev,
                   n = 1)

  static <- list(upslope = upslope,
                 upslope_threshold = upslope_threshold,
                 ddir = ddir)

  if(length(seqno) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- trace(seqno = seqno, drec = drec, loop_func = mark_cut_chan,
                    s = static, v = variable)

  data.frame(variable[c("chan_no", "seedtype", "new_elev")])
}

mark_cut_chan <- function(t, s, v) {

  if(any(v$chan_no[t] == 0)) {

    if(any(last <- v$chan_no[t] != 0)) {
      last <- which(last)[1] - 1
      t <- t[1:last]
    }

    ups_diff <- s$upslope[t] - dplyr::lag(s$upslope[t])

    v$chan_no[t] <- v$n
    v$seedtype[t[1]] <- 1 # Length one gets seed type 1
    # Where diff between current upslope and previous is greater than threshold
    v$seedtype[t[ups_diff > s$upslope_threshold]] <- 2

    # Seedtype 5 only if pit cell
    pits <- t[s$ddir[t] == 5]
    v$seedtype[pits] <- 5

    v$n <- v$n + 1
  }
  v
}
