merge_chan <- function(db) {
  db <- db %>%
    dplyr::mutate(orig_chan_no = chan_no,
                  orig_drec = drec,
                  orig_seedtype = seedtype)

  channels <- dplyr::arrange(db, elev) %>%
    dplyr::filter(chan_no != 0)

  channels <- nb_values(db, max_cols = max(db$col), db_sub = channels,
                        format = "wide", col = "seqno")

  for(i in c(1:4, 6:9)){
    n <- db[channels[[paste0("seqno_n", i)]],]
    # For channel neighbours
    w <- !n$buffer &                      # Neighbour is not buffer
      n$chan_no != 0 &                    # Neighbour is channel
      n$chan_no != channels$chan_no &     # Not the same channel
      n$drec != channels$seqno &          # Neighbour doesn't point to focal
      n$elev >= channels$elev &           # Neighbour above or same as focal
      n$ddir == channels$ddir             # Flow direction is parallel

    # Assign neighbour to flow into channel where this is true
    db_drec_new <- db
    db_drec_new$drec[n$seqno][w] <- channels$seqno[w]

    # Check for circular flow
    if(any(w)) w[w] <- check_circular_flow(n$seqno[w], db_drec_new)
    db$drec[n$seqno][w] <- channels$seqno[w]
    db$chan_no[n$seqno][w] <- 0
    db$seedtype[n$seqno][w] <- 0

    # For non-channel neighbours - Get them to flow into channel as soon as possible
    w <- !n$buffer &                       # Neighbour is not buffer
      n$chan_no == 0 &                     # Neighbour is not channel
      n$fill_shed == channels$fill_shed &  # Neighbour is same watershed
      n$drec != channels$seqno &           # Neighbour doesn't already flow to channel cell
      n$elev >= channels$elev              # Neighbour same elev or higher than channel cell

    w[is.na(w)] <- FALSE
    # Point non-channel neighbour cell flow dir to channel cell
    db$drec[n$seqno][w] <- channels$seqno[w]

  }

  db

}

check_circular_flow <- function(seqno, db_full) {
  w <- vector(length = length(seqno))

  # Which have circular flow?
  for(i in seq_along(seqno)) w[i] <- trace_flow_fast_circ(seqno[i], db_full$drec)

  # Return ones to keep
  !w
}

channel_neighbour <- function(...){
  chan_no <- c(...)[1:9]
  drec <- c(...)[10:18]
  shedno <- c(...)[19:27]

  new_drec <- drec[5]


}

