#' Calculate channel segments
#'
#' From original FoxPro code:
#'
#' Procedure to traverse each marked channel in a processed DEM and to
#' identify, label and store relevant data for each channel segment in
#' each identified channel.  In the process of traversing each channel
#' segment, the program searches the 3x3 neighborhood around each channel cell
#' to identify all cells adjacent to a channel that drain into a channel.
#' The program then labels each of these adjacent cells as occurring on a
#' left side(1), right side (2) or top (3) of a channel.  This is necessary
#' in order to be able to determine whether a collection of cells that
#' drain into a marked channel drain in from the left side, right side or top.
#' WEPP distinguishes these three types of hillslopes (e.g. left, right and top)
#'
#' @noRd
#'
calc_segments <- function(db, grid) {

  # Add seedtype 8 after any seedtype 4/5
  fix <- db %>%
    dplyr::filter(.data$seedtype %in% c(4, 5),
                  .data$drec != .data$seqno,
                  .data$chan_no != 0) %>%
    dplyr::pull(.data$drec)

  fix <- db[fix, ] %>%
    dplyr::filter(.data$seedtype == 0) %>%
    dplyr::pull(.data$seqno)

  db$seedtype[fix] <- 8

  db <- dplyr::mutate(db,
                      segment_no = 0,
                      chan_side = 0,
                      seg_no = as.character(1:dplyr::n()))

  # Calc Channel Side - Only when FLOWING DOWN CHANNELL!
  chans <- dplyr::filter(db, .data$chan_no > 0)
  db <- dplyr::mutate(db, chan_side = sides(db_sub = chans, db = db))

  # Calculate segments
  calc <- db %>%
    dplyr::filter(seedtype %in% c(1,2,8,5)) %>%
    dplyr::arrange(dplyr::desc(elev)) %>%
    calc_segs(channels = ., db = db)

  segs <- calc$segs
  db <- calc$db

  # Relabel segments
  segs <- segs %>%
    dplyr::mutate(orig_id = .data$initial_id) %>%
    dplyr::arrange(dplyr::desc(.data$end_elev), .data$initial_id) %>%
    dplyr::mutate(sort_order = as.integer(1:dplyr::n()),
                  len_meters = .data$len_cells * grid)

  db <- dplyr::left_join(db, dplyr::select(segs, "sort_order", "orig_id"),
                         by = c("segment_no" = "orig_id")) %>%
    dplyr::select(-"segment_no") %>%
    dplyr::rename("segment_no" = "sort_order") %>%
    dplyr::mutate(segment_no = as.integer(tidyr::replace_na(segment_no, 0)))

  segs <- dplyr::select(segs, -"orig_id")

  list(segs = segs, db = db)
}


calc_segs <- function(channels, db) {

  variable <- list(segs = data.frame(), db = db)

  static <- list(channels = channels)

  if(length(channels$seqno) > 500) trace <- trace_matrix else trace <- trace_single

  variable <- trace(seqno = channels$seqno, drec = db$drec,
                    loop_func = calc_segs_trace, s = static, v = variable)

  variable
}

calc_segs_trace <- function(t, s, v) {

  db <- v$db

  segs <- db[t[1],] %>%
    dplyr::select("initial_id" = "seg_no",
                  "start_seqno" = "seqno",
                  "start_row" = "row",
                  "start_col" = "col",
                  "start_elev" = "elev",
                  "start_type" = "seedtype") %>%
    dplyr::mutate(impound = FALSE)

  # Now deal with special cases where two seedtype 2s in a row
  if(length(t) == 1) {
    last <- 1
  } else if (all(db$seedtype[t[1:2]] == 2)) {
    last <- 2
  } else if (length(t) > 2) {
    # skip over first 2 or 8 seedtype if occurrs immediate after start
    last <- which(db$seedtype[t[-c(1:2)]] != 0)[1] + 2
  } else last <- length(t)

  if(!is.na(last)) { # Circling drec issues
    channels <- db[t[1:last], ]

    if(channels$seedtype[1] == 5){
      segs[, c("end_seqno", "end_row", "end_col", "end_elev", "end_type")] <-
        channels[1, c("seqno", "row", "col", "elev", "seedtype")]
      segs$len_cells <- 1
      segs <- dplyr::mutate(segs, initial_id = paste0(.data$initial_id, ".1"),
                            impound = TRUE) %>%
        dplyr::bind_rows(segs, .)

    } else {
      segs$len_cells <- nrow(channels)
      segs[, c("end_seqno", "end_row", "end_col", "end_elev", "end_type")] <-
        channels[nrow(channels), c("seqno", "row", "col", "elev", "seedtype")]
    }

    # Add ids to db
    end <- channels$seqno
    db$segment_no[end][db$segment_no[end] == 0] <- segs$initial_id[1]

    v$segs <- dplyr::bind_rows(v$segs, segs)
    v$db <- db
  }

  v
}

























sides <- function(db_sub, db) {
  n <- nb_values(db, max_cols = max(db$col), col = "seqno",
                 db_sub = db_sub, format = "wide")

  sides <- db$chan_side

  # Calculate chan_side for seedtype 1
  sides <- sides1(n, db, sides)

  # Calculate sides for seedtype X (i.e. not 1)
  sides <- sidesX(n, db, sides)

  sides
}

# Get chan_side for seedtype 1
sides1 <- function(n, db, sides) {
  n <- dplyr::filter(n, .data$seedtype == 1)

  for(i in c(1:4, 6:9)) {
    n_seqno <- n[[paste0("seqno_n", i)]]
    n_chan_no <- db$chan_no[n_seqno]
    n_drec <- db$drec[n_seqno]
    sides[n_seqno[n_chan_no == 0 & n_drec == n$seqno]] <- 1
  }
  sides
}

# Get chan_side for seedtype X (not 1)
sidesX <- function(n, db, sides) {
  n <- dplyr::filter(n, .data$seedtype != 1)

  a <- c(7, 8, 9, 4, 5, 6, 1, 2, 3)
  seqno <- n$seqno
  max_upslope <- rep(0, nrow(n))
  up_dir <- rep(0, nrow(n))
  down_dir <- rep(0, nrow(n))

  max_upslope_drains <- rep(0, nrow(n))
  max_upslope_dir_drains <- rep(0, nrow(n))

  drains <- matrix(data = 0, nrow = nrow(n), ncol = 9)

  for(i in c(1:4, 6:9)) {
    n_seqno <- n[[paste0("seqno_n", i)]]
    n_drec <- db$drec[n_seqno]
    n_chan_no <- db$chan_no[n_seqno]
    n_upslope <- db$upslope[n_seqno]

    # For Marked Channels draining to junction
    ups <- !is.na(n_drec) & n_drec == seqno & n_chan_no != 0
    which_max <- ups & !is.na(n_upslope) & n_upslope > max_upslope

    max_upslope[which_max] <- n_upslope[which_max]
    up_dir[which_max] <- a[i] # Reverse directions

    # For Unmarked Channels draining to junction
    not_ups <- !ups & !is.na(n_drec) & n_drec == seqno & n_chan_no == 0
    which_max <- not_ups & !is.na(n_upslope) & n_upslope > max_upslope_drains

    max_upslope_drains[which_max] <- n_upslope[which_max]
    max_upslope_dir_drains[which_max] <- a[i]

    drains[not_ups, i] <- n_seqno[not_ups]

    # Get downslope cell
    which_downs <- !ups & !not_ups & n$drec == n_seqno
    down_dir[which_downs] <- a[i]

  }

  # Get upper channel cell from drainage cells if necessary
  which_replace <- up_dir == 0 & max_upslope_dir_drains != 0
  up_dir[which_replace] <- max_upslope_dir_drains[which_replace]

  # Get chan_side
  for(i in c(1:4, 6:9)) {
    if(any(drains[,i] != 0)) {
      u <- up_dir[drains[,i] != 0]
      d <- down_dir[drains[,i] != 0]

      side3 <- (d == 1 &
                  ((u == 2) |
                     (u == 3 & a[i] > 3) |
                     (u == 6 & a[i] > 3) |
                     (u == 7 & a[i] == 4) |
                     (u == 8 & a[i] %in% c(4, 7)) |
                     (u == 9 & a[i] %in% c(4, 7, 8, 9)))) |
        (d == 2 &
           ((u == 3) |
              (u == 4 & a[i] == 1) |
              (u == 6 & a[i] != 3) |
              (u == 7 & a[i] %in% c(1, 4)) |
              (u == 8 & a[i] %in% c(1, 4, 7)) |
              (u == 9 & a[i] %in% c(1, 2, 4, 7, 8, 9)))) |
        (d == 3 &
           ((u == 1 & a[i] == 2) |
              (u == 4 & a[i] < 3) |
              (u == 6) |
              (u == 7 & a[i] < 5) |
              (u == 8 & !a[i] %in% c(6, 9)) |
              (u == 9 & a[i] != 6))) |
        (d == 4 &
           ((u == 1) |
              (u == 2 & a[i] != 1) |
              (u == 3 & a[i] >= 3) |
              (u == 6 & a[i] >= 4) |
              (u == 8 & a[i] == 7) |
              (u == 9 & a[i] %in% c(7, 8)))) |
        (d == 6 &
           ((u == 1 & a[i] < 4) |
              (u == 2 & a[i] == 3) |
              (u == 4 & a[i] < 4) |
              (u == 7 & a[i] <= 7) |
              (u == 8 & a[i] != 9) |
              (u == 9))) |
        (d == 7 &
           ((u == 1 & a[i] != 4) |
              (u == 2 & !a[i] %in% c(1, 4)) |
              (u == 3 & a[i] >= 5) |
              (u == 4) |
              (u == 6 & a[i] > 7) |
              (u == 9 & a[i] == 8))) |
        (d == 8 &
           ((u == 1 & !a[i] %in% c(4, 7)) |
              (u == 2 & !a[i] %in% c(1, 4, 7)) |
              (u == 3 & a[i] %in% c(6, 9)) |
              (u == 4 & a[i] != 7) |
              (u == 6 & a[i] == 9) |
              (u == 7))) |
        (d == 9 &
           ((u == 1 & a[i] %in% c(2, 3, 6)) |
              (u == 2 & a[i] %in% c(3, 6)) |
              (u == 3 & a[i] == 6) |
              (u == 4 & a[i] < 7) |
              (u == 7 & a[i] != 8) |
              (u == 8)))

      side3 <- side3
      side2 <- !side3

      sides[drains[,i][drains[,i] != 0][side3]] <- 3
      sides[drains[,i][drains[,i] != 0][side2]] <- 2

    }

  }
  sides
}


assign_seg_no <- function(db, segs) {
  db$segment_no[segs$end_seqno][db$segment_no[segs$end_seqno] == 0] <-
    segs$initial_id[db$segment_no[segs$end_seqno] == 0]
  db
}
