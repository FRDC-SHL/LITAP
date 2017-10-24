join_slopes <- function(u1, u2) {
  return(na_omit(c(u1, u2)))
}

get_track <- function(shed_list, track) {
  t <- sapply(lapply(shed_list, FUN = function(x, track) x[track %in% x], track), length)
  t <- as.numeric(names(t[t > 0]))
  return(t)
}

#' @export
calc_shed4 <- function(db, verbose = FALSE) {

  db_orig <- db

  npits <- sum(db$ldir == 5, na.rm = TRUE)

  db <- db %>%
    dplyr::mutate(shedno = NA,
                  shedno = replace(shedno, ldir == 5 & !is.na(ldir), 1:npits)) %>%
    dplyr::select(seqno, elev, drec, shedno)

  # Assigne each cell to a watershed by climbing UP
  n1 <- length(db$shedno[!is.na(db$shedno)])
  n2 <- 0
  while(n1 != n2){
    db <- dplyr::mutate(db, shedno = db$shedno[db$drec])
    n2 <- n1
    n1 <- length(db$shedno[!is.na(db$shedno)])
  }

  # Relabel to match top down process
  shed_order <- db %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    dplyr::filter(!is.na(shedno)) %>%
    dplyr::pull(shedno) %>%
    unique()
  db <- dplyr::mutate(db, shedno = as.numeric(as.character(factor(shedno, levels = shed_order, labels = 1:npits))))

  # Create sub-watershed seqno's for quicker referencing
  db <- db %>%
    dplyr::arrange(seqno) %>%
    dplyr::group_by(shedno) %>%
    dplyr::mutate(seqno_shed = 1:length(seqno)) %>%
    dplyr::ungroup()
  db$drec_shed <- db$seqno_shed[db$drec]

  # Calculate upslope flow for each cell by watershed
  if(verbose) message("  Getting upslope flow and area")
  db <- db %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    dplyr::mutate(upslope = 0) %>%
    tidyr::nest(-shedno, .key = "db_w") %>%
    dplyr::mutate(db_w = purrr::map2(db_w, shedno, ~get_upslope3(.x, .y[1]))) %>%
    tidyr::unnest(db_w) %>%
    dplyr::arrange(seqno) %>%
    dplyr::mutate(initial_shed = shedno)

  # Merge with original data
  db <- dplyr::left_join(db, db_orig, by = c("seqno", "elev", "drec"))

  # Calculate ridge lines
  db <- db %>%
    dplyr::select(seqno, initial_shed, buffer) %>%
    nb_values(max_cols = max(db$col), col = "initial_shed") %>%
    dplyr::filter(buffer == FALSE) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(ridge = length(unique(initial_shed_n[!is.na(initial_shed_n)])) > 1) %>%
    dplyr::right_join(db, by = "seqno")

  return(db)
}


#' @export
calc_shed3 <- function(db, verbose = FALSE) {

  db_orig <- db

  # Give the order
  seqno_list <- db %>%
    dplyr::filter(!is.na(elev)) %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    dplyr::pull(seqno)

  # Give the file to grab data from
  # (quicker to grab data by index, so sort by seqno and keep all cells)
  db <- db %>%
    dplyr::arrange(seqno) %>%
    dplyr::select(seqno, drec)

  total <- length(seqno_list)
  #total <- 25000
  t <- round(total/100)

  if(verbose) {
    pb <- progress::progress_bar$new(
      format = "  Processing cells [:bar] :percent",
      total = 100, clear = FALSE, width = 80)
  }

  shed_count <- 1

  shed_list <- tibble::tibble()
  seqno_done <- vector()

  if(verbose) message("  Assigning watersheds")
  for(i in 1:total){
    if(verbose) {
      if((i %% t) == 0) pb$tick()
    }

    cell <- seqno_list[i]

    # If no shed number assigned
    if(!(cell %in% seqno_done)){

      track <- trace_flow2(cell, db)

      if(nrow(shed_list) > 0) shed <- shed_list$shedno[purrr::map_lgl(shed_list$seqno, function(x) any(track %in% x))] else shed <- logical()

      if(length(shed) > 0) {
        shed <- as.numeric(shed)
      } else {
        shed <- shed_count
        shed_count <- shed_count + 1
      }

      # Which seqno in which sheds
      if(nrow(shed_list) > 0 && shed %in% shed_list$shedno){
        shed_list$seqno[shed] <- list(unique(c(shed_list$seqno[shed][[1]], track)))
      } else {
        shed_list <- dplyr::bind_rows(shed_list, tibble::tibble(shedno = shed, seqno = list(track)))
      }
      seqno_done <- unique(c(seqno_done, track))
    }
  }

  if(verbose) message("  Getting upslope flow")
  db_sheds <- tidyr::unnest(shed_list, seqno) %>%
    dplyr::full_join(db_orig, by = "seqno") %>%
    dplyr::mutate(sheds = dplyr::if_else(is.na(shedno), FALSE, TRUE)) %>%
    dplyr::arrange(shedno, dplyr::desc(elev)) %>%
    tidyr::nest(-shedno, .key = "db_w") %>%
    dplyr::mutate(db_w = purrr::map2(db_w, shedno, ~get_upslope(.x, .y[1]))) %>%
    tidyr::unnest(db_w) %>%
    dplyr::arrange(seqno) %>%
    dplyr::mutate(initial_shed = shedno)

  return(db_sheds)
}

get_upslope <- function(db_w, w){
  db_w$upslope <- NA
  db_ref <- dplyr::arrange(db_w, seqno_shed)
  if(!is.na(w)) {

    for(cell in db_w$seqno_shed){
      # If no shedno yet
      if(is.na(db_ref$upslope[cell])){

        track <- trace_flow3(cell, db_ref)

        up_new <- lapply(1:length(track), function(x) track[1:which(track == track[x])])
        up_old <- db_ref$upslope[track]
        for(i in 1:length(track)) up_new[[i]] <- na_omit(unique(c(up_new[[i]], up_old[[i]])))
        db_ref$upslope[track] <- up_new
      }
    }
  }
  return(db_ref)
}

get_upslope2 <- function(db_w, w){
  db_ref <- dplyr::arrange(db_w, seqno_shed)
  if(!is.na(w)) {
    for(cell in db_w$seqno_shed){
      if(db_ref$upslope[cell] == 0){
        track <- trace_flow3(cell, db_ref)
        up_new <- 1:length(track)
        up_current <- db_ref$upslope[track]
        up_new[up_current != 0] <- dplyr::last(up_new[up_current == 0])
        db_ref$upslope[track] <- db_ref$upslope[track] + up_new
      }
    }
  }
  return(db_ref)
}

get_upslope3 <- function(db_w, w){
  db_ref <- dplyr::arrange(db_w, seqno_shed)
  flow <- get_all_flow(db_ref)

  if(!is.na(w)) {
    for(cell in db_w$seqno_shed){
      if(db_ref$upslope[cell] == 0){
        track <- na_omit(flow[cell, ])
        up_new <- 1:length(track)
        up_current <- db_ref$upslope[track]
        up_new[up_current != 0] <- dplyr::last(up_new[up_current == 0])
        db_ref$upslope[track] <- db_ref$upslope[track] + up_new
      }
    }
  }
  return(db_ref)
}

get_all_flow <- function(db) {
  db$drec_shed[db$drec_shed == db$seqno_shed] <- NA
  m <- matrix(db$seqno_shed, ncol = 1)
  end <- FALSE
  while(!end){
    m <- cbind(m, db$drec_shed[m[,ncol(m)]])
    if(all(is.na(m[,ncol(m)]))) end <- TRUE
  }
  return(m)
}

#' @export
calc_shed2 <- function(db, verbose = FALSE) {
  db_orig <- db %>%
    dplyr::arrange(seqno) %>%
    dplyr::select(seqno, elev, drec)

  db <- db %>%
    dplyr::arrange(dplyr::desc(elev), seqno)

  pb <- progress::progress_bar$new(
    format = "  Processing cells [:bar] :percent",
    total = length(db$seqno[!is.na(db$elev)][1:5000]), clear = FALSE, width = 80)

  shed_count <- 1

  db_new <- tibble::tibble(seqno = NA, shedno = NA, upslope = NA)

  for(cell in db$seqno[!is.na(db$elev)][1:5000]){
    pb$tick()

    if(!(cell %in% db_new$seqno)){ # If no shed number assigned

      track <- trace_flow2(cell, db_orig)

      shed <- db_new$shedno[db_new$seqno %in% track]
      if(length(shed) > 0) {
        shed <- shed[1]
      } else {
        shed <- shed_count
        shed_count <- shed_count + 1
      }

      # List upslope cells and concatenate to data frame
      up_new <- lapply(1:length(track), function(x) track[1:which(track == track[x])])
      up_old <- db_new$upslope[match(track, db_new$seqno)]
      for(i in 1:length(track)) up_new[[i]] <- na_omit(unique(c(up_new[[i]], up_old[[i]])))

      up <- db_orig[track, ]
      up$upslope <- up_new
      up$shedno <- shed

      rm <- na_omit(match(up$seqno, db_new$seqno))
      if(length(rm) > 0) db_new <- db_new[-rm,]

      db_new <- dplyr::bind_rows(db_new, up)
    }
  }

  db_new <- dplyr::left_join(db_new, db, by = c("seqno", "elev", "drec")) %>%
    dplyr::bind_rows(db[is.na(db$elev),]) %>%
    dplyr::slice(-1) %>%
    dplyr::arrange(seqno)

  #if(!all(db_new$seqno == db_orig$seqno)) browser()

  # Save as initial shed
  db_new <- dplyr::mutate(db_new, initial_shed = shedno)

  return(db_new)
}


#' @export
calc_shed <- function(db, verbose = FALSE) {
  db <- db %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    dplyr::mutate(shedno = NA,
                  upslope = NA)

  pb <- progress::progress_bar$new(
    format = "  Processing cells [:bar] :percent",
    total = length(db$seqno), clear = FALSE, width = 80)

  shed_count <- 1

  for(cell in db$seqno){
    pb$tick()

    if(is.na(db$shedno[db$seqno == cell])){ # If no shed number assigned
      track <- trace_flow(cell, db)

      # Any of these cells already assigned?
      shed <- db$shedno[db$seqno %in% track & !is.na(db$shedno)]

      if(length(shed) > 0) {
        shed <- shed[1]
      } else {
        shed <- shed_count
        shed_count <- shed_count + 1
      }

      # List upslope cells and concatenate to data frame
      up <- tibble::tibble(seqno = track,
                           upslope_new = lapply(1:length(track), function(x) track[1:which(track == track[x])])) %>%
        dplyr::left_join(db, by = "seqno") %>%
        dplyr::mutate(upslope = purrr::map2(upslope_new, upslope, ~join_slopes(.x, .y)))

      up <- up[, names(up) != "upslope_new"]

      # Assign shed number and continue
      # Omit values which are in up

      db$upslope[match(up$seqno, db$seqno)] <- up$upslope
      db$shedno[db$seqno %in% track] <- shed
    }
  }

  # Save as initial shed
  db <- dplyr::mutate(db, initial_shed = shedno) %>%
    dplyr::mutate(upslope = purrr::map(upslope, unique))

  return(db)
}

calc_ups <- function(db) {
  db %>%
    dplyr::rowwise() %>%
    dplyr::mutate(upslope_n = length(upslope)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(seqno)
}
