join_slopes <- function(u1, u2) {
  return(na_omit(c(u1, u2)))
}

get_track <- function(shed_list, track) {
  t <- sapply(lapply(shed_list, FUN = function(x, track) x[track %in% x], track), length)
  t <- as.numeric(names(t[t > 0]))
  return(t)
}

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

calc_ups <- function(db) {
  db %>%
    dplyr::rowwise() %>%
    dplyr::mutate(upslope_n = length(upslope)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(seqno)
}
