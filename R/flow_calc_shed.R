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

  npits <- sum(db$ddir == 5, na.rm = TRUE)

  db <- db %>%
    dplyr::mutate(shedno = NA,
                  shedno = replace(shedno, ddir == 5 & !is.na(ddir), 1:npits)) %>%
    dplyr::select(seqno, elev, drec, shedno)

  # Assign each cell to a watershed by climbing UP
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

  # Calculate upslope and elev diff
  if(verbose) message("  Getting upslope flow and cumulative elevation differences")
  db <- calc_upslopes(db, type = "upslope")

  # Save initial values
  db <- dplyr::mutate(db,
                      initial_shed = shedno)

  # Merge with original data
  db <- dplyr::left_join(db, db_orig, by = c("seqno", "elev", "drec"))

  # Calculate ridge lines
  db %>%
    dplyr::select(seqno, initial_shed, buffer) %>%
    nb_values(max_cols = max(db$col), col = "initial_shed") %>%
    dplyr::filter(buffer == FALSE) %>%
    dplyr::group_by(seqno) %>%
    dplyr::summarize(ridge = length(unique(initial_shed_n[!is.na(initial_shed_n)])) > 1) %>%
    dplyr::right_join(db, by = "seqno") %>%
    dplyr::arrange(.data$seqno)
}

calc_upslopes <- function(db, type = c("upslope", "elev_diff")) {
  # Create sub-watershed seqno's for quicker referencing
  db <- db %>%
    dplyr::arrange(seqno) %>%
    dplyr::group_by(shedno) %>%
    dplyr::mutate(seqno_shed = 1:length(seqno)) %>%
    dplyr::ungroup()

  db$drec_shed <- db$seqno_shed[db$drec]

  # Calculate upslope flow for each cell by watershed
  db %>%
    dplyr::arrange(dplyr::desc(elev), seqno) %>%
    tidyr::nest(db_w = c(-shedno)) %>%
    dplyr::mutate(db_w = purrr::map2(db_w, shedno,
                                     ~get_upslope3(.x, .y[1], type = type))) %>%
    tidyr::unnest(db_w) %>%
    dplyr::arrange(seqno)
}

get_upslope3 <- function(db, w, type = c("upslope", "elev_diff")){
  o <- db$seqno_shed
  db <- dplyr::arrange(db, seqno_shed)
  flow <- get_all_flow(db)

  if("upslope" %in% type) db$upslope <- 0
  if("elev_diff" %in% type) db$elev_diff <- 0


  if(!is.na(w)) {
    for(cell in o){
      if(any(db[cell, type] == 0)){
        track <- na_omit(flow[cell, ])
        if("upslope" %in% type && db[cell, "upslope"] == 0) {
          db$upslope[track] <- upslope_values(track, db)
        }
        if("elev_diff" %in% type && db[cell, "elev_diff"] == 0) {
          db$elev_diff[track] <- elev_diff_values(track, db)
        }
      }
    }
  }
  db
}

upslope_values <- function(track, db){
  new <- 1:length(track)
  current <- db$upslope[track]
  new[current != 0] <- dplyr::last(new[current == 0])
  db$upslope[track] + new
}

elev_diff_values <- function(track, db) {
  new <- dplyr::lag(db$elev[track]) - db$elev[track]
  new <- new[-1]
  new <- cumsum(new)
  current <- db$elev_diff[track]
  current <- current[-length(current)]
  new[current != 0] <- dplyr::last(new[current == 0])
  db$elev_diff[track] + c(0, new)
}

# For each cell, calculate the flow track
get_all_flow <- function(db) {
  db$drec_shed[db$drec_shed == db$seqno_shed] <- NA
  m <- matrix(db$seqno_shed, ncol = 1)
  end <- FALSE
  t2 <- 0
  while(!end){
    m1 <- db$drec_shed[m[,ncol(m)]]
    m1[rowSums(m1 == m, na.rm = TRUE) > 0] <- NA
    m <- cbind(m, m1)
    if(all(is.na(m[,ncol(m)]))) end <- TRUE
  }
  return(m)
}
