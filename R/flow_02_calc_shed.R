join_slopes <- function(u1, u2) {
  na_omit(c(u1, u2))
}

get_track <- function(shed_list, track) {
  t <- sapply(lapply(shed_list, FUN = function(x, track) x[track %in% x], track), length)
  as.numeric(names(t[t > 0]))
}

calc_shed4 <- function(db, verbose) {

  db_orig <- db

  npits <- sum(db$ddir == 5, na.rm = TRUE)

  db <- db %>%
    dplyr::mutate(
      shedno = NA_integer_,
      shedno = replace(shedno, ddir == 5 & !is.na(ddir), 1:!!npits)) %>%
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

  # Merge with original data
  db <- dplyr::left_join(db, db_orig, by = c("seqno", "elev", "drec"))

  # Calculate ridge lines
  db %>%
    dplyr::select(seqno, shedno, buffer) %>%
    nb_values(max_cols = max(db$col), col = "shedno") %>%
    dplyr::filter(buffer == FALSE) %>%
    dplyr::group_by(seqno) %>%
    # Ridges are cells that meet another watershed
    dplyr::summarize(ridge = length(unique(shedno_n[!is.na(shedno_n)])) > 1) %>%
    dplyr::right_join(db, by = "seqno") %>%
    dplyr::arrange(.data$seqno) %>%
    dplyr::rename("initial_shed" = "shedno")
}

calc_upslopes <- function(db, type = c("upslope", "uced")) {
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
                                     ~get_upslope3(.x, .y[1],
                                                   type = !!type))) %>%
    tidyr::unnest(db_w) %>%
    dplyr::arrange(seqno)
}

get_upslope3 <- function(db, w, type = c("upslope", "uced")){
  o <- db$seqno_shed
  db <- dplyr::arrange(db, seqno_shed)
  flow <- get_all_flow(db)

  if("upslope" %in% type) db$upslope <- 0
  if("uced" %in% type) db$uced <- NA_real_


  if(!is.na(w)) {
    for(cell in o){
      if(any(is.na(db[cell, type]) | db[cell, type] == 0)){
        track <- na_omit(flow[cell, ])
        if("upslope" %in% type && db[cell, "upslope"] == 0) {
          db$upslope[track] <- upslope_values(track, db)
        }
        if("uced" %in% type && is.na(db[cell, "uced"])) {
          db$uced[track] <- uced_values(track, db)
        }
      }
    }
  }

  if("uced" %in% type) {
    db <- dplyr::mutate(db, uced = (.data$uced))
  }

  db
}

upslope_values <- function(track, db){
  new <- 1:length(track)
  current <- db$upslope[track]
  new[current != 0] <- dplyr::last(new[current == 0])
  db$upslope[track] + new
}

uced_values <- function(track, db) {

  elev <- db$elev[track]
  current <- db$uced[track]
  new <- rep(0, length(track))

  ii <- 2:length(track)
  jj <- which(is.na(current))
  for(i in ii) new[i] <- new[i] + sum(elev[jj[jj < i]] - elev[i])

  current[is.na(current)] <- 0
  current + new
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
  m
}
