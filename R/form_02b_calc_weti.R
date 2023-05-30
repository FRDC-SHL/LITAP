# FormMapR: Procedure to compute wetness index as per Quinn et all., 1991

calc_weti2 <- function(db, grid, verbose) {

  l1 <- grid * 0.5    # orthogonal
  l2 <- grid * 0.354  # diagonal  (hypothenus = sqrt(0.5^2 + 0.5^2) / 2)
  l_sqr <- grid * grid  # let's use cell area
  orthogonal <- grid
  diagonal <- grid * sqrt(2)
  qarea1 <- 1
  qarea2 <- l_sqr

  max_s <- nrow(db)

  if(verbose) message("  Setting up cell flow")

  # LandMapR neighbour arrangements:
  #
  # 1 2 3
  # 4 F 6
  # 7 8 9

  db_n <- db %>%
    dplyr::arrange(dplyr::desc(elev), upslope, seqno) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::filter(!buffer)

  # Doesn't look right! But does ensure that we can use seqno to index (top down)
  drec <- matrix(db$drec, ncol = max(db$row), nrow = max(db$col))
  ddir <- matrix(db$ddir, ncol = max(db$row), nrow = max(db$col))
  elev <- matrix(db$elev, ncol = max(db$row), nrow = max(db$col))
  seqno <- matrix(db$seqno, ncol = max(db$row), nrow = max(db$col))

  qa1 <- matrix(qarea1, nrow = max(db$col), ncol = max(db$row))
  qa2 <- matrix(qarea2, nrow = max(db$col), ncol = max(db$row))
  qw1 <- matrix(NA_real_, nrow = max(db$col), ncol = max(db$row))
  qw2 <- matrix(NA_real_, nrow = max(db$col), ncol = max(db$row))

  missing <- matrix(is.na(elev), nrow = max(db$col), ncol = max(db$row))

  n_rnds <- matrix(0, ncol = max(db$row), nrow = max(db$col))

  r <- db_n$row
  c <- db_n$col

  o <- c(1, 4, 7, 2, 5, 8, 3, 6, 9) # Convert LandMapR order to matrix indices
  d <- matrix(c(diagonal, orthogonal, diagonal,
                orthogonal, orthogonal, orthogonal,
                diagonal, orthogonal, diagonal),
              nrow = 3)

  l <- matrix(c(l2, l1, l2,
                l1, l1, l1,
                l2, l1, l2), nrow = 3)

  for(i in seq_along(db_n$elev)) {

#    if(60 %in% c(r[i]-1, r[i], r[i]+1) & 125 %in% c(c[i]-1, c[i], c[i]+1)) browser()

    # Note reversed col/rows (see above)
    f <- elev[c[i], r[i]]  # Get focal cell
    n <- elev[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i] + 1)] # Get neighbourhood matrix

    if(!is.na(f)) {
      tan <- ((f - n)/d) * l
      tan[n >= f] <- 0       #only where neighbours lower than focal
      sumtanbl <- sum(tan, na.rm = TRUE)

      if(sumtanbl != 0) {
        qc1 <- qa1[c[i], r[i]] / sumtanbl
        qc2 <- qa2[c[i], r[i]] / sumtanbl
        qa1[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i] + 1)] <-
          qa1[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i] + 1)] + (qc1 * tan) # new_aq = qc * tanb*ql
        qa2[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i]+1)] <-
          qa2[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i]+1)] + (qc2 * tan) # qc * tanb*ql
        n_rnds[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i]+1)] <-
          n_rnds[(c[i] - 1):(c[i] + 1), (r[i] - 1):(r[i]+1)] + 1

      } else if(ddir[c[i], r[i]] != 5) {
        qc1 <- qa1[c[i], r[i]] / 0.0001
        qa1[drec[c[i], r[i]]] <- qa1[drec[c[i], r[i]]] + qa1[c[i], r[i]]
        qa2[drec[c[i], r[i]]] <- qa2[drec[c[i], r[i]]] + qa2[c[i], r[i]]
        n_rnds[drec[c[i], r[i]]] <- n_rnds[drec[c[i], r[i]]] + 1
      }

      if(qc1 > 1) qw1[c[i], r[i]] <- log(qc1) else qw1[c[i], r[i]] <- log(1 + qc1)
      if(qc2 > 1) qw2[c[i], r[i]] <- log(qc2) else qw2[c[i], r[i]] <- log(1 + qc2)

    }
  }

  # Deal with missing
  qw1[is.na(qw1)] <- qa1[is.na(qw1)] / 0.0001
  qw2[is.na(qw1)] <- qa2[is.na(qw1)] / 0.0001

  # Missing always missing
  qw1[missing] <- NA
  qw2[missing] <- NA
  qa1[missing] <- NA
  qa2[missing] <- NA


  db$qarea1 <- trunc_dec(as.vector(qa1),2)
  db$qarea2 <- trunc_dec(as.vector(qa2),2)
  db$qweti1 <- trunc_dec(as.vector(qw1),2)
  db$qweti2 <- trunc_dec(as.vector(qw2),2)
  #db$n_rnds <- as.vector(n_rnds) # Only for debugging

  db
}

