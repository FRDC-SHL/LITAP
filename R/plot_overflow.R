report_overflow <- function(file, n = "") {
  rmarkdown::render(system.file("reports", "plot_overflow.Rmd", package = "LITAP"),
                    params = list(file = file, n = n))
}

plot_overflow <- function(file, n = "") {

  pond <- load_new(file = paste0(file, "_pond.rds")) %>%
    dplyr::select(-out_row, -out_col, -in_row, -in_col) %>%
    dplyr::arrange(pit_elev, varatio)

  final_sheds <- pond %>%
    dplyr::filter(becomes == shedno | becomes == 0)

  db <- load_new(file = paste0(file, "_dem_pond.rds"))

  # Main plot
  g <- ggplot2::ggplot(data = db, ggplot2::aes(x = col, y = row)) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed()+
    ggplot2::geom_raster(aes(alpha = elev)) +
    ggplot2::scale_alpha_continuous(range = c(0, 1), guide = FALSE)

  if(n == "") {
    n <- nrow(final_sheds)
  } else if(n > nrow(final_sheds)) n <- nrow(final_sheds)

  for(i in 1:n) {

    w <- final_sheds[i,]

    all_sheds <- get_sheds(w = w$shedno, sheds = pond)


    db_sub <- db[db$pond_shed == w$shedno,]
    g_sub <- pond[pond$shedno %in% all_sheds,] %>%
      dplyr::arrange(becomes, shedno)
    db_temp <- db

    done <- FALSE
    ggs <- list(g +
                  ggplot2::geom_raster(data = db_temp[db_temp$pond_shed == g_sub$shedno[g_sub$becomes == 0],], alpha = 0.6, fill = "white") +
                  ggplot2::labs(title = paste0("Final Shed: ", g_sub$shedno[g_sub$becomes == 0])))
    finished <- vector()


    while(!done) {
      b <- g_sub$becomes[!(g_sub$shedno %in% finished) & g_sub$becomes != 0][1]
      s <- g_sub[g_sub$becomes == b,]
      d <- db_temp[db_temp$local_shed %in% s$shedno,]

      title <- paste0("Final Shed: ", s$shedno)

      ggs[length(ggs)+1] <- list(g +
                                   ggplot2::geom_raster(data = db_temp[db_temp$pond_shed == g_sub$shedno[g_sub$becomes == 0],], alpha = 0.6, fill = "white") +
                                   ggplot2::geom_raster(data = d, alpha = 0.5, fill = "blue") +
                                   ggplot2::geom_raster(data = d, ggplot2::aes(fill = factor(local_shed)), alpha = 0.5) +
                                   ggplot2::scale_fill_manual(name = "Watershed", values = c("grey", "black"), guide = FALSE) +
                                   ggplot2::geom_point(data = s, ggplot2::aes(x = pit_col, y = pit_row)) +
                                   ggplot2::geom_label(data = s, ggplot2::aes(x = pit_col, y = pit_row, label = shedno)) +
                                   ggplot2::labs(title = title))

      db_temp$local_shed[db_temp$local_shed %in% s$shedno] <- b

      finished <- c(finished, s$shedno)
      if(nrow(g_sub[!(g_sub$shedno %in% finished) & g_sub$becomes != 0,]) == 0) done <- TRUE
    }

    gridExtra::grid.arrange(grobs = ggs, ncol = 3)
  }
}

get_sheds <- function(w, sheds) {
  done <- FALSE
  d <- tibble::tibble()
  while(!done) {
    #message(paste0(w, collapse = ", "))
    d_new <- sheds[sheds$becomes %in% w, ]
    w <- unique(c(w, d_new$shedno))
    if(nrow(d_new) > nrow(d)) d <- d_new else done <- TRUE
  }
  return(w)
}


