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

get_intermediate <- function(dem, sheds) {
  dem$intermediate_shed <- NA
  for(i in sheds$shedno) {
   if(!(i %in% dem$local_shed)) {
     parents <- sheds$shedno[i == sheds$becomes]
     cells <- unique(c(dem$seqno[dem$local_shed %in% parents],
                       dem$seqno[dem$intermediate_shed %in% parents]))
     dem$intermediate_shed[cells] <- i
   }
  }
  return(dem)
}

get_sheds <- function(w, stats) {
  done <- FALSE
  d <- tibble::tibble(shedno = stats$shedno[stats$becomes == w],
                      becomes = 20)
  todo <- d$shedno[!(d$becomes %in% d$shedno)]
  while(!done) {
    for(i in todo) {
      if(nrow(stats[stats$becomes == i,]) > 0) {
        d <- dplyr::bind_rows(d, tibble::tibble(shedno = stats$shedno[stats$becomes == i], becomes = i))
      }
    }
    if(all(d$shedno[!(d$becomes %in% d$shedno)] == todo)) done <- TRUE else todo <- d$ShedNo[!(d$becomes %in% d$shedno)]
  }
  return(d[nrow(d):1, ])
}

plot_fill_single <- function(sheds, dem) {
  d <- dem %>%
    mutate(type = NA,
           type = case_when(local_shed == sheds$shedno[1] | intermediate_shed == sheds$shedno[1] ~ sheds$shedno[1],
                            local_shed == sheds$shedno[2] | intermediate_shed == sheds$shedno[2] ~ sheds$shedno[2]))

  labs <- d %>%
    dplyr::group_by(type) %>%
    dplyr::summarize(row = median(row), col = median(col))

  flow_plot(dem, type = "elevation") +
    theme(legend.position = "none") +
    geom_raster(data = d, aes(fill = type), alpha = 0.8) +
    geom_text(data = labs, aes(label = type)) +
    labs(subtitle = paste0(sheds$shedno[1], " + ", sheds$shedno[2], " = ", sheds$becomes[1]),
         title = sheds$becomes[1])
}

plot_fill <- function(w, dem, stats) {
  sheds <- get_sheds(w, stats)
  dem <- get_intermediate(dem, sheds)
  g <- list()
  for(b in unique(sheds$becomes)) {
    g[[length(g) + 1]] <- plot_fill_single(sheds[sheds$becomes == b,], dem)
  }
  gridExtra::arrangeGrob(grobs = g, ncol = 2)
}


