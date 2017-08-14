
sub_shed <- function(stats, final_shed){
  end <- FALSE
  w <- final_shed
  i <- 1
  sheds <- dplyr::filter(stats, shedno == w) %>%
    dplyr::mutate(stage = 0)

  while(!end){
    s <- dplyr::filter(stats, becomes %in% w) %>%
      dplyr::mutate(stage = i)
    if(nrow(s) > 0) {
      w <- unique(s$shedno)
      sheds <- dplyr::bind_rows(sheds, s)
      i <- i + 1
    } else end <- TRUE
  }
  return(sheds %>% dplyr::arrange(desc(stage), ))
}

plot_pond <- function(file) {

  origs <- load_orig(file = "../TestFiles/11_Ab02PV/ZeroThreshold_FlowMapR/02zpond.dbf") %>% arrange(pit_elev)
  news <- load_new(file = "../Runs/02z/02z_w_stats_global_shed.rds") %>% distinct() %>% arrange(pit_elev)
  new_global <- load_new(file = "../Runs/02z/02z_db_global_shed.rds")


  origs %>%
    arrange(becomes, pit_elev, pit_area) %>%
    select(shedno, pit_elev, shed_area, out_shed, becomes, final)

  final_sheds <- origs$shedno[origs$final == TRUE]

  temp <- sub_shed(stats = origs, final_shed = 36)

  db <- new_global
  db <- db %>%
    mutate(highlight = dplyr::if_else(local_shed %in% temp$shedno[temp$stage == 2], TRUE, FALSE))

  dbs1 <- dplyr::filter(db, highlight == TRUE, local_shed == temp$shedno[temp$stage == 2][1])
  dbs2 <- dplyr::filter(db, highlight == TRUE, local_shed == temp$shedno[temp$stage == 2][2])



  db$relief <- as.vector(r)

  # Main plot
  g <- ggplot(data = db, aes(x = col, y = row, frame = local_shed, cumulative = TRUE)) +
    scale_y_reverse() +
    coord_fixed() +
    #geom_raster(aes(fill = elev)) +
    geom_raster(aes(alpha = relief)) +
    scale_alpha_continuous(range = c(1, 0), guide = FALSE) +
    geom_raster(aes(fill = factor(local_shed)), alpha = 0.25) +

    #scale_fill_gradient(low = "red", high = "green") +
    #geom_raster()

  gganimate(g)

}
