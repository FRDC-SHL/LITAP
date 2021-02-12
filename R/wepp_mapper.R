#' Calculate spatial entities required for WEPP
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates hillslope, channel segments and impoundment
#' spatial entities required for input in to WEPP (Water Erosion and Prediction
#' Project) (among other metrics).  Based on FacetMapR by R. A. (Bob) MacMillan,
#' LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param grid Numeric. Grid size for the original dem
#' @param chan_length Numeric. Channel length maximum length. Used to split
#'   channels into segments
#' @param upslope_threshold Numeric. Threshold of upslope cells to define
#'   channel cells. #' Cells with an upslope value larger than this are
#'   considered channel cells
#'
#' @inheritParams args
#'
#' @examples
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 150, ncol = 150)
#'
#' # Now can run wepp_mapper()
#' wepp_mapper(folder = "./testELEV/", grid = 5)
#'
#' @export

wepp_mapper <- function(folder, grid,
                        chan_length = 200,
                        upslope_threshold = 300,
                        out_format = "rds",
                        clean = FALSE,
                        resume = NULL, end = NULL,
                        log = TRUE,
                        verbose = FALSE, quiet = FALSE) {

  # Setup -------------------------------------------------------------------

  # Get resume options
  if(is.null(resume)) resume <- ""
  if(is.null(end)) end <- ""
  resume_options <- c("", "mark_chan", "cut_chan", "merge_chan", "new_ups",
                      "remark_chan", "mark_pits", "split_segments", "flow2_chan",
                      "calc_segs", "order_segs", "redo_ddir",
                      "find_upsegs", "hill_sheds", "renum_segs", "build_stru",
                      "wepp_form", "wepp_len", "hill_stats", "chan_stats",
                      "new_ups_final")

  check_resume(resume, end, resume_options)

  # Get backup fill dem
  db <- get_previous(folder, step = "fill", where = "flow")
  if("ldir" %in% names(db)) db <- dplyr::rename(db, "ddir" = "ldir")
  db <- dplyr::select(db, seqno, row, col, elev, drec, ddir, upslope, fill_shed) %>%
    add_buffer()

  # Get fill file
  fill <- get_previous(folder, step = "fill", where = "flow", type = "stat") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("row|col")), ~ . + 1) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("seqno")),
                     ~ seqno_to_buffer(., db$seqno[!db$buffer]))

  # Get out locs
  out_locs <- locs_create(folder, which = "wepp", clean = clean)

  # Messaging
  if(quiet) verbose <- FALSE


  # Setup Log
  if(log) {
    log_file <- file.path(folder, paste0(basename(folder), "_wepp.log"))
    unlink(list.files(folder, "wepp.log", full.names = TRUE))
  } else log_file <- FALSE

  start <- Sys.time()

  # File details to log
  write_log("Run options:\n", log = log_file)
  write_log("  Input folder = ", normalizePath(folder), "\n",
            "  grid =  ", grid, "\n",
            "  chan_length = ", chan_length, "\n",
            "  upslope_threshold = ", upslope_threshold, "\n",
            log = log_file)

  # Run start to log
  write_log("\nRun started: ", start, "\n", log = log_file)



  # Mark Channels -----------------------------------------------------------

  task <- "Marking channels and channel junction"
  if(resume == "" || resume == "mark_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    db_marked <- mark_chan(db, upslope_threshold = upslope_threshold)
    save_output2(data = db_marked, name = "marked", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "mark_chan") {
    run_time(start, log_file, quiet)
    return()
  }


  # Cut Channels ------------------------------------------------------------
  task <- "Cut channels fix elevation"
  if(resume == "" || resume == "cut_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_marked")) {
      db_marked <- get_previous(folder, step = "marked", where = "wepp") %>%
        add_buffer()
    }

    db_cut <- cut_chan(db_marked, upslope_threshold)
    save_output2(data = db_cut, name = "cut", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "cut_chan") {
    run_time(start, log_file, quiet)
    return()
  }



  # Merge Channels -----------------------------------------------------------

  task <- "Merge/thin adjacent channels"
  if(resume == "" || resume == "merge_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_cut")) {
      db_cut <- get_previous(folder, step = "cut", where = "wepp") %>%
        add_buffer()
    }

    db_merged <- merge_chan(db_cut)
    save_output2(data = db_merged, name = "merged", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "merge_chan") {
    run_time(start, log_file, quiet)
    return()
  }


  # Recalculate Upslopes ----------------------------------------------------
  task <- "Re-calculate upslopes"
  if(resume == "" || resume == "new_ups") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_merged")) {
      db_merged <- get_previous(folder, step = "merged", where = "wepp") %>%
        add_buffer()
    }

    db_ups <- dplyr::mutate(db_merged, shedno = fill_shed) %>%
      calc_upslopes()
    save_output2(data = db_ups, name = "ups", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "new_ups") {
    run_time(start, log_file, quiet)
    return()
  }

  # Re-mark Upslopes ----------------------------------------------------
  task <- "Re-mark Channels"
  if(resume == "" || resume == "remark_chan") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_ups")) {
      db_ups <- get_previous(folder, step = "ups", where = "wepp") %>%
        add_buffer()
    }

    db_remarked <- remark_chan(db_ups, upslope_threshold = upslope_threshold)
    save_output2(data = db_remarked, name = "remarked", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "remark_chan") {
    run_time(start, log_file, quiet)
    return()
  }

  # Mark Pits ----------------------------------------------------
  task <- "Mark Pits"
  if(resume == "" || resume == "mark_pits") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_remarked")) {
      db_remarked <- get_previous(folder, step = "remarked", where = "wepp") %>%
        add_buffer()
    }

    db_pits <- mark_pits(db_remarked, fill)
    save_output2(data = db_pits, name = "pits", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "mark_pits") {
    run_time(start, log_file, quiet)
    return()
  }

  # Split Segments ----------------------------------------------------
  task <- "Split Segments"
  if(resume == "" || resume == "split_segments") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_pits")) {
      db_pits <- get_previous(folder, step = "pits", where = "wepp") %>%
        add_buffer()
    }

    db_split <- split_segments(db_pits, grid, chan_length)
    save_output2(data = db_split, name = "split", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "split_segments") {
    run_time(start, log_file, quiet)
    return()
  }


  # Flow to channels ----------------------------------------------------
  task <- "Flow to channels"
  if(resume == "" || resume == "flow2_chan") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_split")) {
      db_split <- get_previous(folder, step = "split", where = "wepp") %>%
        add_buffer()
    }

    db_flow <- flow_to_channels(db_split)
    save_output2(data = db_flow, name = "flow", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "flow2_chan") {
    run_time(start, log_file, quiet)
    return()
  }

  # Calculate Segments ----------------------------------------------------
  task <- "Calculate segments"
  if(resume == "" || resume == "calc_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_flow")) {
      db_flow <- get_previous(folder, step = "flow", where = "wepp") %>%
        add_buffer()
    }

    db_segs <- calc_segments(db_flow, grid = grid)
    segs <- db_segs$segs
    db_segs <- db_segs$db
    save_output2(data = db_segs, stats = segs, name = "first_segs", locs = out_locs,
                 out_format = out_format, where = "wepp")
    save_output2(data = db_segs, name = "first_segs", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "calc_segs") {
    run_time(start, log_file, quiet)
    return()
  }

  # Order Segments ----------------------------------------------------
  task <- "Order segments"
  if(resume == "" || resume == "order_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_segs") | !exists("segs")) {
      db_segs <- get_previous(folder, step = "first_segs", where = "wepp") %>%
        add_buffer()

      segs <- get_previous(folder, step = "first_segs", where = "wepp", type = "stats") %>%
        add_buffer(db = db_segs, stats = .)
    }

    db_ordered <- order_segments(db_segs, segs)
    segs_ordered <- db_ordered$segs
    db_ordered <- db_ordered$db
    save_output2(data = db_ordered, stats = segs_ordered, name = "ordered",
                 locs = out_locs,
                 out_format = out_format, where = "wepp")
    save_output2(data = db_ordered, name = "ordered", locs = out_locs,
                 out_format = out_format, where = "wepp")
    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "order_segs") {
    run_time(start, log_file, quiet)
    return()
  }


  # Redo ddir ----------------------------------------------------
  task <- "Redo and Add ddir"
  if(resume == "" || resume == "redo_ddir") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_ordered") | !exists("segs_ordered")) {
      db_ordered <- get_previous(folder, step = "ordered", where = "wepp") %>%
        add_buffer()

      segs_ordered <- get_previous(folder, step = "ordered", where = "wepp", type = "stats") %>%
        add_buffer(db = db_ordered, stats = .)
    }

    db_ddir <- redo_ddir(db_ordered, segs_ordered)
    segs_ddir <- db_ddir$segs
    db_ddir <- db_ddir$db

    save_output2(data = db_ddir, stats = segs_ddir, name = "ddir2", locs = out_locs,
                 out_format = out_format, where = "wepp")
    save_output2(data = db_ddir, name = "ddir2", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "redo_ddir") {
    run_time(start, log_file, quiet)
    return()
  }

  # Find upsegs ----------------------------------------------------
  task <- "Find upslope segments"
  if(resume == "" || resume == "find_upsegs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("segs_ordered")) {
      db_ddir <- get_previous(folder, step = "ddir2", where = "wepp") %>%
        add_buffer()
      segs_ddir <- get_previous(folder, step = "ddir2", where = "wepp",
                                type = "stats") %>%
        add_buffer(db_ddir, stats = .)
    }

    segs_upsegs <- find_upsegs(segs_ddir)

    save_output2(data = db_ddir, stats = segs_upsegs, name = "upsegs", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "find_upsegs") {
    run_time(start, log_file, quiet)
    return()
  }


  # Hillslope segs ----------------------------------------------------
  task <- "Compute and label hillslope segments"
  if(resume == "" || resume == "hill_sheds") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_ddir")) {
      db_ddir <- get_previous(folder, step = "ddir2", where = "wepp") %>%
        add_buffer()
    }

    db_hillsheds <- hill_sheds(db_ddir)

    save_output2(data = db_hillsheds, name = "hillsheds", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "hill_sheds") {
    run_time(start, log_file, quiet)
    return()
  }


  # Renumber segments ----------------------------------------------------
  task <- "Renumber segments"
  if(resume == "" || resume == "renum_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_hillsheds") | !exists("segs_upsegs")) {
      db_hillsheds <- get_previous(folder, step = "hillsheds", where = "wepp") %>%
        add_buffer()
      segs_upsegs <- get_previous(folder, step = "upsegs", where = "wepp", type = "stats") %>%
        add_buffer(db_hillsheds, stats = .)
    }

    db_renum <- renum_segs(db_hillsheds, segs_upsegs)
    segs_renum <- db_renum$segs
    db_renum <- db_renum$db

    save_output2(data = db_renum, stats = segs_renum, name = "renum", locs = out_locs,
                 out_format = out_format, where = "wepp")
    save_output2(data = db_renum, name = "renum", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "renum_segs") {
    run_time(start, log_file, quiet)
    return()
  }


  # Build WEPP structure file ----------------------------------------------------
  task <- "Build WEPP structure file"
  if(resume == "" || resume == "build_stru") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_renum") | !exists("segs_renum")) {
      db_renum <- get_previous(folder, step = "renum", where = "wepp") %>%
        add_buffer()
      segs_renum <- get_previous(folder, step = "renum", where = "wepp",
                                 type = "stats") %>%
        add_buffer(db_renum, stats = .)
    }

    struct <- build_stru(db_renum, segs_renum)
    segs_struct <- struct$segs %>%
      dplyr::mutate(flow2crow = 0, chan_shape = 0, width_m = 0) %>%
      dplyr::select(-"upside", -"not_equal", -"num_down")
    struct <- struct$struct

    save_basic(data = struct, name = "struct", locs = out_locs,
               out_format = out_format, where = "wepp")
    save_output2(data = db_renum, stats = segs_struct, name = "segs",
                 locs = out_locs, out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "build_stru") {
    run_time(start, log_file, quiet)
    return()
  }
  wepp_form


  # Compute aspect and slope for WEPP -----------------------------------------
  task <- "Compute aspect and slope for WEPP"
  if(resume == "" || resume == "wepp_form") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_renum")) {
      db_renum <- get_previous(folder, step = "renum", where = "wepp") %>%
        add_buffer()
    }

    db_wepp_form <- wepp_form(db_renum, grid)

    save_output2(data = db_wepp_form, name = "wepp_form", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "wepp_form") {
    run_time(start, log_file, quiet)
    return()
  }


  # Measures of channel length ------------------------------------------------
  task <- "Calcualte 3 measures of channel length"
  if(resume == "" || resume == "wepp_len") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_wepp_form")) {
      db_wepp_form <- get_previous(folder, step = "wepp_form", where = "wepp") %>%
        add_buffer()
    }

    db_wepp_len <- wepp_len(db_wepp_form, grid)

    save_output2(data = db_wepp_len, name = "wepp_len", locs = out_locs,
                 out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "wepp_len") {
    run_time(start, log_file, quiet)
    return()
  }

  # Hill Stats (profiles) -------------------------------------------------------------------
  task <- "Calculate hill profiles"
  if(resume == "" || resume == "hill_stats") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_wepp_len") | !exists("segs_struct")) {
      db_wepp_len <- get_previous(folder, step = "wepp_len", where = "wepp") %>%
        add_buffer()
      segs_struct <- get_previous(folder, step = "stats_segs", where = "wepp",
                                  type = "stats") %>%
        add_buffer(db_wepp_len, stats = .)
    }

    db_hill_stats <- hill_stats(db_wepp_len, segs_struct, grid)

    save_basic(data = db_hill_stats$hill, name = "hill", locs = out_locs,
               out_format = out_format, where = "wepp")
    save_basic(data = db_hill_stats$prof, name = "prof", locs = out_locs,
               out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "hill_stats") {
    run_time(start, log_file, quiet)
    return()
  }


  # Chan Stats -------------------------------------------------------------------
  task <- "Calculate channel stats"
  if(resume == "" || resume == "chan_stats") {
    announce(task, quiet)
    sub_start <- Sys.time()
    write_start(task, sub_start, log_file)

    if(!exists("db_wepp_len") | !exists("segs_struct")) {
      db_wepp_len <- get_previous(folder, step = "wepp_len", where = "wepp") %>%
        add_buffer()
      segs_struct <- get_previous(folder, step = "struct", where = "wepp",
                                  type = "stats") %>%
        add_buffer(db_wepp_len, stats = .)
    }

    db_chan_stats <- chan_stats(db_wepp_len, segs_struct, grid)

    save_basic(data = db_chan_stats, name = "chan", locs = out_locs,
               out_format = out_format, where = "wepp")

    write_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)
  if(end == "chan_stats") {
    run_time(start, log_file, quiet)
    return()
  }

  # Save final time
  run_time(start, log_file, quiet)
}
