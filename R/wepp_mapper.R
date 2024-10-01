#' Calculate spatial entities required for WEPP
#'
#' This function takes backup data frame output from [flow_mapper()] and
#' [form_mapper()] calculates hillslope, channel segments and impoundment
#' spatial entities required for input in to WEPP (Water Erosion and Prediction
#' Project) (among other metrics).  Based on FacetMapR by R. A. (Bob) MacMillan,
#' LandMapper Environmental Solutions.
#'
#' @param folder Character. Location of [flow_mapper()] output
#' @param chan_length Numeric. Channel length maximum length. Used to split
#'   channels into segments
#' @param upslope_threshold Numeric. Threshold of upslope cells to define
#'   channel cells. #' Cells with an upslope value larger than this are
#'   considered channel cells
#'
#' @inheritParams args
#'
#' @examples
#'
#' \dontrun{
#' # First need to run flow_mapper()
#' flow_mapper(file = system.file("extdata", "testELEV.dbf", package = "LITAP"),
#'            out_folder = "./testELEV/", nrow = 90, ncol = 90, grid = 5)
#'
#' # Now can run wepp_mapper()
#' wepp_mapper(folder = "./testELEV/")
#' }
#'
#' @export

wepp_mapper <- function(folder,
                        chan_length = 200,
                        upslope_threshold = 300,
                        clean = FALSE,
                        resume = NULL,
                        log = TRUE,
                        verbose = FALSE, quiet = FALSE, debug = FALSE) {

  # Checks
  check_folder(folder)

  # Setup -------------------------------------------------------------------

  # Get resume options
  if(is.null(resume)) resume <- ""
  resume_options <- c("", "mark_chan", "cut_chan", "merge_chan", "new_ups",
                      "remark_chan", "mark_pits", "split_segments", "flow2_chan",
                      "calc_segs", "order_segs", "redo_ddir",
                      "find_upsegs", "hill_sheds", "renum_segs", "build_stru",
                      "wepp_form", "wepp_len", "hill_stats", "chan_stats",
                      "new_ups_final")

  check_resume(resume, resume_options)

  # Get out format
  out_format <- get_format(folder, where = "flow")

  # Get backup fill dem

  db <- get_previous(folder, where = "flow", step = "fill")

  # FOR TESTING ------------------
  # db <- foreign::read.dbf("../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/021dem.dbf") %>%
  #   janitor::clean_names() %>%
  #   dplyr::rename(seqno = seq_no, upslope = up_slope, fill_shed = shed_now) %>%
  #   dplyr::mutate(x = col, y = row)

  if("ldir" %in% names(db)) db <- dplyr::rename(db, "ddir" = "ldir")
  db <- dplyr::select(db, "seqno", "x", "y", "row", "col", "elev", "drec",
                      "ddir", "upslope", "fill_shed") %>%
    add_buffer()

  grid <- calc_grid(db)
  check_grid(grid)

  # Get fill file
  fill <- get_previous(folder, where = "flow", step = "fill", type = "stat") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("row|col")), ~ . + 1) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("seqno")),
                     ~ seqno_to_buffer(., db$seqno[!db$buffer]))

  # FOR TESTING ------------------
  # fill <- foreign::read.dbf("../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/021pit.dbf") %>%
  #   janitor::clean_names() %>%
  #   dplyr::rename(shedno = shed_no) %>%
  #   dplyr::rename_with(\(x) stringr::str_replace(x, "rec", "seqno")) %>%
  #   dplyr::mutate_at(dplyr::vars(dplyr::matches("row|col")), ~ . + 1) %>%
  #   dplyr::mutate_at(dplyr::vars(dplyr::contains("seqno")),
  #                    ~ seqno_to_buffer(., db$seqno[!db$buffer])) %>%

  # Get out locs
  out_locs <- locs_create(folder, which = "wepp", clean = clean)

  # Messaging
  if(quiet) verbose <- FALSE

  # Setup Log
  log_file <- log_setup(folder, which = "wepp", log)

  start <- Sys.time()

  # File details to log
  log_write("Run options:\n", log = log_file)
  log_write("  Input folder = ", normalizePath(folder), "\n",
            "  grid =  ", grid, "\n",
            "  chan_length = ", chan_length, "\n",
            "  upslope_threshold = ", upslope_threshold, "\n",
            log = log_file)

  # Run start to log
  log_write("\nRun started: ", start, "\n", log = log_file)



  # Mark Channels -----------------------------------------------------------

  task <- "Marking channels and channel junction"
  if(resume == "" || resume == "mark_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    db_marked <- mark_chan(db, upslope_threshold = upslope_threshold)
    save_output(data = db_marked, name = "marked", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Cut Channels ------------------------------------------------------------
  task <- "Cut channels fix elevation"
  if(resume == "" || resume == "cut_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_marked")) {
      db_marked <- get_previous(folder, where = "wepp", step = "marked") %>%
        add_buffer()
    }

    db_cut <- cut_chan(db_marked, upslope_threshold)
    save_output(data = db_cut, name = "cut", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Merge Channels -----------------------------------------------------------

  task <- "Merge/thin adjacent channels"
  if(resume == "" || resume == "merge_chan"){
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_cut")) {
      db_cut <- get_previous(folder, where = "wepp", step = "cut") %>%
        add_buffer()
    }

    db_merged <- merge_chan(db_cut)
    save_output(data = db_merged, name = "merged", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Recalculate Upslopes ----------------------------------------------------
  task <- "Re-calculate upslopes"
  if(resume == "" || resume == "new_ups") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_merged")) {
      db_merged <- get_previous(folder, where = "wepp", step = "merged") %>%
        add_buffer()
    }

    db_ups <- dplyr::mutate(db_merged, shedno = fill_shed) %>%
      calc_upslopes()
    save_output(data = db_ups, name = "ups", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Re-mark Upslopes ----------------------------------------------------
  task <- "Re-mark Channels"
  if(resume == "" || resume == "remark_chan") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_ups")) {
      db_ups <- get_previous(folder, where = "wepp", step = "ups") %>%
        add_buffer()
    }

    db_remarked <- remark_chan(db_ups, upslope_threshold = upslope_threshold)
    save_output(data = db_remarked, name = "remarked", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Mark Pits ----------------------------------------------------
  task <- "Mark Pits"
  if(resume == "" || resume == "mark_pits") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_remarked")) {
      db_remarked <- get_previous(folder, where = "wepp", step = "remarked") %>%
        add_buffer()
    }

    db_pits <- mark_pits(db_remarked, fill)
    save_output(data = db_pits, name = "pits", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Split Segments ----------------------------------------------------
  task <- "Split Segments"
  if(resume == "" || resume == "split_segments") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_pits")) {
      db_pits <- get_previous(folder, where = "wepp", step = "pits") %>%
        add_buffer()
    }

    db_split <- split_segments(db_pits, grid, chan_length)
    save_output(data = db_split, name = "split", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Flow to channels ----------------------------------------------------
  task <- "Flow to channels"
  if(resume == "" || resume == "flow2_chan") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_split")) {
      db_split <- get_previous(folder, where = "wepp", step = "split") %>%
        add_buffer()
    }

    db_flow <- flow_to_channels(db_split)
    save_output(data = db_flow, name = "flow", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Calculate Segments ----------------------------------------------------
  task <- "Calculate segments"
  if(resume == "" || resume == "calc_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_flow")) {
      db_flow <- get_previous(folder, where = "wepp", step = "flow") %>%
        add_buffer()
    }

    db_segs <- calc_segments(db_flow, grid = grid)
    segs <- db_segs$segs
    db_segs <- db_segs$db
    save_output(data = db_segs, stats = segs, name = "first_segs", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    save_output(data = db_segs, name = "first_segs", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Order Segments ----------------------------------------------------
  task <- "Order segments"
  if(resume == "" || resume == "order_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_segs") | !exists("segs")) {
      db_segs <- get_previous(folder, where = "wepp", step = "first_segs") %>%
        add_buffer()

      segs <- get_previous(folder, where = "wepp", step = "first_segs", type = "stats") %>%
        add_buffer(db = db_segs, stats = .)
    }

    db_ordered <- order_segments(db_segs, segs)
    segs_ordered <- db_ordered$segs
    db_ordered <- db_ordered$db
    save_output(data = db_ordered, stats = segs_ordered, name = "ordered",
                locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    save_output(data = db_ordered, name = "ordered", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)


  # Redo ddir ----------------------------------------------------
  task <- "Redo and Add ddir"
  if(resume == "" || resume == "redo_ddir") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_ordered") | !exists("segs_ordered")) {
      db_ordered <- get_previous(folder, where = "wepp", step = "ordered") %>%
        add_buffer()

      segs_ordered <- get_previous(folder, where = "wepp", step = "ordered", type = "stats") %>%
        add_buffer(db = db_ordered, stats = .)
    }

    db_ddir <- redo_ddir(db_ordered, segs_ordered)
    segs_ddir <- db_ddir$segs
    db_ddir <- db_ddir$db

    save_output(data = db_ddir, stats = segs_ddir, name = "ddir2", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    save_output(data = db_ddir, name = "ddir2", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Find upsegs ----------------------------------------------------
  task <- "Find upslope segments"
  if(resume == "" || resume == "find_upsegs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("segs_ordered")) {
      db_ddir <- get_previous(folder, where = "wepp", step = "ddir2") %>%
        add_buffer()
      segs_ddir <- get_previous(folder, where = "wepp", step = "ddir2",
                                type = "stats") %>%
        add_buffer(db_ddir, stats = .)
    }

    segs_upsegs <- find_upsegs(segs_ddir)

    save_output(data = db_ddir, stats = segs_upsegs, name = "upsegs", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Hillslope segs ----------------------------------------------------
  task <- "Compute and label hillslope segments"
  if(resume == "" || resume == "hill_sheds") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_ddir")) {
      db_ddir <- get_previous(folder, where = "wepp", step = "ddir2") %>%
        add_buffer()
    }

    db_hillsheds <- hill_sheds(db_ddir)

    save_output(data = db_hillsheds, name = "hillsheds", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Renumber segments ----------------------------------------------------
  task <- "Renumber segments"
  if(resume == "" || resume == "renum_segs") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_hillsheds") | !exists("segs_upsegs")) {
      db_hillsheds <- get_previous(folder, where = "wepp", step = "hillsheds") %>%
        add_buffer()
      segs_upsegs <- get_previous(folder, where = "wepp", step = "upsegs", type = "stats") %>%
        add_buffer(db_hillsheds, stats = .)
    }

    db_renum <- renum_segs(db_hillsheds, segs_upsegs)
    segs_renum <- db_renum$segs
    db_renum <- db_renum$db

    save_output(data = db_renum, stats = segs_renum, name = "renum", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)
    save_output(data = db_renum, name = "renum", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Build WEPP structure file ----------------------------------------------------
  task <- "Build WEPP structure file"
  if(resume == "" || resume == "build_stru") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_renum") | !exists("segs_renum")) {
      db_renum <- get_previous(folder, where = "wepp", step = "renum") %>%
        add_buffer()
      segs_renum <- get_previous(folder, where = "wepp", step = "renum",
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
    save_basic(data = remove_buffer(db_renum, segs_struct),
               name = "segs", locs = out_locs,
               out_format = out_format, where = "wepp")

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Compute aspect and slope for WEPP -----------------------------------------
  task <- "Compute aspect and slope for WEPP"
  if(resume == "" || resume == "wepp_form") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_renum")) {
      db_renum <- get_previous(folder, where = "wepp", step = "renum") %>%
        add_buffer()
    }

    db_wepp_form <- wepp_form(db_renum, grid)

    save_output(data = db_wepp_form, name = "wepp_form", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Measures of channel length ------------------------------------------------
  task <- "Calculate 3 measures of channel length"
  if(resume == "" || resume == "wepp_len") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_wepp_form")) {
      db_wepp_form <- get_previous(folder, where = "wepp", step = "wepp_form") %>%
        add_buffer()
    }

    db_wepp_len <- wepp_len(db_wepp_form, grid)

    save_output(data = db_wepp_len, name = "wepp", locs = out_locs,
                out_format = out_format, where = "wepp", debug = debug)

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Hill Stats (profiles) -------------------------------------------------------------------
  task <- "Calculate hill profiles"
  if(resume == "" || resume == "hill_stats") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_wepp_len") | !exists("segs_struct")) {
      db_wepp_len <- get_previous(folder, where = "wepp", step = "wepp") %>%
        add_buffer()
      segs_struct <- get_previous(folder, where = "wepp", step = "stats_segs",
                                  type = "stats") %>%
        add_buffer(db_wepp_len, stats = .)
    }

    db_hill_stats <- hill_stats(db_wepp_len, segs_struct, grid)

    save_basic(data = db_hill_stats$hill, name = "hill", locs = out_locs,
               out_format = out_format, where = "wepp")
    save_basic(data = db_hill_stats$prof, name = "prof", locs = out_locs,
               out_format = out_format, where = "wepp")

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Chan Stats -------------------------------------------------------------------
  task <- "Calculate channel stats"
  if(resume == "" || resume == "chan_stats") {
    announce(task, quiet)
    sub_start <- Sys.time()
    log_start(task, sub_start, log_file)

    if(!exists("db_wepp_len") | !exists("segs_struct")) {
      db_wepp_len <- get_previous(folder, where = "wepp", step = "wepp_len") %>%
        add_buffer()
      segs_struct <- get_previous(folder, where = "wepp", step = "struct",
                                  type = "stats") %>%
        add_buffer(db_wepp_len, stats = .)
    }

    db_chan_stats <- chan_stats(db_wepp_len, segs_struct, grid)

    save_basic(data = db_chan_stats, name = "chan", locs = out_locs,
               out_format = out_format, where = "wepp")

    log_time(sub_start, log_file)
    resume <- ""
  } else skip_task(task, log_file, quiet)

  # Clean up
  if(!debug) remove_output(locs = out_locs, out_format = out_format,
                           where = "wepp")

  # Save final time
  run_time(start, log_file, quiet)
}
