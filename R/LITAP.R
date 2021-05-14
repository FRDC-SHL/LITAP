.onAttach <- function(libname, pkgname) {
  packageStartupMessage("LITAP v", utils::packageVersion("LITAP"), "\n",
                        "LITAP is still in development; Help us by submitting ",
                        "bugs/feature requests: \n",
                        "http://github.com/FRDC-SHL/LITAP/issues")
}


#' Dealing with CRAN Notes due to Non-standard evaluation
#' @keywords internal
.onLoad <- function(libname = find.package("LITAP"), pkgname = "LITAP"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
      c("centre", "col_f", "buffer", "buffer_n", "data", "becomes",
        "drec", "flatcell", "ldir", "ddir", "local_shed",
        "seqno", "adjacent", "elev", "elev_n", "ddir_n", "ldir_n", "seqno_n", "patch",
        "n_p", "n_shedno_", "next_pit", "out_col", "out_elev", "out_row", "out_seqno",
        "parea", "pit_area", "pit_col", "pit_elev", "pit_elev_out", "pit_row",
        "pit_seqno", "pit_seqno_out", "pit_vol", "pond_shed", "pour_elev",
        "pour_elev_out", "pp", "pre_vol", "ridge",
        "row_f", "row_out", "seqno_buffer", "seqno_next", "seqno_shed",
        "shed_area", "shedno", "stage", "total_cells", "upslope", "upslope_n",
        "upslope_new", "value", "varatio", "visited", "xend", "xloc", "yend", "yloc",
        "db_sub1", "db_sub2", "db_sub3", "db_sub4", "db_w", "dist", "dist_min",
        "drains_to", "drains_to_orig", "drec_buffer", "edge", "edge_map", "elev_diff", "end_pit",
        "fill_shed", "final", "group1", "group2", "group3", "group4", "highlight",
        "in_col", "in_elev", "in_row", "in_seqno", "initial_shed", "initial_shed_n", "last_elev",
        "ldir_opts", "ddir_opts", "max_slope", "median", "min_elev_n", "missing_n",
        "mm2fl", "mutate", "n", "n1", "n2", "n3", "n4", "n_elev", "n_seqno", "n_shedno", "shedno",
        'shedno_n', 'removed', "relief",
        "x", "y", "elev", "type", "col_out",
        # piping requires '.' at times
        ".")

    )
  invisible()
}
