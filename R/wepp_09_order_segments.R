#' Order Segments by topological flow
#'
#' From original FoxPro code:
#'
#' "Procedure to compute the topological flow order of channel segments
#' computed by by the WeppMapR(c) program and to then go to the master
#' WEPPfile database and label each grid cell along a numbered segment
#' with the computed segment number"
#'
#' @noRd

order_segments <- function(db, segs) {

  down_cases <- function(start_type, end_type, end_seqno, sort_order, impound,
                         segs, db) {
    if(start_type != 5 & end_type == 5) {
      drain_rec <- end_seqno
      down_seg <- segs$sort_order[segs$end_seqno == end_seqno &
                                    segs$start_type == 5 & !segs$impound]
    } else if (start_type == 5 & end_type == 5) {
      if(!impound){
        drain_rec <- end_seqno
        if(nrow(i <- segs[segs$end_seqno == end_seqno & segs$impound, ]) > 0) {
          down_seg <- i$sort_order
        } else down_seg <- sort_order
      } else {
        drain_rec <- 0L # Need to check db for downstream flow
        down_seg <- 0L
      }
    } else {
      drain_rec <- db$seqno[db$drec[end_seqno]]
      down_seg <- db$segment_no[db$drec[end_seqno]]
    }

    data.frame(drain_rec = drain_rec, down_seg = down_seg)
  }

  segs2 <- segs %>%
    dplyr::mutate(d = purrr::pmap(list(start_type, end_type, end_seqno,
                                       sort_order, impound),
                                  down_cases, segs = !!segs, db = !!db)) %>%
    tidyr::unnest(cols = "d")

  # Now deal with impoundments
  segs3 <- dplyr::filter(segs2, down_seg == 0)

  # Go down to sub cell just in case (if no drain cell, will just circle around)
  down <- db[segs3$end_seqno,]
  down <- db[down$drec,]

  segs3$down_seg <- down$segment_no
  segs3$drain_seqno <- down$seqno

  segs <- dplyr::filter(segs2, down_seg != 0) %>%
    dplyr::bind_rows(segs3) %>%
    dplyr::arrange(sort_order) %>%
    dplyr::select(sort_order, down_seg, dplyr::everything())

  list(db = db, segs = segs)
}
