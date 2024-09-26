expect_silent({
  t <- test_files()
})

test_that("seg-cal", {
  le <- dplyr::select(t$facet, "row", "col", "seqno", "max_facet") |>
    omit_edges(edge_row = t$edge_row, edge_col = t$edge_col,
               nrow = max(t$facet$row), ncol = max(t$facet$col))

  allpnts <- all_points("testing") |> suppressMessages()
  # Seg-Cal ---- SlpCal - A32:M38
  expect_silent(seg_cal <- le5_avg(allpnts, le))

  expect_snapshot_value(seg_cal, style = "json2")

})

test_that("ix", {
  allpnts <- all_points("testing") |> suppressMessages()
  # Bdr-Cal
  expect_silent(ix <- ix_avgs(allpnts, t$edge_row, t$edge_col, max(t$facet$row), max(t$facet$col)))
  expect_snapshot_value(ix, style = "json2")
})
