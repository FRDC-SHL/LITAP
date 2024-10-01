test_that("all_XXX tables()", {
  skip_on_ci()

  expect_silent(meta <<- tbl_meta(test_facet(),  test_log())$meta)
  topo <- test_topo()
  stats <- test_stats()

  # All points ---------
  expect_message(pnts <<- all_points("testing"))
  expect_silent(pnts_no_edge <<- omit_edges(pnts, meta = meta))
  expect_message(allpeak <<- all_peak("testing")) %>% suppressMessages()
  expect_message(allpit <<- all_pit("testing")) %>% suppressMessages()
  expect_message(allcrest <<- all_crest("testing")) %>% suppressMessages()
  expect_message(allstream <<- all_stream("testing")) %>% suppressMessages()

  expect_snapshot(pnts[1:1000,] %>% as.data.frame())
  expect_snapshot(pnts_no_edge[1:1000,] %>% as.data.frame())
  expect_snapshot(allpeak %>% as.data.frame())
  expect_snapshot(allpit %>% as.data.frame())
  expect_snapshot(allcrest %>% as.data.frame())
  expect_snapshot(allstream %>% as.data.frame())
})

test_that("Sub calculations", {
  skip_on_ci()

  topo <- test_topo()
  stats <- test_stats()

  # Sub calculations ---------------
  expect_silent(seg_cal <- le5_avg(pnts_no_edge))
  expect_silent(lsf <- ls_factor(pnts_no_edge))
  expect_silent(ix <- ix_avgs(pnts, pnts_no_edge))
  expect_silent(cnts <- pnts_count(allpeak, allpit, allcrest, allstream, pnts_no_edge))
  expect_silent(avg <- avg_topo(topo, lsf))
  expect_silent(slp_cal <- mid_calc(pnts, pnts_no_edge, cnts, avg, seg_cal))
  expect_silent(density <- ws_density(pnts, allpit, meta))
  expect_silent(edge_drainage <- ws_drainage(pnts, stats, allpit, meta))

  expect_snapshot(seg_cal %>% as.data.frame())
  expect_snapshot(lsf %>% as.data.frame())
  expect_snapshot(ix %>% as.data.frame())
  expect_snapshot(cnts %>% as.data.frame())
  expect_snapshot(avg %>% as.data.frame())
  expect_snapshot(slp_cal %>% as.data.frame())
  expect_snapshot(density %>% as.data.frame())
  expect_snapshot(edge_drainage %>% as.data.frame())
})

test_that("summary_table()", {
  skip_on_ci()

  unlink(test_path("testing"), recursive = TRUE)
  dir.create(test_path("testing"))
  expect_false(file.exists(test_path("testing", "test_topo_summary.xlsx")))
  expect_message(summary_tables("testing")) %>% suppressMessages()
  expect_true(file.exists(test_path("testing", "testing_topo_summary.xlsx")))
  expect_true(file.exists(test_path("testing", "testing_all_crests.xlsx")))
  expect_true(file.exists(test_path("testing", "testing_all_peaks.xlsx")))
  expect_true(file.exists(test_path("testing", "testing_all_pits.xlsx")))
  expect_true(file.exists(test_path("testing", "testing_all_streams.xlsx")))
  unlink(test_path("testing"), recursive = TRUE)
})
