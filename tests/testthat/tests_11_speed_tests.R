
test_that("trace_flow()", {
  skip("never run")
  db <- get_previous(system.file("extdata", "testELEV", package = "LITAP"),
                     where = "flow", step = "fill") %>%
    dplyr::select(seqno, row, col, elev, drec, upslope) %>%
    add_buffer()

  microbenchmark::microbenchmark(t1 <- trace_flow(202, db),
                                 t2 <- trace_flow2(202, db$drec),
                                 t3 <- trace_flow_fast(202, db$drec),
                                 times = 100L)

  expect_true(all(t1 == t2 & t1 == t3))

  microbenchmark::microbenchmark(trace_flow2(202, db$drec),
                                 trace_flow_fast(202, db$drec),
                                 times = 100L)

  # trace_flow_fast is best option


})
