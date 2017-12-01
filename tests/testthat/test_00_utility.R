context("Utility Functions")

test_that("get_dir returns correct direction", {
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1), 1)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1), 2)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2), 3)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1), 4)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2), 5)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2), 6)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1), 7)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1), 8)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2), 9)
}

test_that("get_dir returns correct direction given ldir_opts", {

  #1
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(2, 3)), 2)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(4, 7)), 4)
  expect_equal(get_dir(row = 1, col = 2, row_f = 2, col_f = 1, c(8, 6)), 8)

  #2
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(3, 6)), 3)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(1, 4)), 1)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 1, c(7, 8)), 7)

  #3
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(2, 1)), 2)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(6, 9)), 6)
  expect_equal(get_dir(row = 1, col = 1, row_f = 2, col_f = 2, c(4, 8)), 8)

  #4
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(7, 8)), 7)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(1, 2)), 1)
  expect_equal(get_dir(row = 1, col = 2, row_f = 1, col_f = 1, c(9, 3)), 9)

  #5
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, i), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(1, 2)), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(3, 4)), 5)
  expect_equal(get_dir(row = 2, col = 2, row_f = 2, col_f = 2, c(8, 9)), 5)

  #6
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, i), i)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(8, 9)), 9)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(2, 3)), 3)
  expect_equal(get_dir(row = 1, col = 1, row_f = 1, col_f = 2, c(7, 1)), 7)

  #7
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(4, 1)), 4)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(8, 9)), 8)
  expect_equal(get_dir(row = 2, col = 2, row_f = 1, col_f = 1, c(2, 6)), 6)

  #8
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, i), i)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(4, 7)), 7)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(9, 6)), 9)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 1, c(1, 3)), 1)

  #9
  for(i in (1:9)[-5]) expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, i), i)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(7, 8)), 8)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(3, 6)), 6)
  expect_equal(get_dir(row = 2, col = 1, row_f = 1, col_f = 2, c(4, 2)), 4)
})
