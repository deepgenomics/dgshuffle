test_that("shuffling indices works", {
  expect_equal(shuffle_indices(2), c(1, 2))
  expect_equal(shuffle_indices(4), c(1, 3, 2, 4))
})

test_that("shuffling indices requires positive even number", {
  expect_error(shuffle_indices(3))
  expect_error(shuffle_indices(0))
  expect_error(shuffle_indices(-2))
})

test_that("shuffling columns works", {
  df <- tibble::tribble(~a, ~b, 1, 2)
  expect_equal(select_repeat_cols(df, 1),
               tibble::tribble(~x1, ~x2, ~x3, 1, 1, 1))
  expect_equal(select_repeat_cols(df, 2),
               tibble::tribble(~x1, ~x2, ~x3, 2, 2, 2))
})

test_that("shuffling columns requires an even number of columns", {
  expect_error(select_repeat_cols(tibble::tribble(~a, ~b, ~c, 1, 2, 3)))
})

test_that("shuffling columns requires an even or odd offset", {
  expect_error(select_repeat_cols(tibble::tribble(~a, ~b, 1, 2), 0))
  expect_error(select_repeat_cols(tibble::tribble(~a, ~b, 1, 2), 3))
})

test_that("shuffling columns requires a valid repeat", {
  expect_error(select_repeat_cols(tibble:tribble(~a, ~b, 1, 2), 1, -1))
})

test_that("shuffling works", {
  df <- tibble::tribble(~a, ~b, ~c, ~d, 1, 2, 3, 4, 5, 6, 7, 8)
  expected <- tibble::tribble(~x1, ~x2, ~x3, ~x4, ~x5, ~x6,
                              1, 1, 1, 3, 3, 3,
                              2, 2, 2, 4, 4, 4,
                              5, 5, 5, 7, 7, 7,
                              6, 6, 6, 8, 8, 8)
  actual <- shuffle_data(df)
  expect_equal(expected, actual)
})
