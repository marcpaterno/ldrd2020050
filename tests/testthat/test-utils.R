test_that("%nin% works on empty vectors", {
  a <- c()
  b <- c()
  expect_equal(a %nin% b, logical(0))
})

test_that("%nin% works when second argument is empty", {
  a <- 1
  b <- c()
  expect_equal(a %nin% b, c(TRUE))
})

test_that("%nin% works when first argument is empty", {
  a <- c()
  b <- 1
  expect_equal(a %nin% b, logical(0))
})

test_that("%nin%  works", {
  a <- 1:5
  b <- 2:4
  expect_equal(a %nin% b, c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("adding lengths works", {
  d <- readRDS("small_dims.rds")
  expect_s3_class(d, "tbl_df")
  d_with_lengths <- add_lengths(d)
  expect_s3_class(d_with_lengths, "tbl_df")
  expect_length(d_with_lengths, 2*(length(d) - 2))
  expect_equal(nrow(d_with_lengths), nrow(d))
})

test_that("adding vol works", {
  d <- add_lengths(readRDS("small_dims.rds"))
  expect_s3_class(d, "tbl_df")
  d_with_vol <- add_volume(d)
  expect_length(d_with_vol, length(d) + 1)
  expect_equal(nrow(d_with_vol), nrow(d))
})
