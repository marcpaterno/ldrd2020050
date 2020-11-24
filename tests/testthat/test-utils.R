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

test_that("n_dimensions handles correct dataframe", {
  d <- tibble::tibble(x = 1:2, dim0 = 1:2, dim0l = 1:2)
  expect_equal(n_dimensions(d), 1L)
})

test_that("n_dimensions handles incorrect dataframe", {
  d <- tibble::tibble(x = 1:2, dim0 = 1:2)
  expect_error(n_dimensions(d), "n_dim_cols%%2 == 0 is not TRUE")
})

test_that("augmenting raw dataframe works", {
 d <- readRDS("small_dims.rds")
 expect_s3_class(d, "data.frame")
 d_with_lengths <- augment_raw_dataframe(d)
 expect_s3_class(d_with_lengths, "tbl_df")
 expect_length(d_with_lengths, length(d) + 3) # add two lengths + volume
 expect_equal(nrow(d_with_lengths), nrow(d))
 expect_equal(d_with_lengths$len_0, c(10, 2, 8))
 expect_equal(d_with_lengths$len_1, c(45, 40, 5))
 expect_equal(d_with_lengths$vol, c(450, 80, 40))
 expect_equal(d_with_lengths$active, c(TRUE, TRUE, FALSE))
})

test_that("canonicalize_dim_names works", {
  old <- readRDS("old_small_dims.rds")
  new <- canonicalize_dim_names(old)
  expect_equal(names(new)[3:6],
               c("dim_0_lo", "dim_0_hi", "dim_1_lo", "dim_1_hi"))
})

test_that("canonicalize_dim_names works when columns are out of order", {
  d <- readRDS("example_regions.rds") %>% canonicalize_dim_names()
  expect_equal(names(d),
               c("iteration", "id", "parentID", "estimate", "errorest",
                 "dim_0_lo", "dim_0_hi", "dim_1_lo", "dim_1_hi", "active"))
})

test_that("make_iteration_dataframe works", {
  aug <- readRDS("two_iters.rds")
  by_iter <- make_iteration_dataframe(aug)
  expect_equal(nrow(by_iter), 2L)
  expect_equal(by_iter$act.est, c(10., 3.))
  expect_equal(by_iter$act.err, c(2, 0.25))
  expect_equal(by_iter$act.nreg, c(1,1))
  expect_equal(by_iter$act.vol, c(450., 80.))
  expect_equal(by_iter$fin.vol, c(0., 40.))
  # TODO: complete the testing of other columns
})
