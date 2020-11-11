test_that("read_feather_or_rds finds feather when both are present", {
  # The contents of both.rds does not match the expected tibble.
  x <- read_feather_or_rds("both")
  expect_equal(x, tibble::tibble(x = 1:5))
})

test_that("read_feather_or_rds finds rds when no feather is present", {
  # There must be no file only-rds.feather for this test.
  stopifnot(!file.exists("only-rds.feather"))
  x <- read_feather_or_rds("only-rds")
  expect_equal(x, tibble::tibble(x = 2:6))
})
