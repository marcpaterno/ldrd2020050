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
