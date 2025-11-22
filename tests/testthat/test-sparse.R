test_that("coercion numeric -> sparse -> numeric works", {
  x <- c(0, 3, 0, 5)
  s <- as(x, "sparse_numeric")
  expect_s4_class(s, "sparse_numeric")
  expect_equal(as(s, "numeric"), x)
})

test_that("coercion handles all-zero vector", {
  x <- c(0, 0, 0, 0)
  s <- as(x, "sparse_numeric")
  expect_equal(length(s@pos), 0)
  expect_equal(as(s, "numeric"), x)
})

test_that("mean works for typical and edge cases", {
  x <- as(c(0, 3, 0, 5), "sparse_numeric")
  expect_equal(mean(x), mean(c(0, 3, 0, 5)))

  x0 <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(mean(x0), 0)

  x1 <- as(c(5), "sparse_numeric")
  expect_equal(mean(x1), 5)
})

test_that("norm computes Euclidean norm correctly", {
  x <- as(c(0, 3, 4), "sparse_numeric")
  expect_equal(norm(x), 5)

  x0 <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(norm(x0), 0)
})

test_that("standardize matches dense scale()", {
  x <- as(c(0, 3, 0, 1), "sparse_numeric")
  dense <- c(0, 3, 0, 1)
  s <- standardize(x)
  expect_equal(as.numeric(s), as.numeric(scale(dense)), tolerance = 1e-6)
})

test_that("standardize handles constant vector", {
  x <- as(c(5, 5, 5, 5), "sparse_numeric")
  s <- standardize(x)
  expect_equal(as.numeric(s), rep(0, 4))
})

test_that("arithmetic: + - * works", {
  x <- as(c(0,3,0,5), "sparse_numeric")
  y <- as(c(1,0,2,0), "sparse_numeric")

  expect_equal(as(x + y, "numeric"), c(1,3,2,5))
  expect_equal(as(x - y, "numeric"), c(-1,3,-2,5))
  expect_equal(as(x * y, "numeric"), c(0,0,0,0))  # no overlapping non-zeros
})

test_that("crossproduct works correctly", {
  x <- as(c(0,3,4), "sparse_numeric")
  y <- as(c(1,3,0), "sparse_numeric")

  expect_equal(sparse_crossprod(x, y), 3*3 + 4*0)
})

test_that("length mismatch throws error", {
  x <- as(c(1,2,3), "sparse_numeric")
  y <- as(c(1,2), "sparse_numeric")

  expect_error(x + y)
  expect_error(sparse_crossprod(x, y))
})

test_that("show prints correct structure", {
  x <- as(c(0,3,0,5), "sparse_numeric")
  expect_output(show(x), "sparse_numeric vector of length 4")
  expect_output(show(x), "pos")
  expect_output(show(x), "value")
})

test_that("empty sparse vector prints (all zeros)", {
  x <- as(c(0,0,0), "sparse_numeric")
  expect_output(show(x), "(all zeros)")
})
