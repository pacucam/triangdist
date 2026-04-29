test_that("qtriang calculates basic quantiles correctly", {
  expect_equal(qtriang(0, 0, 10, 5), 0)
  expect_equal(qtriang(1, 0, 10, 5), 10)
  expect_equal(qtriang(0.5, 0, 10, 5), 5)
})

test_that("qtriang and ptriang are consistent (Inverse Identity)", {
  a <- 0; b <- 10; c <- 2
  x_values <- c(1, 2, 5, 8)

  probs <- ptriang(x_values, a, b, c)

  expect_equal(qtriang(probs, a, b, c), x_values)
})

test_that("qtriang handles vectorization and recycling", {
  p_vec <- c(0.1, 0.9)
  res <- qtriang(p_vec, 0, 10, 5)
  expect_length(res, 2)
  expect_equal(res[1] - 0, 10 - res[2])
})

test_that("qtriang catches parameter errors for coverage", {
  expect_error(qtriang(-0.1, 0, 10, 5))
  expect_error(qtriang(1.1, 0, 10, 5))
  #Out of range
  expect_error(qtriang(0.5, 10, 0, 5))
  expect_error(qtriang(0.5, 0, 10, 11))
  expect_error(qtriang(0.5, 0, 2, -1))
})
