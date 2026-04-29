test_that("rtriang outputs have correct structure", {
  n <- 50
  res <- rtriang(n, 0, 10, 5)

  # Integridad de datos
  expect_type(res, "double")
  expect_length(res, n)
  expect_false(any(is.na(res)))
})

test_that("rtriang respects physical boundaries", {
  a <- 10; b <- 20; c <- 15
  samples <- rtriang(1000, a, b, c)
  expect_true(all(samples >= a))
  expect_true(all(samples <= b))
})

test_that("rtriang is statistically consistent", {
  a <- 0; b <- 60; c <- 30
  theoretical_mean <- (a + b + c) / 3 # Resultado: 30
  large_sample <- rtriang(10000, a, b, c)
  expect_equal(mean(large_sample), theoretical_mean, tolerance = 0.5)
})

test_that("rtriang handles edge cases and errors", {
  expect_length(rtriang(0, 0, 10, 5), 0)
  expect_error(rtriang(-5, 0, 10, 5))
  expect_error(rtriang(10, 10, 0, 5))
})

test_that("rtriang handles vector input for n (R convention)", {
  n_vector <- c(10, 20, 30)
  res <- rtriang(n_vector, 0, 10, 5)
  expect_length(res, length(n_vector)) # Se espera longitud 3
})
