test_that("dtriang handles basic cases correctly", {
  expect_equal(dtriang(5, 0, 10, 5), 0.2)
  expect_equal(dtriang(0, 0, 10, 5), 0)
  expect_equal(dtriang(10, 0, 10, 5), 0)
})

test_that("dtriang handles values outside support", {
  expect_equal(dtriang(-1, 0, 10, 5), 0)
  expect_equal(dtriang(11, 0, 10, 5), 0)
})

test_that("dtriang handles vector recycling", {
  res <- dtriang(x = c(2, 8), min = 0, max = 10, mode = 5)
  expect_length(res, 2)
  expect_equal(res[1], 0.08)
})

test_that("dtriang throws errors for invalid parameters", {
  expect_error(dtriang(5, 10, 0, 5))     # min > max
  expect_error(dtriang(5, 0, 10, 15))    # mode > max
})

test_that("dtriang handles parameter errors", {
  expect_error(dtriang(x = 5, min = 10, max = 20, mode = 5),
               "The mode has to be bigger than the minimum")
})
