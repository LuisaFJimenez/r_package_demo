test_that("multiply() multiplies correctly", {
  expect_equal(multiply(2, 3), 6)
  expect_equal(multiply(1:3, 10), c(10, 20, 30))
  expect_equal(multiply(c(0.5, 2), 4), c(2, 8))
})

test_that("multiply() validates inputs", {
  expect_error(multiply("a", 2), "numeric")
  expect_error(multiply(1, "b"), "numeric")
})

