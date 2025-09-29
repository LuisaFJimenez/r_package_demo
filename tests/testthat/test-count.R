test_that("count() counts non-missing elements by default", {
  expect_equal(count(c(1, 2, NA, 4)), 3L)
  expect_equal(count(c(NA, NA)), 0L)
  expect_equal(count(character()), 0L)
})

test_that("count() counts occurrences of a value", {
  expect_equal(count(c("a", "b", "a"), value = "a"), 2L)
  expect_equal(count(c(TRUE, FALSE, TRUE), value = TRUE), 2L)
  expect_equal(count(c(1, 2, NA, 1), value = 1), 2L)
})

test_that("count() validates inputs", {
  expect_error(count(list(1, 2)), "atomic")
  expect_error(count(1:3, value = c(1, 2)), "length 1")
})

