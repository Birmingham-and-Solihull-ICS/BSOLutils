## Unit tests for `thin_binomial()`


test_that("`thin_binomial()` basic behaviour", {
  set.seed(2024)
  x_counts <- c(0, 3, 10, 5, 2)
  p <- 0.4
  res <- thin_binomial(x_counts, p)
  expect_length(res, length(x_counts))
  expect_true(all(res >= 0 & res <= x_counts))
})


test_that("`thin_binomial()` input validation", {
  expect_error(thin_binomial(-1, 0.5))
  expect_error(thin_binomial(3, -0.1))
  expect_error(thin_binomial(3, 1.1))
})


test_that("`thin_binomial()` deterministic with seed", {
  set.seed(1)
  expected <- rbinom(n=3, size=c(1,2,3), prob=0.5)
  set.seed(1)
  actual <- thin_binomial(c(1,2,3), 0.5)
  expect_equal(actual, expected)
})
