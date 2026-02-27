## Unit tests for `tau_func()`


test_that("`tau_func()` returns 0 when S is empty", {
  expect_equal(tau_func(3, 1, numeric(0)), 0)
})


test_that("`tau_func()` computes formula correctly", {
  n <- 3; phi <- 2; S <- c(1,2,3)
  expected <- max(0, ((sum(n) * sum(phi)) - (sum(n) - 1)) /
                     (sum(1/(S^2)) - (sum((1/(S^2))^2) / sum(1/(S^2)))))
  expect_equal(tau_func(n, phi, S), expected)
})
