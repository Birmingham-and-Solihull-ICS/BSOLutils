## Unit tests for `phi_func()`


test_that("`phi_func()` computes correct overdispersion", {
  expect_equal(phi_func(3, c(1,2,3)), (1/3)*(1^2 + 2^2 + 3^2))
})


test_that("`phi_func()` handles empty zscores", {
  # currently returns NaN when n = 0
  expect_true(is.nan(phi_func(0, numeric(0))))
})
