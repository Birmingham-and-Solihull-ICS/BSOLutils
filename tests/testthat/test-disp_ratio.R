## Unit tests for `disp_ratio()`


test_that("`disp_ratio()` returns numeric >=0", {
  set.seed(123)
  df <- data.frame(y = rbinom(100,1,0.5), x = rnorm(100))
  mod <- glm(y ~ x, family = "binomial", data = df)
  res <- disp_ratio(mod)
  expect_type(res, "double")
  expect_true(res >= 0)
  expect_equal(res, 1.02080730286622)
})


test_that("`disp_ratio()` is 0 for perfect fit", {
  df <- data.frame(y = rep(1,10), x = rep(0,10))
  mod <- glm(y ~ x, family = "binomial", data = df)
  expect_equal(disp_ratio(mod), 0)
})
