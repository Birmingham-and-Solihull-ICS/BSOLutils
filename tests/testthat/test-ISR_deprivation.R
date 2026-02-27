## Unit tests for `ISR_deprivation()`

example_dt <-
  data.frame(
    age_related_seed = rep(seq(0.8, 1.2, 0.07), 32),
    age_group_code = rep(seq(1,6), 32),
    sex_group_code = rep(rep(c(1,2), each = 6), 16),
    imd_code = rep(c(1,2,3,4,5,999), 32)
  )


set.seed(123)

example_dt$numerator <- rpois(192,example_dt$age_related_seed * 30)
example_dt$denominator <- rpois(192,example_dt$age_related_seed * 85)

ISR_example <- example_dt[, names(example_dt) != "age_related_seed", drop = FALSE]

test_that("ISR_deprivation returns expected structure and positive ratios", {
  #data(ISR_example)
  out <- ISR_deprivation(ISR_example)
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("imd_quintile","ratio","lowerCI","upperCI"))
  expect_true(all(out$ratio > 0))
  expect_equal(out[1,]$ratio, 1.034290875)
})


test_that("ISR_deprivation can toggle age/sex predictors", {
  #data(ISR_example)
  out1 <- ISR_deprivation(ISR_example, age = FALSE, sex = TRUE)
  out2 <- ISR_deprivation(ISR_example, age = TRUE, sex = TRUE)
  expect_true(nrow(out1) == 5)
  expect_true(nrow(out2) == 5)
})


test_that("ISR_deprivation errors when required columns are missing", {
  expect_error(ISR_deprivation(data.frame()), "numerator")
})
