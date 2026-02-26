## Unit tests for `byars_ci()`

test_that("`byars_ci()` basic behaviour", {
  res <- byars_ci(50, 100)
  expect_named(res, c("Rate", "LowerCI", "UpperCI"))
  expect_equal(res$Rate, 0.5)
  expect_equal(res$LowerCI, 0.371081)
  expect_equal(res$UpperCI, 0.659201567)
  expect_true(res$LowerCI < res$Rate && res$Rate < res$UpperCI)
})

test_that("`byars_ci()` zero count returns zero rate", {
  expect_equal(byars_ci(0, 10)$Rate, 0)
})

test_that("`byars_ci()` known output example", {
  expect_equal(byars_ci(10, 20)$Rate, 0.5)
  expect_equal(byars_ci(10, 20)$LowerCI, 0.239372496)
  expect_equal(byars_ci(10, 20)$Upper,0.919572930)
})
