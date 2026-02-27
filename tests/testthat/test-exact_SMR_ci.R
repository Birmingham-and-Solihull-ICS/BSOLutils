## Unit tests for `exact_SMR_ci()`


test_that("`exact_SMR_ci()` basic behaviour", {
  res <- exact_SMR_ci(50, 100)
  expect_named(res, c("Rate", "LowerCI", "UpperCI"))
  expect_equal(res$Rate, 0.5)
  expect_equal(res$LowerCI, 0.371109637)
  expect_equal(res$UpperCI, 0.659187667)

  expect_true(res$LowerCI < res$Rate && res$Rate < res$UpperCI)
})


test_that("`exact_SMR_ci()` zero count", {
  res <- exact_SMR_ci(0, 10)
  expect_equal(res$Rate, 0)
  expect_equal(res$LowerCI, 0)
  expect_equal(res$UpperCI, 0.368887945)

})

