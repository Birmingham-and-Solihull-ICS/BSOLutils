## Unit tests for `f_year()`


test_that("`f_year()` returns full and short year strings", {
  expect_equal(f_year("2025-05-10"), "2025/26")
  expect_equal(f_year("2025-01-01"), "2024/25")
  expect_equal(f_year("2025-01-01", short = TRUE), "24/25")
  dates <- as.Date(c("2025-04-01","2025-03-31"))
  expect_equal(sapply(dates, f_year), c("2025/26","2024/25"))
})
