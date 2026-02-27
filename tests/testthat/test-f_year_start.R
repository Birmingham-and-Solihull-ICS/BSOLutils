## Unit tests for `f_year_start()`


test_that("`f_year_start()` returns correct start dates", {
  expect_equal(f_year_start("2025-05-10"), as.Date("2025-04-01"))
  expect_equal(f_year_start("2025-01-01"), as.Date("2024-04-01"))
  # vector behaviour tested via sapply to avoid internal length>1 error
  dates <- as.Date(c("2025-04-01","2025-03-31"))
  expect_equal(sapply(dates, f_year_start), as.Date(c("2025-04-01","2024-04-01")))
})
