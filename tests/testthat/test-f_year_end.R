## Unit tests for `f_year_end()`


test_that("`f_year_end()` returns correct end dates", {
  expect_equal(f_year_end("2025-05-10"), as.Date("2026-03-31"))
  expect_equal(f_year_end("2025-01-01"), as.Date("2025-03-31"))
  dates <- as.Date(c("2025-04-01","2025-03-31"))
  expect_equal(sapply(dates, f_year_end), as.Date(c("2026-03-31","2025-03-31")))
})
