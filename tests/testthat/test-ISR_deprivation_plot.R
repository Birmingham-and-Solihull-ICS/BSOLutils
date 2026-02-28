## Unit tests for `ISR_deprivation_plot()`

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

test_that("ISR_deprivation_plot returns ggplot object with expected layers", {
  data(ISR_example)
  out <- ISR_deprivation(ISR_example)
  p <- ISR_deprivation_plot(out)
  expect_s3_class(p, "ggplot")
  geom_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomCol" %in% geom_classes)
})


test_that("ISR_deprivation_plot respects custom y_scale", {
  data(ISR_example)
  out <- ISR_deprivation(ISR_example)
  p <- ISR_deprivation_plot(out, y_scale = c(0,2,4))
  scales <- p$scales$scales
  y_scales <- scales[sapply(scales, function(s) "y" %in% s$aesthetics)]
  expect_equal(y_scales[[1]]$breaks, c(0,2,4))
})
