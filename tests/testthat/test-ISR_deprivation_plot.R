## Unit tests for `ISR_deprivation_plot()`


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
