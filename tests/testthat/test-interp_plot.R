test_that("Checking output type for interp_plot", {
  p <- interp_plot(dat, H = 1.85, interval = 900, param = 'DO_sat')
  expect_s3_class(p, 'ggplot')
})
