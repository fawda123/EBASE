test_that("Check output for fit_plot", {
  result <- fit_plot(res)
  expect_s3_class(result, 'ggplot')
})
