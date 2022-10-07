test_that("Check output for prior_plot", {
  result <- prior_plot()
  expect_s3_class(result, 'ggplot')
})
