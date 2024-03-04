test_that("Check output for credible_plot", {
  result <- credible_plot(exres)
  expect_s3_class(result, 'ggplot')
})
