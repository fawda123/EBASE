test_that("Check output for credible_plot", {
  result <- credible_plot(res)
  expect_s3_class(result, 'ggplot')
})

test_that("Check error for wrong params for credible_plot", {
  expect_error(credible_plot(res, params = 'abc'))
})