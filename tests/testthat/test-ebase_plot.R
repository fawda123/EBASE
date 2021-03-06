test_that("Checking ggplot class output, instantaneous", {
  result <- ebase_plot(res)
  expect_s3_class(result, 'ggplot')
})

test_that("Checking ggplot class output, daily", {
  result <- ebase_plot(res, instantaneous = FALSE)
  expect_s3_class(result, 'ggplot')
})
