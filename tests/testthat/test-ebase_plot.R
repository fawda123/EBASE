test_that("Checking ggplot class output, instantaneous", {
  result <- ebase_plot(exres)
  expect_s3_class(result, 'ggplot')
})

test_that("Checking ggplot class output, daily", {
  result <- ebase_plot(exres, instantaneous = FALSE)
  expect_s3_class(result, 'ggplot')
})
