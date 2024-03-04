test_that("Check output for fit_plot", {
  result <- fit_plot(exres)
  expect_s3_class(result, 'ggplot')
})

test_that("Check output for fit_plot, scatter = TRUE", {
  result <- fit_plot(exres, scatter = TRUE)
  expect_s3_class(result, 'ggplot')
})

test_that("Check output for fit_plot, bygroup = TRUE", {
  result <- fit_plot(exres, bygroup = TRUE)
  expect_s3_class(result, 'ggplot')
})
