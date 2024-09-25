test_that("Checking ggplot class output, instantaneous", {
  result <- ebase_plot(exres)
  expect_s3_class(result, 'ggplot')
})

test_that("Checking ggplot class output, daily", {
  result <- ebase_plot(exres, instantaneous = FALSE)
  expect_s3_class(result, 'ggplot')
  ylb <- as.character(result$labels$y)
  expect_true(grepl("Daily-averaged", ylb))
})

test_that("Checking ggplot class output, asnem true", {
  result <- ebase_plot(exres, asnem = T)
  expect_s3_class(result, 'ggplot')
  expect_true("NEM" %in% result$data$name)
})
