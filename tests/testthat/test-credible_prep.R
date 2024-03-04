test_that("Check output for credible_lrep", {
  result <- credible_prep(exres)
  expect_s3_class(result, 'data.frame')
})

test_that("Check error for wrong params for credible_prep", {
  expect_error(credible_prep(exres, params = 'abc'))
})