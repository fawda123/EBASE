test_that("ebase_schmidt calculates Schmidt number correctly", {
  
  temp <- c(10, 20, 30)
  salt <- c(30, 35, 40)
  
  # Expected results based on manual calculations or external validation
  expected_result <- c(971.9186, 568.2032, 354.7613)
  
  # Run the function and check if the results match the expected values
  result <- ebase_schmidt(temp = temp, salt = salt)
  expect_equal(result, expected_result, tolerance = 0.001)
  
})
