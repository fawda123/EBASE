test_that("ebase_rho calculates seawater density correctly at atmospheric pressure", {
  temp <- c(10, 20, 30)
  salt <- c(30, 35, 40)
  P <- 0
  
  # Expected results based on manual calculations or external validation
  expected_result <- c(1023.0511, 1024.7630, 1025.4829)
  
  # Run the function and check if the results match the expected values
  result <- ebase_rho(temp = temp, salt = salt, P = P)
  expect_equal(result, expected_result, tolerance = 0.001)
  
})
