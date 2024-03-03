test_that("ebase_eqboxy calculates oxygen saturation correctly", {
  temp <- c(10, 20, 30)
  salt <- c(30, 35, 40)
  
  # Expected results based on manual calculations or external validation
  expected_result <- c(291.2038, 230.8762, 189.3412)
  
  # Run the function and check if the results match the expected values
  result <- ebase_eqboxy(temp = temp, salt = salt)
  expect_equal(result, expected_result, tolerance = 0.001)
  
})
