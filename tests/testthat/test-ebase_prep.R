test_that("Checking ebase_prep names", {
  result <- ebase_prep(exdat, H = 1.85) %>% 
    names
  
  expect_equal(result, c("Date", "DateTimeStamp", "DO_obs", "DO_sat", "H", "Temp", "Sal", 
                         "PAR", "WSpd", "sc"))
})

test_that("Checking ebase_prep incorrect input", {
  
  expect_error(ebase_prep(exdat[,-5], H = 1.85))
               
})
