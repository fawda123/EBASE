test_that("Checking ebase_prep names", {
  result <- ebase_prep(exdat, H = 1.85) %>% 
    names
  
  expect_equal(result, c("Date", "DateTimeStamp", "DO_obs", "DO_sat", "H", "Temp", "Sal", 
                         "PAR", "WSpd", "sc", "grp"))
})

test_that("Checking ebase_prep incorrect input", {
  
  expect_error(ebase_prep(exdat[,-5], H = 1.85))
               
})

test_that("Checking ebase_prep all groups", {
  
  result <- ebase_prep(exdat, H = 1.85, ndays = 2) %>% 
    pull(grp) %>% 
    unique %>% 
    length
  
  expect_equal(result, 1)
  
})
