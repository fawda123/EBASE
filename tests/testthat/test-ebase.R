test_that("Checking ebase names", {
  
  result <- res %>% 
    names
  
  expect_equal(result, c("Date", "DO_obs", "DO_mod", "DateTimeStamp", "dDO", "a", "b", 
                         "Pg_vol", "Rt_vol", "D"))
  
})
