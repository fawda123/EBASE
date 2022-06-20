test_that("Checking ebase names", {
  
  result <- res %>% 
    names
  
  expect_equal(result, c("Date", "grp", "DO_obs", "DO_mod", "DateTimeStamp", "dDO", "converge", "rsq", "a", "b", 
                         "Pg_vol", "Rt_vol", "D"))
  
})