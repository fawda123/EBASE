test_that("Checking ebase names", {
  
  result <- res %>% 
    names
  
  expect_equal(result, c("Date", "grp", "DO_obs", "DO_mod", "DateTimeStamp", "dDO", "converge", "rsq", "a", 
                         "alo", "ahi", "b", "blo", "bhi", "Pg_vol", "Rt_vol", "Rt_vollo", "Rt_volhi", "D"))
  
})