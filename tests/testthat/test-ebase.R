test_that("Checking ebase names", {
  
  result <- res %>% 
    names
  
  expect_equal(result, c("DateTimeStamp", "Date", "grp", "DO_obs", "DO_mod", "DO_modlo", "DO_modhi", 
                         "dDO", "converge", "rsq", "a", "alo", "ahi", "b", "blo", "bhi", "Pg_vol", 
                         "Pg_vollo", "Pg_volhi", "Rt_vol", "Rt_vollo", "Rt_volhi", "D", "Dlo", "Dhi"))
  
})