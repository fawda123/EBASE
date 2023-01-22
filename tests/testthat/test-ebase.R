test_that("Checking ebase names", {
  
  result <- res %>% 
    names
  
  expect_equal(result, c("DateTimeStamp", "Date", "grp", "H", "DO_obs", "DO_mod", "DO_modlo",
                         "DO_modhi", "dDO", "converge", "rsq", "a", "alo", "ahi", "b", "blo", 
                         "bhi", "P", "Plo", "Phi", "R", "Rlo", "Rhi", "D", "Dlo", "Dhi"))
  
})