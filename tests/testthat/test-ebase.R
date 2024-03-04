test_that("Checking ebase names", {
  
  result <- exres %>% 
    names
  
  expect_equal(result, c("DateTimeStamp", "Date", "grp", "Z", "DO_obs", "DO_mod", "DO_modlo",
                         "DO_modhi", "dDO", "converge", "rsq", "a", "alo", "ahi", "b", "blo", 
                         "bhi", "P", "Plo", "Phi", "R", "Rlo", "Rhi", "D", "Dlo", "Dhi"))
  
})

test_that("Checking ebase output, doave as F", {
  
  result <- EBASE::ebase(dat, interval = 900, Z = 1.85, progress = FALSE, n.chains = 2, doave = F, 
                         n.iter = 100,
                      model_file = system.file("ebase_model.txt", package = "EBASE")) 
  
  expect_s3_class(result, 'data.frame')
  
})
