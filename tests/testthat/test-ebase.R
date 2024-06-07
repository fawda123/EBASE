test_that("Checking ebase output, doave as F", {
  
  # check format
  result <- EBASE::ebase(dat, interval = 900, Z = 1.85, n.chains = 2, doave = F, 
                         n.iter = 50,
                      model_file = system.file("ebase_model.txt", package = "EBASE")) 
  
  expect_s3_class(result, 'data.frame')
  
  # check names
  nms <- names(result)
  expect_equal(nms, c("DateTimeStamp", "Date", "grp", "Z", "DO_obs", "DO_mod", "DO_modlo",
                         "DO_modhi", "dDO", "converge", "rsq", "a", "alo", "ahi", "b", "blo", 
                         "bhi", "P", "Plo", "Phi", "R", "Rlo", "Rhi", "D", "Dlo", "Dhi"))
  
})

test_that("Checking ebase output, no gas exchange", {
  
  result <- EBASE::ebase(dat, interval = 900, Z = 1.85, n.chains = 2, doave = F, 
                         n.iter = 50, nogas = T,
                         model_file = system.file("ebase_model.txt", package = "EBASE")) 
  
  aved <- round(mean(result$D, na.rm = T))
  
  # check gas exchange is zero
  expect_equal(aved, 0)
  
})